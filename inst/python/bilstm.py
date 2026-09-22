"""Generic BiLSTM calibration engine.

This file does not know what SIR, SEIR, or SIRH mean.  It receives:
  * an epidemic-curve matrix,
  * a matrix of known parameters,
  * a matrix of target parameters.

The R side decides which columns are known and which are targets.
"""

import copy
import json
from pathlib import Path

import joblib
import numpy as np
import pandas as pd
import torch
import torch.nn as nn
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
from torch.utils.data import DataLoader, TensorDataset


class BiLSTMModel(nn.Module):
    def __init__(self, known_dim, target_dim, hidden_size=160, num_layers=3, dropout=0.5):
        super().__init__()
        self.lstm = nn.LSTM(
            input_size=1,
            hidden_size=hidden_size,
            num_layers=num_layers,
            batch_first=True,
            dropout=dropout if num_layers > 1 else 0.0,
            bidirectional=True,
        )
        self.fc1 = nn.Linear(2 * hidden_size + known_dim, 64)
        self.fc2 = nn.Linear(64, target_dim)

    def forward(self, epicurve, known):
        _, (hidden, _) = self.lstm(epicurve)
        curve_features = torch.cat((hidden[-2], hidden[-1]), dim=1)
        features = torch.cat((curve_features, known), dim=1)
        features = torch.relu(self.fc1(features))
        # All targets are min-max scaled during training.
        return torch.sigmoid(self.fc2(features))


def _fixed_incidence_scaler(n_days, incidence_max=100000.0):
    scaler = MinMaxScaler(feature_range=(0, 1))
    scaler.data_min_ = np.zeros(n_days, dtype=np.float64)
    scaler.data_max_ = np.full(n_days, incidence_max, dtype=np.float64)
    scaler.data_range_ = scaler.data_max_ - scaler.data_min_
    scaler.scale_ = 1.0 / scaler.data_range_
    scaler.min_ = -scaler.data_min_ * scaler.scale_
    scaler.n_features_in_ = n_days
    scaler.n_samples_seen_ = 1
    return scaler


def _scale_incidence(x, scaler):
    x = np.asarray(x, dtype=np.float32)
    if x.ndim == 1:
        x = x[None, :]
    if x.shape[1] == scaler.n_features_in_:
        return scaler.transform(x).astype(np.float32)
    raise ValueError(
        f"Epicurve has {x.shape[1]} days but the saved model expects "
        f"{scaler.n_features_in_}."
    )


def _optional_physics_loss(pred_scaled, known_scaled, target_scaler, known_scaler,
                           target_names, known_names, weight, mse):
    if weight <= 0:
        return torch.tensor(0.0, dtype=pred_scaled.dtype, device=pred_scaled.device)

    required_targets = {"ptran", "crate", "R0"}
    if not required_targets.issubset(target_names) or "recov" not in known_names:
        return torch.tensor(0.0, dtype=pred_scaled.dtype, device=pred_scaled.device)

    target_min = torch.tensor(target_scaler.data_min_, dtype=pred_scaled.dtype, device=pred_scaled.device)
    target_range = torch.tensor(target_scaler.data_range_, dtype=pred_scaled.dtype, device=pred_scaled.device)
    known_min = torch.tensor(known_scaler.data_min_, dtype=pred_scaled.dtype, device=pred_scaled.device)
    known_range = torch.tensor(known_scaler.data_range_, dtype=pred_scaled.dtype, device=pred_scaled.device)

    pred = pred_scaled * target_range + target_min
    known = known_scaled * known_range + known_min

    p = target_names.index("ptran")
    c = target_names.index("crate")
    r = target_names.index("R0")
    g = known_names.index("recov")

    return weight * mse(pred[:, r] * known[:, g], pred[:, p] * pred[:, c])


def train_model(theta_csv, incidence_csv, output_dir, known_names, target_names,
                model_type="generic", epochs=100, batch_size=64,
                learning_rate=0.001, seed=122, hidden_size=160,
                num_layers=3, dropout=0.5, physics_weight=0.0,
                validation_fraction=0.2):
    torch.manual_seed(seed)
    np.random.seed(seed)

    theta = pd.read_csv(theta_csv)
    incidence = pd.read_csv(incidence_csv).values.astype(np.float32)

    missing = [x for x in known_names + target_names if x not in theta.columns]
    if missing:
        raise ValueError(f"Columns not found in theta CSV: {missing}")

    known_raw = theta[known_names].values.astype(np.float32)
    targets_raw = theta[target_names].values.astype(np.float32)

    idx = np.arange(len(theta))
    train_idx, val_idx = train_test_split(
        idx, test_size=validation_fraction, random_state=42, shuffle=True
    )

    incidence_scaler = _fixed_incidence_scaler(incidence.shape[1])
    known_scaler = MinMaxScaler().fit(known_raw[train_idx])
    target_scaler = MinMaxScaler().fit(targets_raw[train_idx])

    x_train = _scale_incidence(incidence[train_idx], incidence_scaler)
    x_val = _scale_incidence(incidence[val_idx], incidence_scaler)
    k_train = known_scaler.transform(known_raw[train_idx]).astype(np.float32)
    k_val = known_scaler.transform(known_raw[val_idx]).astype(np.float32)
    y_train = target_scaler.transform(targets_raw[train_idx]).astype(np.float32)
    y_val = target_scaler.transform(targets_raw[val_idx]).astype(np.float32)

    loader = DataLoader(
        TensorDataset(
            torch.tensor(x_train[:, :, None], dtype=torch.float32),
            torch.tensor(k_train, dtype=torch.float32),
            torch.tensor(y_train, dtype=torch.float32),
        ),
        batch_size=batch_size,
        shuffle=True,
    )

    x_val_t = torch.tensor(x_val[:, :, None], dtype=torch.float32)
    k_val_t = torch.tensor(k_val, dtype=torch.float32)
    y_val_t = torch.tensor(y_val, dtype=torch.float32)

    model = BiLSTMModel(
        known_dim=len(known_names),
        target_dim=len(target_names),
        hidden_size=hidden_size,
        num_layers=num_layers,
        dropout=dropout,
    )
    optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
    mse = nn.MSELoss()

    train_history = []
    validation_history = []
    best_loss = float("inf")
    best_epoch = 0
    best_state = copy.deepcopy(model.state_dict())

    for epoch in range(epochs):
        model.train()
        total = 0.0
        for xb, kb, yb in loader:
            optimizer.zero_grad()
            pred = model(xb, kb)
            loss = mse(pred, yb)
            loss = loss + _optional_physics_loss(
                pred, kb, target_scaler, known_scaler,
                target_names, known_names, physics_weight, mse
            )
            loss.backward()
            optimizer.step()
            total += loss.item() * len(xb)
        train_history.append(total / len(loader.dataset))

        model.eval()
        with torch.no_grad():
            pred = model(x_val_t, k_val_t)
            val_loss = mse(pred, y_val_t)
            val_loss = val_loss + _optional_physics_loss(
                pred, k_val_t, target_scaler, known_scaler,
                target_names, known_names, physics_weight, mse
            )
            val_loss = float(val_loss.item())

        validation_history.append(val_loss)
        if val_loss < best_loss:
            best_loss = val_loss
            best_epoch = epoch + 1
            best_state = copy.deepcopy(model.state_dict())

    model.load_state_dict(best_state)
    model.eval()

    with torch.no_grad():
        pred_scaled = model(x_val_t, k_val_t).numpy()
    pred = target_scaler.inverse_transform(pred_scaled)
    truth = targets_raw[val_idx]

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    scripted = torch.jit.script(model)
    config = {
        "model_type": model_type,
        "calibrator": "BiLSTM",
        "known_names": list(known_names),
        "target_names": list(target_names),
        "sequence_length": int(incidence.shape[1]),
        "hidden_size": int(hidden_size),
        "num_layers": int(num_layers),
        "dropout": float(dropout),
        "epochs": int(epochs),
        "best_epoch": int(best_epoch),
        "batch_size": int(batch_size),
        "learning_rate": float(learning_rate),
        "seed": int(seed),
        "physics_weight": float(physics_weight),
    }

    torch.jit.save(
        scripted,
        str(output_dir / "model.pt"),
        _extra_files={"metadata.json": json.dumps(config)},
    )
    torch.save(model.state_dict(), output_dir / "weights.pt")
    joblib.dump(incidence_scaler, output_dir / "scaler_incidence.pkl")
    joblib.dump(known_scaler, output_dir / "scaler_additional.pkl")
    joblib.dump(target_scaler, output_dir / "scaler_targets.pkl")
    with open(output_dir / "model_config.json", "w") as f:
        json.dump(config, f, indent=2)
        f.write("\n")

    metrics = []
    pred_table = {}
    for j, name in enumerate(target_names):
        metrics.append({
            "parameter": name,
            "MAE": float(mean_absolute_error(truth[:, j], pred[:, j])),
            "RMSE": float(np.sqrt(mean_squared_error(truth[:, j], pred[:, j]))),
            "R2": float(r2_score(truth[:, j], pred[:, j])),
        })
        pred_table[f"true_{name}"] = truth[:, j]
        pred_table[f"pred_{name}"] = pred[:, j]

    pd.DataFrame(metrics).to_csv(output_dir / "validation_metrics.csv", index=False)
    pd.DataFrame(pred_table).to_csv(output_dir / "validation_predictions.csv", index=False)
    pd.DataFrame({
        "epoch": np.arange(1, epochs + 1),
        "train_loss": train_history,
        "validation_loss": validation_history,
    }).to_csv(output_dir / "training_history.csv", index=False)

    print(f"Saved trained calibrator to: {output_dir}")
    print(f"Best epoch: {best_epoch}")
    print(pd.DataFrame(metrics).to_string(index=False))


def predict_saved_model(epicurve, known, model_dir):
    model_dir = Path(model_dir)
    metadata = {"metadata.json": ""}
    model = torch.jit.load(
        str(model_dir / "model.pt"),
        map_location="cpu",
        _extra_files=metadata,
    )
    config = json.loads(metadata["metadata.json"])

    incidence_scaler = joblib.load(model_dir / "scaler_incidence.pkl")
    known_scaler = joblib.load(model_dir / "scaler_additional.pkl")
    target_scaler = joblib.load(model_dir / "scaler_targets.pkl")

    x = _scale_incidence(epicurve, incidence_scaler)
    known = np.asarray(known, dtype=np.float32)
    if known.ndim == 1:
        known = known[None, :]
    known = known_scaler.transform(known).astype(np.float32)

    with torch.no_grad():
        pred_scaled = model(
            torch.tensor(x[:, :, None], dtype=torch.float32),
            torch.tensor(known, dtype=torch.float32),
        ).numpy()

    pred = target_scaler.inverse_transform(pred_scaled)[0]
    return dict(zip(config["target_names"], pred.tolist()))


