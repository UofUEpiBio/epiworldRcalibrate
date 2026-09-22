"""Command-line wrapper for the generic BiLSTM trainer.

The reusable model/training/prediction functions live in bilstm.py.
This file only parses command-line arguments, so reticulate can safely
source bilstm.py during prediction without triggering argparse.
"""

import argparse
from bilstm import train_model


def _parse_names(text):
    return [x.strip() for x in text.split(",") if x.strip()]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--theta", required=True)
    curve_group = parser.add_mutually_exclusive_group(required=True)
    curve_group.add_argument("--curves")
    curve_group.add_argument("--incidence")
    parser.add_argument("--output-dir", required=True)
    parser.add_argument("--known", required=True)
    parser.add_argument("--targets", required=True)
    parser.add_argument("--type", default="generic")
    parser.add_argument("--epochs", type=int, default=100)
    parser.add_argument("--batch-size", type=int, default=64)
    parser.add_argument("--learning-rate", type=float, default=0.001)
    parser.add_argument("--seed", type=int, default=122)
    parser.add_argument("--hidden-size", type=int, default=160)
    parser.add_argument("--num-layers", type=int, default=3)
    parser.add_argument("--dropout", type=float, default=0.5)
    parser.add_argument("--physics-weight", type=float, default=0.0)
    args = parser.parse_args()

    train_model(
        theta_csv=args.theta,
        incidence_csv=args.curves if args.curves is not None else args.incidence,
        output_dir=args.output_dir,
        known_names=_parse_names(args.known),
        target_names=_parse_names(args.targets),
        model_type=args.type,
        epochs=args.epochs,
        batch_size=args.batch_size,
        learning_rate=args.learning_rate,
        seed=args.seed,
        hidden_size=args.hidden_size,
        num_layers=args.num_layers,
        dropout=args.dropout,
        physics_weight=args.physics_weight,
    )


if __name__ == "__main__":
    main()
