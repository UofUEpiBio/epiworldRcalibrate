#' Plot Results of Model Predictions
#'
#' @description
#' Generates plots comparing predicted and observed values, including boxplots and scatter plots with MAE labels.
#'
#' @param pred `data.table`. The predicted values from the model.
#' @param test_data List. A list containing test data (`x` and `y`).
#' @param theta `data.table`. The parameters corresponding to the simulations.
#' @param MAEs Numeric vector. The Mean Absolute Errors for each output variable.
#' @param N Integer. Total number of simulations.
#' @param N_train Integer. Number of training examples.
#' @importFrom dplyr mutate row_number
#' @importFrom ggplot2 aes geom_boxplot geom_point facet_wrap labs geom_abline
#' @importFrom data.table dcast melt setnames as.data.table
#' @return Generates plots (displayed on the active graphics device).
#' @export
plot_results <- function(pred, test_data, theta, MAEs, N, N_train) {
  pred <- dplyr::mutate(pred, id = dplyr::row_number())
  pred <- dplyr::mutate(pred, crate = stats::qlogis(crate) * 10)

  pred_long <- data.table::melt(pred, id.vars = "id")
  theta_long <- data.table::as.data.table(test_data$y)
  data.table::setnames(theta_long, names(theta))
  theta_long$id <- seq_len(nrow(theta_long))

  theta_long$crate <- stats::qlogis(theta_long$crate) * 10
  theta_long <- data.table::melt(theta_long, id.vars = "id")

  alldat <- rbind(
    cbind(pred_long, Type = "Predicted"),
    cbind(theta_long, Type = "Observed")
  )

  p1 <- ggplot2::ggplot(alldat, ggplot2::aes(x = value, colour = Type)) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = "Boxplot: Predicted vs Observed")

  print(p1)

  alldat_wide <- data.table::dcast(alldat, id + variable ~ Type, value.var = "value")

  vnames <- data.table::data.table(
    variable = c("preval", "crate", "ptran", "prec"),
    Name = paste(
      c("Init. state", "Contact Rate", "P(transmit)", "P(recover)"),
      sprintf("(MAE: %.2f)", MAEs)
    )
  )

  alldat_wide <- merge(alldat_wide, vnames, by = "variable")

  p2 <- ggplot2::ggplot(alldat_wide, ggplot2::aes(x = Observed, y = Predicted)) +
    ggplot2::facet_wrap(~ Name, scales = "free") +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::geom_point(alpha = .2) +
    ggplot2::labs(
      title    = "Observed vs Predicted (validation set)",
      subtitle = sprintf(
        "The model includes %i simulated datasets, of which %i were used for training.",
        N, N_train
      ),
      caption  = "Predictions made using a CNN as implemented with loss function MAE."
    )

  print(p2)
}
