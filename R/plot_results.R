
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
#' @importFrom keras3 array_reshape
#' @importFrom data.table dcast melt
#' @return Generates plots (displayed on the active graphics device).
#' @export

plot_results <- function(pred, test_data, theta, MAEs, N, N_train) {
  # Prepare the data for plotting
  library(dplyr)
  library(ggplot2)

  # Add an 'id' column
  pred <- pred |>
    mutate(id = row_number())

  # Modify the 'crate' column
  pred <- pred |>
    mutate(crate = qlogis(crate) * 10)

  pred_long <- melt(pred, id.vars = "id")
  theta_long <- test_data$y |>  data.table::as.data.table()
  data.table::setnames(theta_long, names(theta))
  theta_long$id <- seq_len(nrow(theta_long))

  # Modify the 'crate' column in theta_long
  theta_long$crate <- qlogis(theta_long$crate) * 10
  theta_long <- melt(theta_long, id.vars = "id")

  alldat <- rbind(
    cbind(pred_long, Type = "Predicted"),
    cbind(theta_long, Type = "Observed")
  )

  # Plot 1: Boxplot of Predicted vs Observed values
  p1 <- ggplot2::ggplot(alldat, aes(x = value, colour = Type)) +
    facet_wrap(~variable, scales = "free") +
    geom_boxplot() +
    labs(title = "Boxplot: Predicted vs Observed")

  print(p1)  # Display the first plot

  # Prepare data for second plot
  alldat_wide <- dcast(alldat, id + variable ~ Type, value.var = "value")

  vnames <- data.table::data.table(
    variable = c("preval", "crate", "ptran", "prec"),
    Name     = paste(
      c("Init. state", "Contact Rate", "P(transmit)", "P(recover)"),
      sprintf("(MAE: %.2f)", MAEs)
    )
  )

  alldat_wide <- merge(alldat_wide, vnames, by = "variable")

  # Plot 2: Observed vs Predicted with MAE labels
  p2 <- ggplot2::ggplot(alldat_wide, aes(x = Observed, y = Predicted)) +
    facet_wrap(~ Name, scales = "free") +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(alpha = .2) +
    labs(
      title    = "Observed vs Predicted (validation set)",
      subtitle = sprintf(
        "The model includes %i simulated datasets, of which %i were used for training.",
        N, N_train
      ),
      caption  = "Predictions made using a CNN as implemented with loss function MAE."
    )

  print(p2)  # Display the second plot
}
