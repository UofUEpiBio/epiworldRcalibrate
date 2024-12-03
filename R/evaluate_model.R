#' Evaluate the CNN Model
#'
#' @description
#' Evaluates the trained model on test data, makes predictions, and calculates Mean Absolute Error (MAE).
#'
#' @param model Keras model. The trained model to be evaluated.
#' @param test_data List. A list containing test data (`x` and `y`).
#' @param theta `data.table`. The parameters corresponding to the simulations.
#' @return A list containing:
#' \describe{
#'   \item{pred}{The predicted values.}
#'   \item{MAEs}{The Mean Absolute Errors for each output variable.}
#' }
#' @export
evaluate_model <- function(model, test_data, theta) {
  pred <- predict(model, x = test_data$x) |>
    data.table::as.data.table() |>
    data.table::setnames(colnames(theta))

  MAEs <- abs(pred - as.matrix(test_data$y)) |> colMeans()

  return(list(pred = pred, MAEs = MAEs))
}
