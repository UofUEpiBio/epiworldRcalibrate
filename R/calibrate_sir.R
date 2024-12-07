
#' Predict Parameters Using a CNN Model
#' Generates predictions for input test data using a pre-trained Convolutional Neural Network (CNN) model.
#'
#' This function loads a specific Keras CNN model based on the length of the input data and uses it to make predictions. The predicted values are returned as a `data.table` with standardized column names.
#'
#' @param data A numeric matrix or array containing the input test data. The function determines which model to load based on the number of columns in `data` (expects either 30 or 60).
#'
#' @return A list containing:
#' \describe{
#'   \item{pred}{A `data.table` of predicted values with the following columns:
#'     \describe{
#'       \item{\code{preval}}{Predicted prevalence.}
#'       \item{\code{crate}}{Predicted case rate.}
#'       \item{\code{ptran}}{Predicted transmission probability.}
#'       \item{\code{prec}}{Predicted precision.}
#'     }
#'   }
#' }
#'
#' @details
#' The function determines which pre-trained CNN model to load based on the number of features (columns) in the input `data`. If `data` has 30 columns, it loads the `sir30-cnn.keras` model; if it has 60 columns, it loads the `sir60-cnn.keras` model. Ensure that the input data matches one of these expected formats to avoid errors.
#' @export
# calibrate_sir <- function(data) {
#   library(keras3)
#   ans=preprocessing_data(data)
#   a=length(ans)
#   ans <- tensorflow::array_reshape(ans, dim = c(1, 1, a, 1))
#
#   if(a <=30){
#     model <- keras3::load_model(
#       system.file("models", "sir30-cnn.keras", package = "epiworldRcalibrate")
#       )
#   }
#   else{
#     model <- keras3::load_model(
#       system.file("models", "sir60-cnn.keras", package = "epiworldRcalibrate")
#       )
#   }
#   pred <- predict(model, x =ans ) |>
#     data.table::as.data.table() |>
#     data.table::setnames(c("preval","crate","ptran","prec"))
#   pred$crate=qlogis(pred$crate)
#
#   return(list(pred = pred))
# }

calibrate_sir <- function(data) {
  # Load required libraries
  library(tensorflow)
  library(data.table)

  # Preprocess the data
  ans <- preprocessing_data(data)
  a <- length(ans)
  ans <- tensorflow::array_reshape(ans, dim = c(1, 1, a, 1))  # Reshape for the model

  # Determine model file path
  model_path <- if (a <= 30) {
    system.file("models", "sir30-cnn.keras", package = "epiworldRcalibrate")
  } else {
    system.file("models", "sir60-cnn.keras", package = "epiworldRcalibrate")
  }

  # Check if the model file exists
  if (model_path == "") {
    stop("Model file not found. Please ensure the models are included in the 'epiworldRcalibrate' package.")
  }

  # Load the model using tensorflow
  model <- tensorflow::tf$keras$models$load_model(model_path)

  # Make predictions
  pred <- model$predict(ans) |>
    data.table::as.data.table() |>
    data.table::setnames(c("preval", "crate", "ptran", "prec"))

  # Apply the logistic transformation to the 'crate' column
  pred$crate <- qlogis(pred$crate)

  # Return predictions as a list
  return(list(pred = pred))
}


