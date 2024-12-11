#' Calibrate SIR Model Predictions
#'
#' @description
#' Generates predictions for input test data using a pre-trained CNN model.
#'
#' @param data A numeric matrix or array containing the input test data.
#' @return A list containing a `data.table` of predicted values.
#' @export
calibrate_sir <- function(data) {
  ans <- preprocessing_data(data)
  a <- length(ans)
  ans <- tensorflow::tf$reshape(ans, shape = c(1L, 1L, a, 1L))

  model_path <- if (a <= 31) {
    system.file("models", "sir30-cnn.keras", package = "epiworldRcalibrate")
  } else {
    system.file("models", "sir60-cnn.keras", package = "epiworldRcalibrate")
  }

  if (model_path == "") {
    stop("Model file not found. Please ensure the models are included in the 'epiworldRcalibrate' package.")
  }

  model <- tensorflow::tf$keras$models$load_model(model_path)

  pred <- model$predict(ans) |>
    data.table::as.data.table() |>
    data.table::setnames(c("preval", "crate", "ptran", "prec"))

  return(list(pred = pred))
}
