
#' Train the CNN Model
#'
#' @description
#' Trains the CNN model on the provided training data.
#'
#' @param model Keras model. The compiled model to be trained.
#' @param train_data List. A list containing training data (`x` and `y`).
#' @param epochs Integer. The number of epochs for training.
#' @param verbose Integer. Verbosity mode for training output.
#' @return The trained model (updated in place).
#' @export
train_model <- function(model, train_data, epochs, verbose) {
  tensorflow::set_random_seed(331)
  model |> keras3::fit(train_data$x, train_data$y, epochs = epochs, verbose = verbose)
}
