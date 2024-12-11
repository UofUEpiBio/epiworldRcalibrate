#' Build a Convolutional Neural Network Model
#'
#' @description
#' Constructs and compiles a CNN model using the `keras3` package.
#'
#' @param input_shape Integer vector. The shape of the input data (excluding batch size).
#' @param output_units Integer. The number of output units (number of output variables).
#' @return A compiled Keras model ready for training.
#' @export
build_cnn_model <- function(input_shape, output_units) {
  model <- keras3::keras_model_sequential() |>
    keras3::layer_conv_2d(
      filters = 32,
      input_shape = c(input_shape, 1),
      activation = "linear",
      kernel_size = c(3, 5)
    ) |>
    keras3::layer_max_pooling_2d(pool_size = 2, padding = 'same') |>
    keras3::layer_flatten() |>
    keras3::layer_dense(units = output_units, activation = 'sigmoid')

  model |> keras3::compile(optimizer = 'adam', loss = 'mse', metrics = 'accuracy')

  return(model)
}
