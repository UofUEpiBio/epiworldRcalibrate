#' Build CNN Model
#'
#' Constructs a CNN model using `keras`.
#'
#' @param input_shape Integer vector. Shape of the input data.
#' @param output_units Integer. Number of output units.
#' @return A compiled CNN model.
#' @export
build_cnn_model <- function(input_shape, output_units) {
  model <- keras3::keras_model_sequential() %>%
    keras3::layer_conv_2d(
      filters = 32,
      input_shape = c(input_shape, 1),
      activation = "linear",
      kernel_size = c(3, 5)
    ) %>%
    keras3::layer_max_pooling_2d(pool_size = 2, padding = 'same') %>%
    keras3::layer_flatten(input_shape = input_shape) %>%
    keras3::layer_dense(units = output_units, activation = 'sigmoid')

  model %>% compile(optimizer = 'adam', loss = 'mse', metric = 'accuracy')
  return(model)
}
