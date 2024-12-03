# calibrate <- function(y, fitted_model) {
#
#   if (!length(fitted_model))
#     stop("You need to specify a fitted model")
#
#
#   # Read the model
#   model <- keras3::read_my_model(fitted_mode)
#
#   prepro <- preprocess_data(y)
#
#   predict(y, model)
#
#
#
# }
#
# calibrate_sir <- function(y) {
#
#   calibrate(
#     y,
#     fitted_model = system.file("models/sir_30.keras", package="epiworldRcalibrate")
#   )
#
# }
#' #' THis is myu function
#' #' @param model
#' #' @param ndays
#' #' @noRd
#' verbose_off_and_run <- function(model, ndays) {
#'
#'   verbose_off(model)
#'   run(model, ndays)
#' }

