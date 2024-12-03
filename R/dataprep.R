#' Prepare Data for TensorFlow Model: General SIR Data Preparation
#'
#' @param m An `epiworldR` model object.
#' @param max_days The maximum number of days to consider for data preparation. Defaults to 50.
#'
#' @return A reshaped array of processed data suitable for TensorFlow models. If an error occurs, it returns the error object.
#'
#' @export

prepare_data <- function(m, max_days = 50) {

  err <- tryCatch({
    ans <- list(
      repnum    = epiworldR::plot_reproductive_number(m, plot = FALSE),
      incidence = epiworldR::plot_incidence(m, plot = FALSE),
      gentime   = epiworldR::plot_generation_time(m, plot = FALSE)
    )

    # Filling
    ans <- lapply(ans, data.table::as.data.table)

    # Replacing NaN and NAs with the previous value
    # in each element in the list
    # Replace NA values with the last observed value
    ans$repnum$avg <- data.table::nafill(ans$repnum$avg, type = "locf")

    # Replace NA values with the last observed value
    ans$gentime$avg <- data.table::nafill(ans$gentime$avg, type = "locf")


    # Filtering up to max_days
    ans$repnum    <- ans$repnum[ans$repnum$date <= max_days,]
    ans$gentime   <- ans$gentime[ans$gentime$date <= max_days,]
    ans$incidence <- ans$incidence[as.integer(rownames(ans$incidence)) <= (max_days + 1),]

    # Reference table for merging
    # ndays <- epiworldR::get_ndays(m)

    ref_table <- data.table::data.table(
      date = 0:max_days
    )

    # Replace the $ with the [[ ]] to avoid the warning in the next
    # two lines
    ans[["repnum"]] <- data.table::merge.data.table(
      ref_table, ans[["repnum"]], by = "date", all.x = TRUE
    )
    ans[["gentime"]] <- data.table::merge.data.table(
      ref_table, ans[["gentime"]], by = "date", all.x = TRUE
    )


    # Generating the data.table with necessary columns
    ans <- data.table::data.table(
      infected  = ans[["incidence"]][["Infected"]],
      recovered = ans[["incidence"]][["Recovered"]],
      repnum    = ans[["repnum"]][["avg"]],
      gentime   = ans[["gentime"]][["avg"]],
      repnum_sd = ans[["repnum"]][["sd"]],
      gentime_sd = ans[["gentime"]][["sd"]]
    )

    # Replace NA values with the last observed value for all columns
    nafill_cols <- c("infected", "recovered", "repnum", "gentime", "repnum_sd", "gentime_sd")

    for (col in nafill_cols) {
      ans[[col]] <- data.table::nafill(ans[[col]], type = "locf")
    }

  }, error = function(e) e)

  # If there is an error, return NULL
  if (inherits(err, "error")) {
    return(err)
  }

  # Returning without the first observation (which is mostly zero)
  dprep <- t(diff(as.matrix(ans[-1,])))

  ans <- array(dim = c(1, dim(dprep)))
  ans[1,,] <- dprep
  abm_hist_feat <- ans

  array_reshape(
    abm_hist_feat,
    dim = c(1, dim(dprep))
  )

}



#
# prepare_data_infections_only <- function(m, ...) {
#   UseMethod("prepare_data_infectios_only")
# }
#
# prepare_data_infections_only.epiworld_model <- function(m, ...) {
#   ans <- epiworldR::plot_incidence(m, plot = FALSE) |>
#     data.table::as.data.table()
#
#   prepare_data_infections_only.data.table(
#     m = ans,
#     ...
#   )
# }
#
# prepare_data_infections_only.default <- function(m, max_days = 50, ...) {
#
#   err <- tryCatch({
#     ans <- list(
#       incidence = data.table(Infected=m)
#     )
#
#     # Replacing NaN and NAs with the previous value
#     # in each element in the list
#     # Filtering up to max_days
#     ans$incidence <- ans$incidence[as.integer(rownames(ans$incidence)) <= (max_days + 1),]
#
#     # Reference table for merging
#     # ndays <- epiworldR::get_ndays(m)
#     ref_table <- data.table::data.table(
#       date = 0:max_days
#     )
#
#     # Generating the arrays
#     ans <- data.table::data.table(
#       infected =  ans[["incidence"]][["Infected"]]
#     )
#
#     # Filling NAs with last obs
#     ans[, "infected" := data.table::nafill(.SD[[1]], "locf"),
#         .SDcols = "infected"]
#
#   }, error = function(e) e)
#
#   # If there is an error, return NULL
#   if (inherits(err, "error")) {
#     return(err)
#   }
#
#   # Returning without the first observation (which is mostly zero)
#   dprep <- t(diff(as.matrix(ans[-1,])))
#
#   ans <- array(dim = c(1, dim(dprep)))
#   ans[1,,] <- dprep
#   abm_hist_feat <- ans
#
#   array_reshape(
#     abm_hist_feat,
#     dim = c(1, dim(dprep))
#   )
#
# }
