
#' Preprocesses Data for Model Input
#'
#' This function preprocesses data for use in modeling. It handles missing values, filters data
#' based on the maximum number of days, and reshapes the output into a structured array format.
#'
#' @param m A data structure containing the input data. Assumes a named list with an element `Infected`.
#'
#' @return A reshaped array containing preprocessed data. If an error occurs, the function returns an error object.
#'
#' @details
#' The function performs the following steps:
#' - Converts the input data to a `data.table` format.
#' - Filters rows based on `max_days`.
#' - Fills missing values using the last observation carried forward ("locf").
#' - Calculates the difference between consecutive rows after the first observation.
#' - Reshapes the processed data into a 3D array.
#'
#' If any error occurs during processing, the function catches the error and returns it.
#'
#' @importFrom data.table data.table nafill
#' @importFrom keras3 array_reshape
#' @export
preprocessing_data <- function(data) {
 max_days=length(data)
  err <- tryCatch({
    ans <- list(
      incidence = data.table(Infected=data)
    )

    # Replacing NaN and NAs with the previous value
    # in each element in the list
    # Filtering up to max_days
    ans$incidence <- ans$incidence[as.integer(rownames(ans$incidence)) <= (max_days + 1),]

    # Reference table for merging
    # ndays <- epiworldR::get_ndays(m)
    ref_table <- data.table::data.table(
      date = 0:max_days
    )

    # Generating the arrays
    ans <- data.table::data.table(
      infected =  ans[["incidence"]][["Infected"]]
    )

    # Filling NAs with last obs
    ans[, "infected" := data.table::nafill(.SD[[1]], "locf"),
        .SDcols = "infected"]

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

