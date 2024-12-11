#' Prepare Data for TensorFlow Model: General SIR Data Preparation
#'
#' @param m An `epiworldR` model object.
#' @param max_days The maximum number of days to consider for data preparation. Defaults to 50.
#' @return A reshaped array of processed data suitable for TensorFlow models.
#'   If an error occurs, it returns the error object.
#'
#' @export
prepare_data <- function(m, max_days = max_days) {

  err <- tryCatch({
    ans <- list(
      repnum    = epiworldR::plot_reproductive_number(m, plot = FALSE),
      incidence = epiworldR::plot_incidence(m, plot = FALSE),
      gentime   = epiworldR::plot_generation_time(m, plot = FALSE)
    )

    # Convert all elements to data.table
    ans <- lapply(ans, data.table::as.data.table)

    # Replace NA values in repnum$avg and gentime$avg with the last observed value
    ans[["repnum"]][, avg := data.table::nafill(avg, type = "locf")]
    ans[["gentime"]][, avg := data.table::nafill(avg, type = "locf")]

    # Filter up to max_days
    ans[["repnum"]] <- ans[["repnum"]][date <= max_days, ]
    ans[["gentime"]] <- ans[["gentime"]][date <= max_days, ]

    # incidence is indexed by row number since date is not explicitly in that data
    # We assume the first row represents day 0, hence (max_days + 1) rows total.
    ans[["incidence"]] <- ans[["incidence"]][as.integer(.I) <= (max_days + 1), ]

    # Create a reference table for merging
    ref_table <- data.table::data.table(date = 0:max_days)

    # Merge repnum and gentime with the reference table to ensure consistent length
    ans[["repnum"]] <- data.table::merge.data.table(
      ref_table, ans[["repnum"]], by = "date", all.x = TRUE
    )
    ans[["gentime"]] <- data.table::merge.data.table(
      ref_table, ans[["gentime"]], by = "date", all.x = TRUE
    )

    # Create a data.table with all required columns
    ans <- data.table::data.table(
      infected   = ans[["incidence"]][["Infected"]],
      recovered  = ans[["incidence"]][["Recovered"]],
      repnum     = ans[["repnum"]][["avg"]],
      gentime    = ans[["gentime"]][["avg"]],
      repnum_sd  = ans[["repnum"]][["sd"]],
      gentime_sd = ans[["gentime"]][["sd"]]
    )

    # Replace NA values in all relevant columns using locf
    nafill_cols <- c("infected", "recovered", "repnum", "gentime", "repnum_sd", "gentime_sd")
    for (col in nafill_cols) {
      ans[[col]] <- data.table::nafill(ans[[col]], type = "locf")
    }

    # Return ans as processed data
    ans
  }, error = function(e) e)

  # If there was an error, return it
  if (inherits(err, "error")) {
    return(err)
  }

  # Remove the first observation (often zero) and take differences
  # ans is now a data.table with rows representing days
  # ans[-1, ] removes the first row
  dprep <- t(diff(as.matrix(err[-1,])))

  # Construct a 3D array with shape (1, n_features, n_timesteps)
  # Here n_features = number of variables (rows of dprep after transpose)
  # and n_timesteps = number of columns (days-1)
  ans_array <- array(dim = c(1, dim(dprep)[1], dim(dprep)[2]))
  ans_array[1,,] <- dprep

  # Reshape for TensorFlow input using keras3 (adjust if using another keras interface)
  keras3::array_reshape(ans_array, dim = c(1, dim(dprep)))
}
