#' Prepare Data for TensorFlow Model: Infected Cases
#'
#' @param data A numeric vector representing the number of infected cases over time (e.g., daily incidence).
#'
#' @return A reshaped array of processed data suitable for TensorFlow models. If an error occurs, it returns the error object.
#' @export
prepare_data_infected <- function(data) {
  if (!is.numeric(data)) {
    stop("Input 'data' must be a numeric vector.")
  }

  max_days <- length(data)

  err <- tryCatch({
    incidence_table <- data.table::data.table(
      day = seq_len(length(data)),
      Infected = data
    )

    incidence_table <- incidence_table[incidence_table$day <= max_days, ]

    incidence_table$Infected <- data.table::nafill(
      incidence_table$Infected, type = "locf"
    )

    dprep <- diff(as.matrix(incidence_table$Infected))
    dprep <- t(dprep)

    ans_array <- array(dim = c(1, length(dprep), 1))
    ans_array[1, , 1] <- dprep

    result <- tensorflow::array_reshape(ans_array, dim = c(1, length(dprep), 1))
    return(result)

  }, error = function(e) e)

  if (inherits(err, "error")) {
    return(err)
  }

  return(err)
}
