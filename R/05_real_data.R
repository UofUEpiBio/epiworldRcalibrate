# =============================================================================
# 05_real_data.R
# Real epidemic data shipped with the package, and windows cut from them
#
# This file contains:
#   - the documentation of the datasets: covid_us_states, covid_utah,
#     admissions_utah, flu_baltimore_1918 and real_data_waves
#   - wave_data(): the daily counts of one wave of real_data_waves
#   - real_data_windows(): windows from the beginning, middle and end of the
#     waves, ready for calibrate() and compare_methods()
#
# The datasets are built by data-raw/real_data.R.
# =============================================================================

#' Daily COVID-19 cases in US states
#'
#' Daily new COVID-19 cases in the 50 US states and Washington, DC, from 22
#' January 2020 to 9 March 2023: the daily differences of the cumulative county
#' counts added up per state (negative corrections set to 0).
#'
#' @format A data frame with 58,293 rows:
#' \describe{
#'   \item{state}{State name.}
#'   \item{date}{Date of report.}
#'   \item{cases}{Daily new cases.}
#'   \item{population}{State population (sum of the county populations in the
#'     source).}
#' }
#' @source COVID-19 Data Repository by the Center for Systems Science and
#'   Engineering (CSSE) at Johns Hopkins University,
#'   \url{https://github.com/CSSEGISandData/COVID-19} (CC BY 4.0). Dong E, Du H,
#'   Gardner L (2020). An interactive web-based dashboard to track COVID-19 in
#'   real time. The Lancet Infectious Diseases 20(5), 533-534.
#' @seealso [real_data_waves], [real_data_windows()]
"covid_us_states"

#' Daily COVID-19 cases in Utah
#'
#' @format A data frame with 1,876 rows (18 March 2020 to 6 May 2025):
#' \describe{
#'   \item{date}{Date.}
#'   \item{cases}{Daily new cases.}
#' }
#' @source Utah Department of Health and Human Services,
#'   \url{https://coronavirus-dashboard.utah.gov/Utah_COVID19_data.zip}.
#' @seealso [real_data_waves], [real_data_windows()]
"covid_utah"

#' Daily hospital admissions for influenza and COVID-19 in Utah
#'
#' Confirmed influenza and COVID-19 (adult plus pediatric) hospital admissions
#' in Utah. The source reports the admissions of the previous day; `date` is
#' the day of admission.
#'
#' @format A data frame with 1,500 rows (13 March 2020 to 26 April 2024; one
#'   gap of 6 days in March 2020):
#' \describe{
#'   \item{date}{Day of admission.}
#'   \item{influenza}{Confirmed influenza admissions (`NA`: not reported).}
#'   \item{covid}{Confirmed COVID-19 admissions (`NA`: not reported).}
#' }
#' @source U.S. Department of Health and Human Services, COVID-19 Reported
#'   Patient Impact and Hospital Capacity by State Timeseries,
#'   \url{https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh}.
#' @seealso [real_data_waves], [real_data_windows()]
"admissions_utah"

#' Daily influenza cases in Baltimore, 1918
#'
#' Daily cases (onset of symptoms) of the 1918 influenza epidemic in Baltimore,
#' Maryland, over 92 days.
#'
#' @format A data frame with 92 rows:
#' \describe{
#'   \item{day}{Day of the epidemic.}
#'   \item{cases}{Daily new cases.}
#' }
#' @source Frost WH, Sydenstricker E (1919). Influenza in Maryland: preliminary
#'   statistics of certain localities. Public Health Reports 34(11), 491-504; as
#'   in the `Flu1918` data of the EpiEstim package (Cori A, Ferguson NM, Fraser
#'   C, Cauchemez S (2013). A new framework and software to estimate
#'   time-varying reproduction numbers during epidemics. American Journal of
#'   Epidemiology 178(9), 1505-1512).
#' @seealso [real_data_waves], [real_data_windows()]
"flu_baltimore_1918"

#' The real-data waves of the report
#'
#' The 35 waves used to compare the methods on real data: COVID-19 in 10 US
#' states that reported every day (fall 2020, Delta 2021 and Omicron 2021-22),
#' Utah COVID-19 cases (the last 365 days of the data and the summer 2023
#' wave), Utah influenza admissions (2022-23 and 2023-24 seasons) and Baltimore
#' 1918. A season is the part with the peak, between the last week without
#' cases before the peak and the first one after it.
#'
#' @format A data frame with one row per wave:
#' \describe{
#'   \item{wave}{Name of the wave.}
#'   \item{group}{`"US states"` or `"Utah and Baltimore"`.}
#'   \item{dataset, region, column}{Where the counts are: the dataset, the
#'     state (or `"Utah"`, `"Baltimore"`) and the column.}
#'   \item{start, end}{First and last day of the wave (`NA`: the whole
#'     series).}
#'   \item{n}{Population size given to the models.}
#'   \item{recov, incub}{Known recovery rate and incubation period (days):
#'     1/7 and 5 for COVID-19, 0.25 and 2 for influenza.}
#'   \item{disease}{`"COVID-19"` or `"influenza"`.}
#'   \item{per_10000}{`TRUE` when the counts are scaled to cases per 10,000
#'     people (US states, with `n` = 10,000).}
#' }
#' @seealso [wave_data()], [real_data_windows()]
"real_data_waves"

.dataset <- function(name) {
  e <- new.env()
  utils::data(list = name, package = "epiworldRcalibrate", envir = e)
  e[[name]]
}

# The full daily series of a wave's region (x: date or day, y: count, scaled
# per 10,000 people for US states) and which days make up the wave.
.wave_series <- function(row) {
  d <- .dataset(row$dataset)
  if (row$dataset == "covid_us_states") {
    d <- d[d$state == row$region, ]
    y <- round(d$cases * 10000 / d$population)
  } else {
    y <- d[[row$column]]
  }
  x <- if ("date" %in% names(d)) d$date else d$day
  idx <- if (is.na(row$start)) seq_along(x) else which(x >= row$start & x <= row$end)
  list(x = x, y = as.numeric(y), idx = idx)
}

.wave_row <- function(wave) {
  tab <- .dataset("real_data_waves")
  if (!all(wave %in% tab$wave)) {
    stop("Unknown wave(s): ", paste(setdiff(wave, tab$wave), collapse = ", "), ". See real_data_waves$wave.")
  }
  tab[match(wave, tab$wave), , drop = FALSE]
}

#' The daily counts of a real-data wave
#'
#' @param wave A name from `real_data_waves$wave`, for example `"New York
#'   Omicron 2021-22"`.
#' @return A data frame with `time` (date, or day for Baltimore 1918) and
#'   `count` (daily new cases or admissions; cases per 10,000 people for US
#'   states), with the known values `n`, `recov` and `incub` as attributes.
#' @examples
#' w <- wave_data("Utah flu 2022-23")
#' plot(w$time, w$count, type = "h")
#' @export
wave_data <- function(wave) {
  row <- .wave_row(wave[1])
  s <- .wave_series(row)
  structure(data.frame(time = s$x[s$idx], count = s$y[s$idx]), wave = row$wave, n = row$n, recov = row$recov,
            incub = row$incub)
}

#' Windows of real data for calibrate() and compare_methods()
#'
#' Cuts windows of the given lengths from the beginning, the middle (centered on
#' the peak of the 7-day average) and the end of each wave in
#' [real_data_waves], as in the report. With the defaults it returns the report's
#' 315 windows (270 from US states, 45 from Utah and Baltimore). Windows longer
#' than a wave are skipped.
#'
#' @param waves Names from `real_data_waves$wave`; `NULL` for all.
#' @param lengths Window lengths (days).
#' @param positions Any of `"beginning"`, `"middle"`, `"end"`.
#' @param ahead Days after the window kept as `future` (where the data
#'   continue).
#' @return A named list of windows, each with `cases`, `future`, the known
#'   values `n`, `recov` and `incub`, the labels `wave`, `group`, `disease` and
#'   `position`, and `time` (the dates of the window).
#' @examples
#' windows <- real_data_windows("New York Omicron 2021-22", lengths = 60)
#' w <- windows[[1]]
#' \dontrun{
#' fit <- calibrate(w$cases, window_calibrator("SEIR"), n = w$n, recov = w$recov, incub = w$incub)
#' results <- compare_methods(windows, "SEIR", cores = 3)
#' }
#' @export
real_data_windows <- function(waves = NULL, lengths = c(30L, 60L, 90L), positions = c("beginning", "middle", "end"),
                              ahead = 14L) {
  tab <- .dataset("real_data_waves")
  if (!is.null(waves)) tab <- .wave_row(waves)
  positions <- match.arg(positions, several.ok = TRUE)
  out <- list()
  for (i in seq_len(nrow(tab))) {
    row <- tab[i, ]
    s <- .wave_series(row)
    wy <- s$y[s$idx]
    pk <- which.max(as.numeric(stats::filter(wy, rep(1 / 7, 7), sides = 2)))
    for (L in lengths) {
      if (L > length(s$idx)) next
      for (pos in positions) {
        s0 <- switch(pos, beginning = 1, end = length(wy) - L + 1,
                     middle = min(max(1, pk - floor(L / 2)), length(wy) - L + 1))
        at <- s$idx[s0:(s0 + L - 1)]
        fut <- at[L] + seq_len(ahead)
        fut <- fut[fut <= length(s$y)]
        out[[sprintf("w%03d", length(out) + 1)]] <- list(
          cases = s$y[at], future = s$y[fut], n = row$n, recov = row$recov, incub = row$incub,
          wave = row$wave, group = row$group, disease = row$disease, position = pos, time = s$x[at])
      }
    }
  }
  out
}
