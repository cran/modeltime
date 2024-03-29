% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeltime-forecast.R
\name{mdl_time_forecast}
\alias{mdl_time_forecast}
\title{Modeltime Forecast Helpers}
\usage{
mdl_time_forecast(
  object,
  calibration_data,
  new_data = NULL,
  h = NULL,
  actual_data = NULL,
  bind_actual = TRUE,
  keep_data = FALSE,
  arrange_index = FALSE,
  ...
)
}
\arguments{
\item{object}{A Modeltime Table}

\item{calibration_data}{Data that has been calibrated from a testing set}

\item{new_data}{A \code{tibble} containing future information to forecast.
If \code{NULL}, forecasts the calibration data.}

\item{h}{The forecast horizon (can be used instead of \code{new_data} for
time series with no exogenous regressors).
Extends the calibration data \code{h} periods into the future.}

\item{actual_data}{Reference data that is combined with the output tibble and given a \code{.key = "actual"}}

\item{bind_actual}{Logical. Whether or not to skip rowwise binding of `actual_data``}

\item{keep_data}{Whether or not to keep the \code{new_data} and \code{actual_data} as extra columns in the results.
This can be useful if there is an important feature in the \code{new_data} and \code{actual_data} needed
when forecasting.
Default: \code{FALSE}.}

\item{arrange_index}{Whether or not to sort the index in rowwise chronological order (oldest to newest) or to
keep the original order of the data.
Default: \code{FALSE}.}

\item{...}{Not currently used}
}
\value{
A tibble with forecast features
}
\description{
Used for low-level forecasting of modeltime, parnsip and workflow models.
These functions are not intended for user use.
}
\keyword{internal}
