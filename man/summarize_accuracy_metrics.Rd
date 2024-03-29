% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yardstick-metric-sets.R
\name{summarize_accuracy_metrics}
\alias{summarize_accuracy_metrics}
\title{Summarize Accuracy Metrics}
\usage{
summarize_accuracy_metrics(data, truth, estimate, metric_set)
}
\arguments{
\item{data}{A \code{data.frame} containing the truth and estimate columns.}

\item{truth}{The column identifier for the true results (that is numeric).}

\item{estimate}{The column identifier for the predicted results (that is also numeric).}

\item{metric_set}{A \code{yardstick::metric_set()} that is used to summarize one or more
forecast accuracy (regression) metrics.}
}
\description{
This is an internal function used by \code{modeltime_accuracy()}.
}
\examples{
library(tibble)
library(dplyr)

predictions_tbl <- tibble(
    group = c("model 1", "model 1", "model 1",
              "model 2", "model 2", "model 2"),
    truth = c(1, 2, 3,
              1, 2, 3),
    estimate = c(1.2, 2.0, 2.5,
                 0.9, 1.9, 3.3)
)

predictions_tbl \%>\%
    group_by(group) \%>\%
    summarize_accuracy_metrics(
        truth, estimate,
        metric_set = default_forecast_accuracy_metric_set()
    )

}
