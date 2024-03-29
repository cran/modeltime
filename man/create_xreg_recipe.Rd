% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dev-xregs.R
\name{create_xreg_recipe}
\alias{create_xreg_recipe}
\title{Developer Tools for preparing XREGS (Regressors)}
\usage{
create_xreg_recipe(
  data,
  prepare = TRUE,
  clean_names = TRUE,
  dummy_encode = TRUE,
  one_hot = FALSE
)
}
\arguments{
\item{data}{A data frame}

\item{prepare}{Whether or not to run \code{recipes::prep()} on the final recipe.
Default is to prepare. User can set this to FALSE to return an un prepared recipe.}

\item{clean_names}{Uses \code{janitor::clean_names()} to process the names and improve robustness
to failure during dummy (one-hot) encoding step.}

\item{dummy_encode}{Should \code{factors} (categorical data) be}

\item{one_hot}{If \code{dummy_encode = TRUE}, should the encoding return
one column for each feature or one less column than each feature. Default is \code{FALSE}.}
}
\value{
A \code{recipe} in either prepared or un-prepared format.
}
\description{
These functions are designed to assist developers in extending the \code{modeltime}
package. \code{create_xregs_recipe()} makes it simple to automate conversion
of raw un-encoded features to machine-learning ready features.
}
\details{
The default recipe contains steps to:
\enumerate{
\item Remove date features
\item Clean the column names removing spaces and bad characters
\item Convert ordered factors to regular factors
\item Convert factors to dummy variables
\item Remove any variables that have zero variance
}
}
\examples{
library(dplyr)
library(timetk)
library(recipes)
library(lubridate)

predictors <- m4_monthly \%>\%
    filter(id == "M750") \%>\%
    select(-value) \%>\%
    mutate(month = month(date, label = TRUE))
predictors

# Create default recipe
xreg_recipe_spec <- create_xreg_recipe(predictors, prepare = TRUE)

# Extracts the preprocessed training data from the recipe (used in your fit function)
juice_xreg_recipe(xreg_recipe_spec)

# Applies the prepared recipe to new data (used in your predict function)
bake_xreg_recipe(xreg_recipe_spec, new_data = predictors)

}
