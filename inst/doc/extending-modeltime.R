## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  out.width = '80%',
  fig.width = 5,
  fig.height = 4,
  fig.align='center'
)

## ----setup--------------------------------------------------------------------
library(parsnip)
library(forecast)
library(rsample)
library(modeltime)
library(tidyverse)
library(timetk)
library(rlang)

## -----------------------------------------------------------------------------
taylor_30_min %>%
  plot_time_series(date, value, .interactive = FALSE)

## -----------------------------------------------------------------------------
stlm_model <- taylor_30_min %>%
  pull(value) %>%
  msts(seasonal.periods = c(24*2, 24*2*7)) %>%
  stlm()

stlm_model$stl %>% autoplot()

## -----------------------------------------------------------------------------
stlm_model %>%
  forecast(h = 24*2*7) %>%
  autoplot()

## -----------------------------------------------------------------------------
set_new_model("decomposition_reg")
set_model_mode(model = "decomposition_reg", mode = "regression")
set_model_engine(model = "decomposition_reg", mode = "regression", eng = "stlm_ets")

# Here we set the dependency to forecast, though we can also use
# your package if you import the lower level package
set_dependency(model = "decomposition_reg", eng = "stlm_ets", pkg = "forecast")

## -----------------------------------------------------------------------------
show_model_info("decomposition_reg")

## -----------------------------------------------------------------------------
# 1st Frequency (period_seasonal_1 is a non-jargony term)
set_model_arg(
  model        = "decomposition_reg", 
  eng          = "stlm_ets", 
  parsnip      = "period_seasonal_1", 
  original     = "period_seasonal_1", 
  func         = list(pkg = "foo", fun = "bar"),
  has_submodel = FALSE
)

# 2nd Frequency (period_seasonal_2 is a non-jargony term)
set_model_arg(
  model        = "decomposition_reg", 
  eng          = "stlm_ets", 
  parsnip      = "period_seasonal_2", 
  original     = "period_seasonal_2", 
  func         = list(pkg = "foo", fun = "bar"),
  has_submodel = FALSE
)


## -----------------------------------------------------------------------------
decomposition_reg <- function(mode = "regression", 
                              period_seasonal_1 = NULL, 
                              period_seasonal_2 = NULL) {

    args <- list(
        period_seasonal_1 = rlang::enquo(period_seasonal_1),
        period_seasonal_2 = rlang::enquo(period_seasonal_2)
    )

    parsnip::new_model_spec(
        "decomposition_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

## -----------------------------------------------------------------------------
bridge_stlm_ets_fit_impl <- function(x, y, period_seasonal_1 = NULL, period_seasonal_2 = NULL, ...) {
  
  outcome    <- y # Comes in as a vector
  predictors <- x # Comes in as a data.frame (dates and possible xregs)
  
  # 1. Create outcome msts object by mapping `period_seasonal` args to msts()
  if (is.null(period_seasonal_1) || period_seasonal_1 <= 1) {
    stop("'period_seasonal_1' must be greater than 1 to assess seasonality")
  } else if (is.null(period_seasonal_2) || period_seasonal_2 <= 1) {
    seasonal.periods <- period_seasonal_1
  } else {
    seasonal.periods <- c(period_seasonal_1, period_seasonal_2)
  }
  outcome_msts <- forecast::msts(outcome, seasonal.periods = seasonal.periods)
  
  # 2. Predictors - Handle Dates 
  index_tbl <- modeltime::parse_index_from_data(predictors)
  idx_col   <- names(index_tbl)
  idx       <- timetk::tk_index(index_tbl)
  
  # 3. Predictors - Handle Xregs
  # NOT REQUIRED - ETS is univariate
  # REQUIRED FOR ARIMA - ARIMA can accept XRegs
  # xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
  # xreg_matrix <- juice_xreg_recipe(xreg_recipe, format = "matrix")
  
  # 4. Fitting
  model_1 <- forecast::stlm(y = outcome_msts, method = "ets", ...)
  
  # 5. New Modeltime Bridge
  new_modeltime_bridge(
    class  = "bridge_stlm_ets_fit_impl",
    models = list(model_1 = model_1),
    data   = tibble::tibble(
      idx_col   := idx,
      .actual    = y,
      .fitted    = model_1$fitted,
      .residuals = model_1$residuals
    ),
    extras = list(NULL), # Can add xreg preprocessors here
    desc   = stringr::str_c("STLM Model: ", model_1$model$method)
  )
  
}

## -----------------------------------------------------------------------------
print.bridge_stlm_ets_fit_impl <- function(x, ...) {
  
  model <- x$models$model_1$model
  
  cat(x$desc)
  cat("\n")
  print(model$call)
  cat("\n")
  print(
    tibble(
      aic    = model$aic,
      bic    = model$bic,
      aicc   = model$aicc,
      loglik = model$loglik,
      mse    = model$mse  
    )
  )
  invisible(x)
}

## ---- paged.print = FALSE-----------------------------------------------------
stlm_test <- bridge_stlm_ets_fit_impl(
  x = taylor_30_min[,"date"],
  y = taylor_30_min %>% pull(value),
  period_seasonal_1 = 24*2,
  period_seasonal_2 = 24*2*7
)

stlm_test

## ---- paged.print = F---------------------------------------------------------
set_fit(
  model  = "decomposition_reg",
  eng    = "stlm_ets",
  mode   = "regression",
  value  = list(
    interface = "data.frame",
    protect   = c("x", "y"),
    func      = c(fun = "bridge_stlm_ets_fit_impl"),
    defaults  = list()
  )
)

show_model_info("decomposition_reg")

## -----------------------------------------------------------------------------
predict.bridge_stlm_ets_fit_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$models$model_1
    h_horizon   <- nrow(new_data)

    # XREG
    # NOT REQUIRED FOR ETS. 
    # xreg_recipe <- object$extras$xreg_recipe 
    # xreg_matrix <- bake_xreg_recipe(xreg_recipe, new_data, format = "matrix")

    # PREDICTIONS
    preds_forecast <- forecast::forecast(model, h = h_horizon)
    
    # Return predictions as numeric vector
    preds <- as.numeric(preds_forecast$mean)

    return(preds)

}

## -----------------------------------------------------------------------------
stlm_test %>%
  predict(new_data = taylor_30_min %>% future_frame(.length_out = "1 week")) %>% 
  plot()

## ---- paged.print = FALSE-----------------------------------------------------
set_pred(
    model         = "decomposition_reg",
    eng           = "stlm_ets",
    mode          = "regression",
    type          = "numeric",
    value         = list(
        pre       = NULL,
        post      = NULL,
        func      = c(fun = "predict"),
        args      =
            list(
                object   = rlang::expr(object$fit),
                new_data = rlang::expr(new_data)
            )
    )
)

show_model_info("decomposition_reg")

## ---- eval = F----------------------------------------------------------------
#  parsnip::set_encoding(
#    model   = "decomposition_reg",
#    eng     = "stlm_ets",
#    mode    = "regression",
#    options = list(
#      predictor_indicators = "none",
#      compute_intercept = FALSE,
#      remove_intercept = FALSE
#    )
#  )

## -----------------------------------------------------------------------------
splits <- initial_time_split(taylor_30_min, prop = 0.9)

## ---- paged.print = FALSE, eval=F---------------------------------------------
#  model_fit <- decomposition_reg(
#      period_seasonal_1 = 24*2,
#      period_seasonal_2 = 24*2*7
#    ) %>%
#    set_engine("stlm_ets") %>%
#    fit(value ~ date, data = training(splits))
#  
#  model_fit

## ----echo=F-------------------------------------------------------------------
model_fit <- read_rds("model_fit.rds")
model_fit

## ---- paged.print = FALSE-----------------------------------------------------
calibration_tbl <- model_fit %>%
  modeltime_table() %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

## ----eval=F-------------------------------------------------------------------
#  refit_tbl <- calibration_tbl %>%
#    modeltime_refit(data = taylor_30_min)

## ----echo=F-------------------------------------------------------------------
refit_tbl <- read_rds("refit_tbl.rds")

## -----------------------------------------------------------------------------
refit_tbl %>%
  modeltime_forecast(h = "1 week", actual_data = taylor_30_min) %>%
  plot_modeltime_forecast(.interactive = FALSE)

