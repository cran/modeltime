## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = F,
  warning = F,
  echo = F
)

## ----setup, echo = F----------------------------------------------------------
library(modeltime)
library(tidyverse)
library(reactable)

interactive <- TRUE

# Model List
model_list_tbl <- tribble(
  ~ `Model Category`, ~ `Model Function`, ~ `Engine`, ~ `Packages`,
  "Prophet", "prophet_reg()", "prophet", "prophet",
  "Boosted Prophet", "prophet_boost", "prophet_xgboost", "prophet, xgboost",
  "ARIMA", "arima_reg()", "auto_arima", "forecast",
  "ARIMA", "arima_reg()", "arima", "forecast",
  "Boosted ARIMA", "arima_boost()", "auto_arima_xgboost", "forecast, xgboost",
  "Boosted ARIMA", "arima_boost()", "arima_xgboost", "forecast, xgboost",
  "Exponential Smoothing", "exp_smoothing()", "ets", "forecast",
  "Seasonal Decomposition", "seasonal_decomp()", "stlm_ets", "forecast",
  "Seasonal Decomposition", "seasonal_decomp()", "stlm_arima", "forecast"
) %>% 
  mutate(Mode = "regression")

param_list_tbl <- tribble(
  ~ `Model Function`, ~ `Engine`, ~ `Parameter`, ~ `Original Param`, ~ `Original Func`,
  # prophet_reg: prophet
  "prophet_reg", "prophet", "growth", "growth", "prophet::prophet",
  "prophet_reg", "prophet", "num_changepoints", "n.changepoints", "prophet::prophet",
  "prophet_reg", "prophet", "season", "seasonality.mode", "prophet::prophet",
  "prophet_reg", "prophet", "prior_scale_changepoints", "changepoint.prior.scale", "prophet::prophet",
  "prophet_reg", "prophet", "prior_scale_seasonality", "seasonality.prior.scale", "prophet::prophet",
  "prophet_reg", "prophet", "prior_scale_holidays", "holiday.prior.scale", "prophet::prophet",
  # prophet_boost: prophet_xgboost
  "prophet_boost", "prophet_xgboost", "growth", "growth", "prophet::prophet",
  "prophet_boost", "prophet_xgboost", "num_changepoints", "n.changepoints", "prophet::prophet",
  "prophet_boost", "prophet_xgboost", "season", "seasonality.mode", "prophet::prophet",
  "prophet_boost", "prophet_xgboost", "prior_scale_changepoints", "changepoint.prior.scale", "prophet::prophet",
  "prophet_boost", "prophet_xgboost", "prior_scale_seasonality", "seasonality.prior.scale", "prophet::prophet",
  "prophet_boost", "prophet_xgboost", "prior_scale_holidays", "holiday.prior.scale", "prophet::prophet",
  "prophet_boost", "prophet_xgboost", "tree_depth", "max_depth", "xgboost::xgb.train",
  "prophet_boost", "prophet_xgboost", "trees", "nrounds", "xgboost::xgb.train",
  "prophet_boost", "prophet_xgboost", "learn_rate", "eta", "xgboost::xgb.train",
  "prophet_boost", "prophet_xgboost", "mtry", "colsample_bytree", "xgboost::xgb.train",
  "prophet_boost", "prophet_xgboost", "min_n", "min_child_weight", "xgboost::xgb.train",
  "prophet_boost", "prophet_xgboost", "loss_reduction", "gamma", "xgboost::xgb.train",
  "prophet_boost", "prophet_xgboost", "sample_size", "subsample", "xgboost::xgb.train",
  # arima_reg: auto_arima
  "arima_reg", "auto_arima", "seasonal_period", "ts(frequency)", "forecast::auto.arima",
  "arima_reg", "auto_arima", "non_seasonal_ar", "max.p", "forecast::auto.arima",
  "arima_reg", "auto_arima", "non_seasonal_differences", "max.d", "forecast::auto.arima",
  "arima_reg", "auto_arima", "non_seasonal_ma", "max.q", "forecast::auto.arima",
  "arima_reg", "auto_arima", "seasonal_ar", "max.P", "forecast::auto.arima",
  "arima_reg", "auto_arima", "seasonal_differences", "max.D", "forecast::auto.arima",
  "arima_reg", "auto_arima", "seasonal_ma", "max.Q", "forecast::auto.arima",
  # arima_reg: arima
  "arima_reg", "arima", "seasonal_period", "ts(frequency)", "forecast::Arima",
  "arima_reg", "arima", "non_seasonal_ar", "p", "forecast::Arima",
  "arima_reg", "arima", "non_seasonal_differences", "d", "forecast::Arima",
  "arima_reg", "arima", "non_seasonal_ma", "q", "forecast::Arima",
  "arima_reg", "arima", "seasonal_ar", "P", "forecast::Arima",
  "arima_reg", "arima", "seasonal_differences", "D", "forecast::Arima",
  "arima_reg", "arima", "seasonal_ma", "Q", "forecast::Arima",
  # arima_reg: auto_arima_xgboost
  "arima_boost", "auto_arima_xgboost", "seasonal_period", "ts(frequency)", "forecast::auto.arima",
  "arima_boost", "auto_arima_xgboost", "non_seasonal_ar", "max.p", "forecast::auto.arima",
  "arima_boost", "auto_arima_xgboost", "non_seasonal_differences", "max.d", "forecast::auto.arima",
  "arima_boost", "auto_arima_xgboost", "non_seasonal_ma", "max.q", "forecast::auto.arima",
  "arima_boost", "auto_arima_xgboost", "seasonal_ar", "max.P", "forecast::auto.arima",
  "arima_boost", "auto_arima_xgboost", "seasonal_differences", "max.D", "forecast::auto.arima",
  "arima_boost", "auto_arima_xgboost", "seasonal_ma", "max.Q", "forecast::auto.arima",
  "arima_boost", "auto_arima_xgboost", "tree_depth", "max_depth", "xgboost::xgb.train",
  "arima_boost", "auto_arima_xgboost", "trees", "nrounds", "xgboost::xgb.train",
  "arima_boost", "auto_arima_xgboost", "learn_rate", "eta", "xgboost::xgb.train",
  "arima_boost", "auto_arima_xgboost", "mtry", "colsample_bytree", "xgboost::xgb.train",
  "arima_boost", "auto_arima_xgboost", "min_n", "min_child_weight", "xgboost::xgb.train",
  "arima_boost", "auto_arima_xgboost", "loss_reduction", "gamma", "xgboost::xgb.train",
  "arima_boost", "auto_arima_xgboost", "sample_size", "subsample", "xgboost::xgb.train",
  # arima_boost: arima
  "arima_boost", "arima_xgboost", "seasonal_period", "ts(frequency)", "forecast::Arima",
  "arima_boost", "arima_xgboost", "non_seasonal_ar", "p", "forecast::Arima",
  "arima_boost", "arima_xgboost", "non_seasonal_differences", "d", "forecast::Arima",
  "arima_boost", "arima_xgboost", "non_seasonal_ma", "q", "forecast::Arima",
  "arima_boost", "arima_xgboost", "seasonal_ar", "P", "forecast::Arima",
  "arima_boost", "arima_xgboost", "seasonal_differences", "D", "forecast::Arima",
  "arima_boost", "arima_xgboost", "seasonal_ma", "Q", "forecast::Arima",
  "arima_boost", "arima_xgboost", "tree_depth", "max_depth", "xgboost::xgb.train",
  "arima_boost", "arima_xgboost", "trees", "nrounds", "xgboost::xgb.train",
  "arima_boost", "arima_xgboost", "learn_rate", "eta", "xgboost::xgb.train",
  "arima_boost", "arima_xgboost", "mtry", "colsample_bytree", "xgboost::xgb.train",
  "arima_boost", "arima_xgboost", "min_n", "min_child_weight", "xgboost::xgb.train",
  "arima_boost", "arima_xgboost", "loss_reduction", "gamma", "xgboost::xgb.train",
  "arima_boost", "arima_xgboost", "sample_size", "subsample", "xgboost::xgb.train",
  # exp_smoothing: ets
  "exp_smoothing", "ets", "seasonal_period", "ts(frequency)", "forecast::ets",
  "exp_smoothing", "ets", "error", "model = c(error, trend, season)", "forecast::ets",
  "exp_smoothing", "ets", "trend", "model = c(error, trend, season)", "forecast::ets",
  "exp_smoothing", "ets", "season", "model = c(error, trend, season)", "forecast::ets",
  "exp_smoothing", "ets", "damping", "damped", "forecast::ets",
  # seasonal decomp
  "seasonal_decomp", "stlm_ets", "seasonal_period_1", "msts(seasonal.periods)", "forecast::stlm",
  "seasonal_decomp", "stlm_ets", "seasonal_period_2", "msts(seasonal.periods)", "forecast::stlm",
  "seasonal_decomp", "stlm_ets", "seasonal_period_3", "msts(seasonal.periods)", "forecast::stlm",
  # seasonal decomp
  "seasonal_decomp", "stlm_arima", "seasonal_period_1", "msts(seasonal.periods)", "forecast::stlm",
  "seasonal_decomp", "stlm_arima", "seasonal_period_2", "msts(seasonal.periods)", "forecast::stlm",
  "seasonal_decomp", "stlm_arima", "seasonal_period_3", "msts(seasonal.periods)", "forecast::stlm"
)


## ---- echo=F------------------------------------------------------------------
model_list_tbl %>%
  reactable(
    groupBy = "Model Category", 
    searchable = TRUE, 
    defaultExpanded = TRUE, 
    resizable = TRUE, 
    filterable = FALSE, 
    striped = TRUE,
    highlight = TRUE,
    bordered = TRUE,
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      # backgroundColor = "hsl(233, 9%, 19%)",
      backgroundColor = "hsl(210, 29%, 24%)",
      # borderColor = "hsl(233, 9%, 22%)",
      borderColor = "hsl(210, 29%, 27%)",
      # stripedColor = "hsl(233, 12%, 22%)",
      stripedColor = "hsl(210, 32%, 27%)",
      # highlightColor = "hsl(233, 12%, 24%)",
      highlightColor = "hsl(210, 32%, 30%)",
      
      # inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      inputStyle = list(backgroundColor = "hsl(210, 29%, 30%)")
    )
  )

## -----------------------------------------------------------------------------
param_list_tbl %>%
  reactable(
    groupBy = "Engine", 
    searchable = TRUE, 
    defaultExpanded = FALSE, 
    resizable = TRUE, 
    filterable = TRUE, 
    striped = TRUE,
    highlight = TRUE,
    bordered = TRUE,
    defaultPageSize = 10,
    columns = list(
      `Model Function` = colDef(aggregate = "frequency")
    ),
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      # backgroundColor = "hsl(233, 9%, 19%)",
      backgroundColor = "hsl(210, 29%, 24%)",
      # borderColor = "hsl(233, 9%, 22%)",
      borderColor = "hsl(210, 29%, 27%)",
      # stripedColor = "hsl(233, 12%, 22%)",
      stripedColor = "hsl(210, 32%, 27%)",
      # highlightColor = "hsl(233, 12%, 24%)",
      highlightColor = "hsl(210, 32%, 30%)",
      
      # inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      # pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      inputStyle = list(backgroundColor = "hsl(210, 29%, 30%)")
    )
  )

