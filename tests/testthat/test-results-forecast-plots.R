# FORECAST PLOTS -----
context("TEST MODELTIME PLOTS")

# SETUP ----

# Data
m750   <- m4_monthly %>% filter(id == "M750")
splits <- initial_time_split(m750, prop = 0.8)



test_that("modeltime plotting", {

    testthat::skip_on_cran()

    # SETUP

    # Model Spec
    model_spec <- arima_reg(seasonal_period = 12) %>%
        set_engine("auto_arima")

    # PARSNIP INTERFACE ----

    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = training(splits))

    # * Forecast ----
    forecast_tbl <- model_fit %>%
        modeltime_calibrate(new_data = testing(splits)) %>%
        modeltime_forecast(
            actual_data = m750,
            conf_interval = 0.95)

    # VISUALIZATIONS WITH CONF INTERVALS ----

    # * ggplot2 visualization ----
    g <- forecast_tbl %>%
        mutate_at(vars(.value:.conf_hi), exp) %>%
        plot_modeltime_forecast(.interactive = FALSE)

    # * plotly visualization ----
    suppressWarnings({
        # Needed until plotly is resolved: https://github.com/ropensci/plotly/issues/1783
        p <- forecast_tbl %>%
            mutate_at(vars(.value:.conf_hi), exp) %>%
            plot_modeltime_forecast(.interactive = TRUE)
    })

    # "modeltime plot, Test Static ggplot

    # Structure
    testthat::expect_s3_class(g, "ggplot")
    testthat::expect_s3_class(g$layers[[1]]$geom, "GeomRibbon")


    # modeltime plot, Test Interactive plotly

    # Structure
    testthat::expect_s3_class(p, "plotly")


    # # PLOTS WITHOUT CONF INTERVALS -----

    g <- forecast_tbl %>%
        mutate_at(vars(.value:.conf_hi), exp) %>%
        plot_modeltime_forecast(.interactive = FALSE, .conf_interval_show = FALSE)


    p <- forecast_tbl %>%
        mutate_at(vars(.value:.conf_hi), exp) %>%
        plot_modeltime_forecast(.interactive = TRUE, .conf_interval_show = FALSE)

    # Structure
    testthat::expect_s3_class(g, "ggplot")
    testthat::expect_s3_class(g$layers[[1]]$geom, "GeomLine")


    # Structure
    testthat::expect_s3_class(p, "plotly")

})

# WORKFLOW INTERFACE ----

test_that("modeltime plot - workflow, Test Static ggplot", {

    testthat::skip_on_cran()

    # SETUP

    # Model Spec
    model_spec <- arima_reg(seasonal_period = 12) %>%
        set_engine("auto_arima")

    # Recipe spec
    recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
        step_log(value, skip = FALSE)

    # Workflow
    wflw <- workflow() %>%
        add_recipe(recipe_spec) %>%
        add_model(model_spec)

    wflw_fit <- wflw %>%
        fit(training(splits))

    forecast_tbl <- wflw_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(actual_data = m750, conf_interval = 0.8)

    # * ggplot2 visualization ----
    g <- forecast_tbl %>%
        mutate_at(vars(.value:.conf_hi), exp) %>%
        plot_modeltime_forecast(.conf_interval_show = TRUE, .interactive = FALSE)

    # * plotly visualization ----
    p <- forecast_tbl %>%
        mutate_at(vars(.value:.conf_hi), exp) %>%
        plot_modeltime_forecast(.conf_interval_show = TRUE, .interactive = TRUE)


    # Structure
    testthat::expect_s3_class(g, "ggplot")
    testthat::expect_s3_class(g$layers[[1]]$geom, "GeomRibbon")


    # Structure
    testthat::expect_s3_class(p, "plotly")

})
