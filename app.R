
# ============================================================
# ADS 506 FINAL PROJECT
# NYC MOTOR VEHICLE COLLISION FORECASTING DASHBOARD
#
# Forecasting Models:
# 1. Trend TSLM
# 2. Weekly TSLM
# 3. Full TSLM
# 4. ETS
# 5. ARIMA
#
# ============================================================


# ============================================================
# 1. LOAD LIBRARIES
# ============================================================

library(shiny)
library(fpp3)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)
library(bslib)

# Needed by ARIMA's unit root (KPSS) tests; loading it makes sure
# deployment installs it, since fable/feasts only suggest it.
library(urca)


# ============================================================
# 2. LOAD NYC COLLISION DATA
# ============================================================

data_file <- file.path("data", "daily_borough_counts.csv")

if (!file.exists(data_file)) {
  stop(
    "Cannot find ", data_file, ". Run prep_data.R to create it from the raw collision CSV."
  )
}

# Pre-aggregated by prep_data.R: one row per borough and day with collisions
daily_data <- readr::read_csv(
  data_file,
  col_types = cols(
    borough = col_character(),
    day = col_date(),
    crashes = col_integer()
  )
) |>
  
  # Days with no collisions are absent from the file; fill with zero
  complete(
    borough,
    day = seq(min(day), max(day), by = "day"),
    fill = list(crashes = 0)
  ) |>
  
  arrange(borough, day)

borough_choices <- sort(unique(daily_data$borough))

default_boroughs <- setdiff(borough_choices, "Unknown")


# ============================================================
# 4. CREATE TREND AND SEASONAL FEATURES
# ============================================================

make_features <- function(dates) {
  
  dates <- as.Date(dates)
  
  annual_position <- as.numeric(
    dates - as.Date("2021-01-01")
  )
  
  result <- tibble(
    
    day = dates,
    
    trend = annual_position + 1,
    
    weekday = factor(
      lubridate::wday(dates),
      levels = 1:7,
      labels = c(
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      )
    )
  )
  
  # Annual Fourier seasonality
  
  for (k in 1:5) {
    
    result[[paste0("sin", k)]] <-
      sin(
        2 * pi * k * annual_position / 365.25
      )
    
    result[[paste0("cos", k)]] <-
      cos(
        2 * pi * k * annual_position / 365.25
      )
  }
  
  result
}


daily_data <- daily_data |>
  
  left_join(
    make_features(unique(daily_data$day)),
    by = "day"
  ) |>
  
  as_tsibble(key = borough, index = day)


# ============================================================
# 5. DATASET INFORMATION
# ============================================================

min_day <- min(daily_data$day)

max_day <- max(daily_data$day)

default_train_end <- min(
  as.Date("2024-12-31"),
  max_day - 365
)

default_validation_end <- min(
  as.Date("2025-12-31"),
  max_day
)


# ============================================================
# 6. FORECASTING FUNCTION
# ============================================================

model_specs <- list(
  Trend_TSLM = TSLM(crashes ~ trend()),
  Weekly_TSLM = TSLM(crashes ~ trend() + season()),
  Full_TSLM = TSLM(
    crashes ~ trend() + season() +
      sin1 + cos1 + sin2 + cos2 + sin3 + cos3 +
      sin4 + cos4 + sin5 + cos5
  ),
  ETS = ETS(crashes),
  # Restricted order search with approximation: faster and lighter on memory
  ARIMA = ARIMA(
    crashes ~ pdq(0:2, 0:1, 0:2) + PDQ(0:1, 0, 0:1),
    stepwise = TRUE,
    approximation = TRUE
  )
)

# Simpler specs tried if the primary spec fails for any series
model_fallbacks <- list(
  ARIMA = list(
    "weekday regressors with a non-seasonal ARIMA search" =
      ARIMA(
        crashes ~ season() + pdq(0:2, 0:1, 0:2) + PDQ(0, 0, 0),
        stepwise = TRUE,
        approximation = TRUE
      ),
    "a fixed ARIMA(1,1,1)" =
      ARIMA(crashes ~ pdq(1, 1, 1) + PDQ(0, 0, 0))
  )
)

# Fits are cached by model and data window, so re-running with the
# same dates (e.g., a different horizon) does not refit.
fit_cache <- new.env()

failed_series <- function(fit, m) {
  vapply(fit[[m]], \(x) inherits(x$fit, "null_mdl"), logical(1))
}

# Fit one model for every series. Failures are written to the server log
# (including fable's own warning text), retried with a fallback spec, and
# returned as an "issues" string for display in the app.
fit_one_model <- function(data, m) {
  
  warns <- character()
  
  fit <- withCallingHandlers(
    model(data, !!m := model_specs[[m]]),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  issues <- character()
  failed <- failed_series(fit, m)
  
  if (any(failed)) {
    message(
      "Model ", m, " failed for: ",
      paste(fit$borough[failed], collapse = ", "),
      " | fable messages: ", paste(warns, collapse = " || ")
    )
    
    primary_failed <- fit$borough[failed]
    
    for (nm in names(model_fallbacks[[m]])) {
      
      if (!any(failed)) break
      
      fit <- suppressWarnings(
        model(data, !!m := model_fallbacks[[m]][[nm]])
      )
      failed <- failed_series(fit, m)
      
      if (!any(failed)) {
        issues <- paste0(
          m, ": the primary specification failed on this server; used ", nm, "."
        )
      } else {
        message("Fallback (", nm, ") for ", m, " failed for: ",
                paste(fit$borough[failed], collapse = ", "))
      }
    }
    
    if (any(failed)) {
      message("Fallback for ", m, " also failed for: ",
              paste(fit$borough[failed], collapse = ", "))
      issues <- paste0(
        m, " could not be fit for: ",
        paste(fit$borough[failed], collapse = ", "), "."
      )
    }
  }
  
  list(fit = fit, issues = issues)
}

fit_models <- function(data, models = names(model_specs)) {
  
  window <- paste(
    min(data$day), max(data$day),
    paste(sort(unique(data$borough)), collapse = ",")
  )
  
  for (m in models) {
    key <- paste(m, window)
    if (is.null(fit_cache[[key]])) {
      fit_cache[[key]] <- fit_one_model(data, m)
    }
  }
  
  entries <- lapply(models, \(m) fit_cache[[paste(m, window)]])
  
  result <- Reduce(
    \(a, b) dplyr::left_join(a, b, by = "borough"),
    lapply(entries, \(e) e$fit)
  )
  
  attr(result, "issues") <- unlist(lapply(entries, \(e) e$issues))
  
  result
}

fc_bounds <- function(fc, levels) {
  
  h <- fc |>
    hilo(level = levels) |>
    as_tibble()
  
  bind_rows(lapply(levels, \(lv) {
    x <- h[[paste0(lv, "%")]]
    tibble(
      borough = h$borough,
      .model = h$.model,
      day = h$day,
      level = paste0(lv, "%"),
      lower = x$lower,
      upper = x$upper
    )
  }))
}

# Forecast lines for every model; interval ribbons for one model;
# actuals drawn as a thin grey line underneath.
plot_forecast <- function(fc, actual, levels, ribbon_model, title) {
  
  fc_tbl <- as_tibble(fc)
  
  if (is.null(ribbon_model) || !ribbon_model %in% fc_tbl$.model) {
    ribbon_model <- fc_tbl$.model[1]
  }
  
  bounds <- fc_bounds(fc, levels) |>
    filter(.model == ribbon_model)
  
  ggplot() +
    geom_line(
      data = actual,
      aes(x = day, y = crashes),
      color = "grey55",
      linewidth = 0.3
    ) +
    geom_ribbon(
      data = bounds,
      aes(
        x = day, ymin = lower, ymax = upper,
        fill = .model, group = level, alpha = level
      )
    ) +
    geom_line(
      data = fc_tbl,
      aes(x = day, y = .mean, color = .model),
      linewidth = 0.7
    ) +
    scale_alpha_manual(
      values = c("80%" = 0.35, "95%" = 0.18),
      name = "Interval"
    ) +
    facet_wrap(vars(borough), scales = "free_y", ncol = 1) +
    labs(
      title = title,
      subtitle = paste0("Grey line = actual collisions; shaded band = prediction interval for ", ribbon_model),
      x = "Date",
      y = "Daily Collisions",
      color = "Model",
      fill = "Model"
    ) +
    theme_minimal()
}


# ============================================================
# 7. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#245A81"
  ),
  
  titlePanel(
    "NYC Motor Vehicle Collision Forecasting"
  ),
  
  p(
    "ADS 506 | Interactive Time Series Analysis",
    style = "color: #64748b;"
  ),
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      width = 3,
      
      h4("Analysis Controls"),
      
      dateRangeInput(
        "date_range",
        "Historical Date Range",
        
        start = as.Date("2021-01-01"),
        end = max_day,
        
        min = as.Date("2021-01-01"),
        max = max_day
      ),
      
      selectInput(
        "boroughs",
        "Boroughs",
        
        choices = borough_choices,
        selected = default_boroughs,
        multiple = TRUE
      ),
      
      dateInput(
        "train_end",
        "Training End Date",
        
        value = default_train_end,
        
        min = as.Date("2021-12-31"),
        max = max_day - 1
      ),
      
      dateInput(
        "validation_end",
        "Validation End Date",
        
        value = default_validation_end,
        
        min = default_train_end + 1,
        max = max_day
      ),
      
      selectInput(
        "models",
        "Select Forecasting Models",
        
        choices = c(
          "Trend TSLM" = "Trend_TSLM",
          "Weekly TSLM" = "Weekly_TSLM",
          "Full TSLM" = "Full_TSLM",
          "ETS" = "ETS",
          "ARIMA" = "ARIMA"
        ),
        
        selected = c(
          "Trend_TSLM",
          "Weekly_TSLM",
          "Full_TSLM",
          "ETS",
          "ARIMA"
        ),
        
        multiple = TRUE
      ),
      
      sliderInput(
        "horizon",
        "Future Forecast Horizon (Days)",
        
        min = 7,
        max = 365,
        value = 90,
        step = 7
      ),
      
      selectInput(
        "interval",
        "Prediction Interval",
        
        choices = c(
          "80% and 95%" = "both",
          "80%" = "80",
          "95%" = "95"
        ),
        
        selected = "both"
      ),
      
      actionButton(
        "run",
        "Run Forecast Analysis",
        
        class = "btn-primary",
        width = "100%"
      ),
      
      br(),
      br(),
      
      helpText(
        "Training begins January 1, 2021. Change the training cutoff and validation period to evaluate different forecasting scenarios."
      )
    ),
    
    
    # ========================================================
    # MAIN DASHBOARD
    # ========================================================
    
    mainPanel(
      
      width = 9,
      
      uiOutput("model_notice"),
      
      tabsetPanel(
        
        # ====================================================
        # TAB 1: OVERVIEW
        # ====================================================
        
        tabPanel(
          "Overview",
          
          br(),
          
          fluidRow(
            
            column(
              4,
              
              wellPanel(
                h5("Total Observations"),
                h3(textOutput("total_days"))
              )
            ),
            
            column(
              4,
              
              wellPanel(
                h5("Average Daily Collisions"),
                h3(textOutput("average_crashes"))
              )
            ),
            
            column(
              4,
              
              wellPanel(
                h5("Maximum Daily Collisions"),
                h3(textOutput("maximum_crashes"))
              )
            )
          ),
          
          h4("Historical Daily Collision Trends"),
          
          plotlyOutput(
            "historical_plot",
            height = "420px"
          ),
          
          br(),
          
          h4("Collision Summary"),
          
          DTOutput("summary_table")
        ),
        
        
        # ====================================================
        # TAB 2: FORECASTS
        # ====================================================
        
        tabPanel(
          "Forecasts",
          
          br(),
          
          h4("Validation Forecasts"),
          
          p(
            "Comparison of actual daily collisions and predicted values during the selected validation period."
          ),
          
          selectInput(
            "ribbon_model",
            "Show prediction intervals for",
            
            choices = c(
              "Trend TSLM" = "Trend_TSLM",
              "Weekly TSLM" = "Weekly_TSLM",
              "Full TSLM" = "Full_TSLM",
              "ETS" = "ETS",
              "ARIMA" = "ARIMA"
            ),
            
            selected = "Full_TSLM"
          ),
          
          plotOutput(
            "validation_plot",
            height = "500px"
          ),
          
          tags$hr(),
          
          h4("Future Forecasts"),
          
          p(
            "Forecasts beyond the latest available historical observation."
          ),
          
          plotOutput(
            "future_plot",
            height = "500px"
          ),
          
          br(),
          
          h4("Future Forecast Values"),
          
          DTOutput("future_table")
        ),
        
        
        # ====================================================
        # TAB 3: MODEL ACCURACY
        # ====================================================
        
        tabPanel(
          "Model Accuracy",
          
          br(),
          
          h4("Forecast Accuracy Comparison"),
          
          p(
            "Models are evaluated using the same validation period."
          ),
          
          DTOutput("accuracy_table"),
          
          br(),
          
          h4("RMSE Comparison"),
          
          plotOutput(
            "accuracy_plot",
            height = "400px"
          ),
          
          br(),
          
          h4("Best Performing Model"),
          
          verbatimTextOutput("best_model")
        ),
        
        
        # ====================================================
        # TAB 4: DIAGNOSTICS
        # ====================================================
        
        tabPanel(
          "Diagnostics",
          
          br(),
          
          h4("Residual Diagnostics"),
          
          selectInput(
            "diagnostic_model",
            "Select Model",
            
            choices = c(
              "Trend_TSLM",
              "Weekly_TSLM",
              "Full_TSLM",
              "ETS",
              "ARIMA"
            ),
            
            selected = "Full_TSLM"
          ),
          
          selectInput(
            "diagnostic_borough",
            "Select Borough",
            
            choices = borough_choices,
            selected = borough_choices[1]
          ),
          
          plotOutput(
            "diagnostic_plot",
            height = "650px"
          ),
          
          p(
            "Residual plots help identify remaining temporal patterns and assess model assumptions."
          )
        ),
        
        
        # ====================================================
        # TAB 5: DATA EXPLORER
        # ====================================================
        
        tabPanel(
          "Data Explorer",
          
          br(),
          
          h4("Daily Collision Data"),
          
          DTOutput("data_table")
        ),
        
        
        # ====================================================
        # TAB 6: ABOUT
        # ====================================================
        
        tabPanel(
          "About",
          
          br(),
          
          h3("About This Application"),
          
          p(
            "This application analyzes and forecasts daily motor vehicle collisions in New York City using historical collision records."
          ),
          
          h4("Data Source"),
          
          p(
            "NYC Open Data - Motor Vehicle Collisions - Crashes."
          ),
          
          tags$a(
            href = "https://data.cityofnewyork.us/",
            target = "_blank",
            "NYC Open Data Portal"
          ),
          
          br(),
          br(),
          
          h4("Data Preparation"),
          
          p(
            "Individual collision records are aggregated into daily collision counts for each borough (records with no borough are grouped as Unknown). Missing calendar dates are assigned zero collisions so that each series has a continuous daily index. Each borough is modeled separately, using data from 2021-01-01 onward."
          ),
          
          h4("Forecasting Models"),
          
          tags$ul(
            
            tags$li(
              strong("Trend TSLM: "),
              "Linear time trend."
            ),
            
            tags$li(
              strong("Weekly TSLM: "),
              "Linear trend and weekday effects."
            ),
            
            tags$li(
              strong("Full TSLM: "),
              "Linear trend, weekday effects, and annual Fourier seasonality."
            ),
            
            tags$li(
              strong("ETS: "),
              "Exponential smoothing."
            ),
            
            tags$li(
              strong("ARIMA: "),
              "Autoregressive Integrated Moving Average."
            )
          ),
          
          h4("Model Evaluation"),
          
          p(
            "Forecast accuracy is evaluated using a separate validation period."
          ),
          
          tags$ul(
            
            tags$li(
              "RMSE: Root Mean Squared Error"
            ),
            
            tags$li(
              "MAE: Mean Absolute Error"
            ),
            
            tags$li(
              "MAPE: Mean Absolute Percentage Error"
            ),
            
            tags$li(
              "ACF1: First-order residual autocorrelation"
            )
          ),
          
          h4("Reproducing the Analysis"),
          
          tags$ol(
            
            tags$li(
              "Select the historical date range."
            ),
            
            tags$li(
              "Choose the training cutoff date."
            ),
            
            tags$li(
              "Choose the validation end date."
            ),
            
            tags$li(
              "Select forecasting models."
            ),
            
            tags$li(
              "Choose the future forecast horizon."
            ),
            
            tags$li(
              "Click Run Forecast Analysis."
            ),
            
            tags$li(
              "Review forecasts, prediction intervals, accuracy metrics, and diagnostics."
            )
          ),
          
          h4("Software"),
          
          p(
            "R, Shiny, fpp3, fable, tsibble, tidyverse, ggplot2, plotly, and DT."
          ),
          
          h4("Course"),
          
          p(
            "ADS 506 - Module 5: Build and Deploy a Shiny App."
          )
        )
      )
    )
  )
)


# ============================================================
# 8. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # UPDATE VALIDATION DATE
  # ----------------------------------------------------------
  
  observeEvent(
    input$train_end,
    {
      
      updateDateInput(
        session,
        "validation_end",
        
        min = input$train_end + 1,
        
        value = min(
          max_day,
          input$train_end + 365
        )
      )
      
    },
    ignoreInit = TRUE
  )
  
  
  # ----------------------------------------------------------
  # HISTORICAL DATA FILTER
  # ----------------------------------------------------------
  
  selected_data <- reactive({
    
    req(input$date_range, input$boroughs)
    
    daily_data |>
      
      filter(
        borough %in% input$boroughs,
        day >= input$date_range[1],
        day <= input$date_range[2]
      )
  })
  
  
  # Daily totals across the selected boroughs
  daily_total <- reactive({
    
    selected_data() |>
      as_tibble() |>
      summarise(crashes = sum(crashes), .by = day)
  })
  
  
  # ----------------------------------------------------------
  # OVERVIEW OUTPUTS
  # ----------------------------------------------------------
  
  output$total_days <- renderText({
    
    format(
      nrow(daily_total()),
      big.mark = ","
    )
  })
  
  
  output$average_crashes <- renderText({
    
    round(
      mean(daily_total()$crashes),
      2
    )
  })
  
  
  output$maximum_crashes <- renderText({
    
    max(daily_total()$crashes)
  })
  
  
  output$historical_plot <- renderPlotly({
    
    p <- ggplot(
      selected_data(),
      aes(
        x = day,
        y = crashes,
        color = borough
      )
    ) +
      
      geom_line(
        linewidth = 0.35
      ) +
      
      labs(
        title = "NYC Daily Motor Vehicle Collisions",
        x = "Date",
        y = "Daily Collisions"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$summary_table <- renderDT({
    
    selected_data() |>
      
      as_tibble() |>
      
      summarise(
        
        Total_Collisions = sum(crashes),
        
        Average_Daily_Collisions =
          round(mean(crashes), 2),
        
        Median_Daily_Collisions =
          median(crashes),
        
        Maximum_Daily_Collisions =
          max(crashes),
        
        .by = borough
      ) |>
      
      datatable(
        options = list(
          dom = "t",
          scrollX = TRUE
        )
      )
  })
  
  
  # ----------------------------------------------------------
  # FORECASTING (runs when the button is clicked)
  # ----------------------------------------------------------
  
  results <- eventReactive(
    input$run,
    {
      
      req(input$models, input$train_end, input$validation_end)
      
      train_end <- as.Date(input$train_end)
      validation_end <- as.Date(input$validation_end)
      
      shiny::validate(
        need(
          validation_end > train_end,
          "Validation end date must be after the training end date."
        )
      )
      
      hist_data <- selected_data()
      
      train <- hist_data |>
        filter(day <= train_end)
      
      valid <- hist_data |>
        filter(day > train_end, day <= validation_end)
      
      shiny::validate(
        need(nrow(train) > 400, "Not enough training data. Widen the date range or move the training end date later."),
        need(nrow(valid) > 0, "No validation data in the selected range.")
      )
      
      withProgress(message = "Fitting models...", value = 0.2, {
        
        fit_train <- fit_models(train, input$models)
        issues_train <- attr(fit_train, "issues")
        
        fc_valid <- forecast(fit_train, new_data = valid)
        
        acc <- bind_rows(
          accuracy(fit_train) |> mutate(Set = "Training"),
          accuracy(fc_valid, valid) |> mutate(Set = "Validation")
        ) |>
          as_tibble() |>
          select(any_of(c("borough", ".model", "Set", "RMSE", "MAE", "MAPE", "ACF1"))) |>
          mutate(across(where(is.numeric), \(x) round(x, 3))) |>
          arrange(borough, Set, RMSE)
        
        setProgress(0.6, message = "Forecasting future...")
        
        fit_full <- fit_models(hist_data, input$models)
        issues_full <- attr(fit_full, "issues")
        
        future_dates <- seq(
          max(hist_data$day) + 1,
          by = "day",
          length.out = input$horizon
        )
        
        future_data <- tidyr::crossing(
          borough = unique(hist_data$borough),
          make_features(future_dates)
        ) |>
          as_tsibble(key = borough, index = day)
        
        fc_future <- forecast(fit_full, new_data = future_data)
        
        list(
          hist = hist_data,
          valid = valid,
          fit_train = fit_train,
          fc_valid = fc_valid,
          acc = acc,
          fc_future = fc_future,
          issues = unique(c(issues_train, issues_full))
        )
      })
    },
    ignoreNULL = FALSE
  )
  
  
  output$model_notice <- renderUI({
    
    issues <- results()$issues
    
    if (length(issues) > 0) {
      div(
        class = "alert alert-warning",
        strong("Model notice: "),
        paste(issues, collapse = " ")
      )
    }
  })
  
  
  interval_levels <- reactive({
    switch(
      input$interval,
      both = c(80, 95),
      "80" = 80,
      "95" = 95
    )
  })
  
  
  output$validation_plot <- renderPlot({
    
    res <- results()
    
    plot_forecast(
      res$fc_valid,
      res$valid,
      interval_levels(),
      input$ribbon_model,
      "Validation Period: Actual vs Forecast"
    )
  })
  
  
  output$future_plot <- renderPlot({
    
    res <- results()
    
    plot_forecast(
      res$fc_future,
      res$hist |> filter(day > max(day) - 365),
      interval_levels(),
      input$ribbon_model,
      "Future Collision Forecasts"
    )
  })
  
  
  output$future_table <- renderDT({
    
    fc <- results()$fc_future
    lv <- interval_levels()
    
    wide <- fc_bounds(fc, lv) |>
      pivot_wider(
        names_from = level,
        values_from = c(lower, upper),
        names_glue = "{level} {str_to_title(.value)}"
      )
    
    fc |>
      as_tibble() |>
      select(borough, .model, day, .mean) |>
      left_join(wide, by = c("borough", ".model", "day")) |>
      rename(Borough = borough, Model = .model, Date = day, Forecast = .mean) |>
      mutate(across(where(is.numeric), \(x) round(x, 1))) |>
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
  })
  
  
  output$accuracy_table <- renderDT({
    
    datatable(
      results()$acc,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
    )
  })
  
  
  output$accuracy_plot <- renderPlot({
    
    results()$acc |>
      filter(Set == "Validation") |>
      ggplot(aes(x = RMSE, y = reorder(.model, -RMSE))) +
      geom_col() +
      facet_wrap(vars(borough), scales = "free_x") +
      labs(x = "Validation RMSE", y = "Model") +
      theme_minimal()
  })
  
  
  output$best_model <- renderText({
    
    best <- results()$acc |>
      filter(Set == "Validation") |>
      slice_min(RMSE, n = 1, by = borough)
    
    paste0(
      "Best model by validation RMSE\n",
      paste0(
        best$borough, ": ", best$.model,
        " (RMSE ", best$RMSE, ", MAE ", best$MAE,
        ", MAPE ", best$MAPE, ")",
        collapse = "\n"
      )
    )
  })
  
  
  output$diagnostic_plot <- renderPlot({
    
    res <- results()
    
    shiny::validate(
      need(
        input$diagnostic_model %in% names(res$fit_train),
        "Selected model was not included in the forecast run."
      )
    )
    
    shiny::validate(
      need(
        input$diagnostic_borough %in% res$fit_train$borough,
        "Selected borough was not included in the forecast run."
      )
    )
    
    res$fit_train |>
      filter(borough == input$diagnostic_borough) |>
      select(all_of(input$diagnostic_model)) |>
      gg_tsresiduals()
  })
  
  
  output$data_table <- renderDT({
    
    selected_data() |>
      as_tibble() |>
      select(borough, day, crashes) |>
      datatable(
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })
}


shinyApp(ui, server)
