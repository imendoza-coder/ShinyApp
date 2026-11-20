# NYC Motor Vehicle Collision Forecasting (Shiny)

Interactive dashboard that forecasts daily NYC motor vehicle collisions by borough
using Trend TSLM, Weekly TSLM, Full TSLM, ETS, and ARIMA models (fpp3/fable).

## Files

| File | Purpose |
|---|---|
| `app.R` | The Shiny app. |
| `prep_data.R` | Aggregates the raw collision CSV into daily counts by borough. |
| `data/daily_borough_counts.csv` | Output of `prep_data.R` (borough, day, crashes); the only data file the app reads. |

The raw NYC Open Data "Motor Vehicle Collisions - Crashes" CSV is not tracked in git
(see `.gitignore`) and is not needed to run or deploy the app.

## Preparing the data

Only needed when the raw CSV is new or updated:

1. Place the raw CSV in `data/` (default name `Motor_Vehicle_Collisions_-_Crashes_20261114.csv`;
   edit `raw_file` in `prep_data.R` if yours differs).
2. From this folder, run `source("prep_data.R")`.

This keeps records from 2021-01-01 onward, labels missing boroughs as `Unknown`, and
writes `data/daily_borough_counts.csv`. The app fills days with no collisions with zero.

## Running locally

```r
shiny::runApp()
```

Required packages: shiny, fpp3, tidyverse, lubridate, DT, plotly, bslib.

## Deploying

Deploy only the app and the small data file, not the raw CSVs:

```r
rsconnect::deployApp(
  appFiles = c("app.R", "data/daily_borough_counts.csv")
)
```

This requires an account on shinyapps.io or Posit Connect, configured with
`rsconnect::setAccountInfo()`. Models are fit when the user clicks
"Run Forecast Analysis", so a small hosting instance may take a while with
ARIMA and many boroughs selected.
