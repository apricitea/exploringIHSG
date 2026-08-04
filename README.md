# Exploring Time Series Data of IHSG (Indonesia Stock Exchange Composite Index)

Time series exploration, smoothing, and forecasting on daily IHSG data, in R.

## Contents

- `exploration.Rmd` — exploratory analysis of the daily IHSG series
- `smoothing.Rmd` — smoothing methods compared: single/double exponential smoothing,
  moving average, Holt-Winters (additive and multiplicative)
- `forecasting.Rmd` — SARIMA forecasting pipeline: stationarity checks (ADF), Box-Cox
  transformation, differencing, ACF-based model identification, model selection via AIC,
  and residual diagnostics (Ljung-Box, Breusch-Pagan, Jarque-Bera)
- `IHSG.csv` — the daily index data used throughout
- `exploration.html`, `smoothing.pdf` — rendered output for the exploration and
  smoothing notebooks (forecasting has no rendered top-level copy; see
  `coursework-archive/` below)
- `forecast/`, `formula/` — plot images embedded by `forecasting.Rmd` and
  `smoothing.Rmd` via relative paths; not standalone
- `scripts/code.R` — early exploratory smoothing script, kept for reference
- `scripts/try.R` — fetches raw IHSG data from Yahoo Finance and writes `IHSG.csv`
- `coursework-archive/` — the original university submission exports (including
  revision passes), superseded by the cleaned-up `.Rmd` sources above but kept
  since the two forecasting HTML exports here are the only rendered forecasting
  output currently checked in
- `exploringIHSG.Rproj` — RStudio project file
