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
