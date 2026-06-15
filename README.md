# Modelling Stock Market Volatility: A GARCH Approach for the DAX Index

---

## Overview

This project investigates the volatility of the **DAX Index** (Deutscher Aktien Index) using a family of GARCH-type econometric models. The goal is to compare different model specifications in their ability to forecast daily log-return volatility for December 2022, based on historical data from January 2008 to November 2022 (3,804 trading days).

The four models compared are:
- **GARCH(1,1)** — Standard Generalized Autoregressive Conditional Heteroskedasticity
- **GARCH-M(1,1)** — GARCH in Mean (tests for the presence of a risk premium)
- **eGARCH(1,1)** — Exponential GARCH (captures asymmetry in volatility)
- **GJR-GARCH(1,1)** — Glosten-Jagannathan-Runkle GARCH (models the leverage effect)

---

## Repository Structure

```
.
├── runProject.R                        # Main R script: full analysis pipeline
├── DAX.csv                             # Daily adjusted closing prices of the DAX Index (Jan 2008 – Dec 2022)
├── IXIC.csv                            # NASDAQ Composite data (supplementary)
├── NIKKEI225.csv                       # Nikkei 225 data (supplementary)
├── Econometrics_Project_Report.pdf     # Full project report with methodology, results and references
└── README.md
```

---

## Methodology

### 1. Data Preparation
- Daily adjusted closing prices of the DAX are loaded and cleaned.
- Log-returns are computed as $r_t = \log(S_t / S_{t-1})$.
- The dataset is split into a **training set** (up to 30 Nov 2022) and a **test set** (December 2022).

### 2. Exploratory Data Analysis
- **Descriptive statistics**: mean, median, standard deviation, skewness, excess kurtosis.
- **Normality tests**: Shapiro-Wilk, Anderson-Darling, Kolmogorov-Smirnov, Jarque-Bera.
- **Stationarity**: Augmented Dickey-Fuller (ADF) test.
- **Structural breaks**: PELT method via the `changepoint` package.
- **Heteroskedasticity**: Bartlett test across variance-homogeneous segments.
- **Autocorrelation**: ACF and PACF plots of log-returns and squared log-returns.
- **ARCH effect**: Engle's ARCH LM test at lags 1, 2, and 3.

### 3. Model Fitting
Each of the four GARCH variants is estimated on the training set using the `rugarch` package. Residual diagnostics include:
- Gaussianity of standardized residuals (QQ-plot, density vs. standard normal, Jarque-Bera).
- Residual autocorrelation via the Ljung-Box test.

### 4. Forecasting
A **rolling one-step-ahead forecast** is performed for each day in December 2022. At each step, all four models are re-estimated on the updated history, and the next-day volatility is forecasted. Forecast accuracy is evaluated using the **sum of absolute errors** between forecasted volatility and realized absolute log-returns.

---

## Requirements

### R version
R ≥ 4.0 recommended.

### Required packages
Install all dependencies by un-commenting the `install.packages(...)` lines at the top of `runProject.R`, or run:

```r
install.packages(c(
  "readr", "moments", "nortest", "ggplot2", "tseries",
  "rugarch", "fGarch", "FinTS", "dynlm", "vars",
  "nlWaldTest", "lmtest", "broom", "car", "sandwich",
  "knitr", "forecast", "tsbox", "zoo", "vrtest",
  "strucchange", "tidyverse", "lubridate", "changepoint"
))
```

---

## How to Run

1. Clone this repository.
2. Open `runProject.R` in RStudio (or any R environment).
3. Set the working directory to the folder containing the CSV files:
   ```r
   setwd("path/to/project/folder")
   ```
4. Run the script. Expected execution time: **~50 seconds**.

The script will automatically produce all plots and print test statistics to the console.

---

## Key Results

- Log-returns of the DAX exhibit **fat tails**, **volatility clustering**, and significant **ARCH effects**, motivating the use of GARCH-type models.
- All four models pass the Ljung-Box test (no residual autocorrelation), but standardized residuals retain non-Gaussianity (fat tails).
- One-month rolling forecasts for December 2022 are compared across models using absolute forecast errors.

---

## References

Key references from the econometrics literature used in this project:

- Engle, R.F. (1982). *Autoregressive Conditional Heteroskedasticity with Estimates of the Variance of United Kingdom Inflation*. Econometrica.
- Bollerslev, T. (1986). *Generalized Autoregressive Conditional Heteroskedasticity*. Journal of Econometrics.
- Nelson, D.B. (1991). *Conditional Heteroskedasticity in Asset Returns: A New Approach*. Econometrica.
- Glosten, L.R., Jagannathan, R., & Runkle, D.E. (1993). *On the Relation Between the Expected Value and the Volatility of the Nominal Excess Return on Stocks*. Journal of Finance.

---

## License

This project was developed for academic purposes as part of the Econometrics course (MSc in Mathematical Engineering, Academic Year 2022/2023). All data sourced from public financial databases.
