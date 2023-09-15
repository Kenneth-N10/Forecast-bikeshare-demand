---
title: "Forecast daily bike rental demand using time series models"
date: "`r Sys.Date()`"
output: html_document
author: "Kenneth Paul Nodado"
---

# About Data Analysis Report

This RMarkdown file contains the report of the data analysis done for the project on forecasting daily bike rental demand using time series models in R. It contains analysis such as data exploration, summary statistics and building the time series models. The final report was completed on `r date()`.

**Data Description:**

This dataset contains the daily count of rental bike transactions between years 2011 and 2012 in Capital bikeshare system with the corresponding weather and seasonal information.

**Data Source:** <https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset>

**Relevant Paper:**

Fanaee-T, Hadi, and Gama, Joao. Event labeling combining ensemble detectors and background knowledge, Progress in Artificial Intelligence (2013): pp. 1-15, Springer Berlin Heidelberg

# Task One: Load and explore the data

## Load data and install packages

```{r}
## Import required packages
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}
if(!require('lubridate')) {
  install.packages('lubridate')
  library('lubridate')
}
if(!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')
}
if(!require('timetk')) {
  install.packages('timetk')
  library('timetk')
}
if(!require('dbplyr')) {
  install.packages('dbplyr')
  library('dbplyr')
}
if(!require('tseries')) {
  install.packages('tseries')
  library('tseries')
}
if(!require('forecast')) {
  install.packages('forecast')
  library('forecast')
}
```

```{r}
Data_day <- read.csv("day.csv")
Data_hour <- read.csv("hour.csv")
```

## Describe and explore the data

```{r}
Data_day <- mutate(Data_day,dteday = as.Date(dteday))
Data_day$ncnt <- Data_day$cnt / max(Data_day$cnt)
Data_day$nr <- Data_day$registered / max(Data_day$registered)
Data_day$rr <- Data_day$cnt / max(Data_day$registered)
summary(Data_day)
```

# Task Two: Create interactive time series plots

```{r}
Data_day %>% 
  group_by(yr) %>% 
  plot_time_series(dteday
                   , temp
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "Temperature"
                   , .title = "Normalized Temperature vs Date"
                   , .interactive = TRUE)
```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , hum
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "Humidity"
                   , .title = "Normalized Humidity vs. Date"
                   , .interactive = TRUE)
```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , windspeed
                   , .color_var = season
                   , .x_lab = "Date" 
                   , .y_lab = "Windspeed"
                   , .title = "Normalized Windspeed vs Date" 
                   , .interactive = TRUE)
```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , ncnt
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "No. of Rental bikes including Casual and Registered"
                   , .title = "No. of Rental bikes including Casual and Registered vs. Date"
                   , .interactive = TRUE)
```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , nr
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "No. of Registered users"
                   , .title = "No. of Registered users vs. Date"
                   , .interactive = TRUE)

```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , rr
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "Ratio of Registered users"
                   , .title = "Ratio of Registered users vs. Date"
                   , .interactive = TRUE) 

```

# Task Three: Smooth time series data

```{r}
Data_day$temp <- tsclean(Data_day$temp)
Data_day$ncnt <- tsclean(Data_day$ncnt)
Data_day$nr <- tsclean(Data_day$nr)
Data_day$rr <- tsclean(Data_day$rr)
head(Data_day)
```

```{r}
Data_day %>% 
  group_by(yr) %>% 
  plot_time_series(dteday
                   , temp
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "Temperature"
                   , .title = "Normalized Temperature vs Date"
                   , .interactive = TRUE)
```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , hum
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "Humidity"
                   , .title = "Normalized Humidity vs. Date"
                   , .interactive = TRUE)
```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , windspeed
                   , .color_var = season
                   , .x_lab = "Date" 
                   , .y_lab = "Windspeed"
                   , .title = "Normalized Windspeed vs Date" 
                   , .interactive = TRUE)
```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , ncnt
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "No. of Rental bikes including Casual and Registered"
                   , .title = "No. of Rental bikes including Casual and Registered vs. Date"
                   , .interactive = TRUE)
```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , nr
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "No. of Registered users"
                   , .title = "No. of Registered users vs. Date"
                   , .interactive = TRUE)
```

```{r}
Data_day %>%
  group_by(yr) %>%
  plot_time_series(dteday
                   , rr
                   , .color_var = season
                   , .x_lab = "Date"
                   , .y_lab = "Ratio of Registered users"
                   , .title = "Ratio of Registered users vs. Date"
                   , .interactive = TRUE) 
```

# Task Four: Decompose and assess the stationarity of time series data

```{r}
Data_day$temp %>% adf.test()
Data_day$hum %>% adf.test()
Data_day$windspeed %>% adf.test()
Data_day$ncnt %>% adf.test()
Data_day$nr %>% adf.test()
Data_day$rr %>% adf.test()
```

```{r}
freq <- 365
```

```{r}
norm_rentals <- ts(Data_day$nr, frequency = freq)
decompd <- stl(norm_rentals, "periodic")
plot(decompd$time.series[,2], ylab = "Stationary of the Normalized Rental Reservations", 
     xlab = "Day of the Year")
checkresiduals(decompd$time.series[, 3])
```

```{r}
norm_cnt <- ts(Data_day$ncnt, frequency = freq)
decompd2 <- stl(norm_cnt, "periodic")
plot(decompd2$time.series[,2], ylab = "Stationary of the Normalized Rental Counts", 
     xlab = "Day of the Year")
checkresiduals(decompd2$time.series[, 3])
```

```{r}
norm_rr <- ts(Data_day$rr, frequency = freq)
decompd3 <- stl(norm_rr, "periodic")
plot(decompd3$time.series[,2], ylab = "Stationary of the Normalized Rental Counts to Reservations", 
     xlab = "Day of the Year")
checkresiduals(decompd3$time.series[, 3])
```

```{r}
shapiro.test(decompd$time.series[, 3])
shapiro.test(decompd2$time.series[, 3])
shapiro.test(decompd3$time.series[, 3])
```

# Task Five: Fit and forecast time series data using ARIMA models

```{r}
fit1 <- auto.arima(norm_cnt, seasonal = TRUE, )
hist(fit1$residuals, xlab = "Residual", ylab = "Distribution", main = "Histogram of Model Errors - Bike Count")
shapiro.test(fit1$residuals)
prediction1 <- forecast(fit1, 25)
plot(prediction1, xlab = "Date", ylab = "Normalized Count of Rentals", main = "Prediction of Bike Rental Counts")
```

```{r}
fit2 <- auto.arima(norm_rentals, seasonal = TRUE, )
hist(fit2$residuals, xlab = "Residual", ylab = "Distribution", main = "Histogram of Model Errors - Rental Count")
shapiro.test(fit2$residuals)
prediction2 <- forecast(fit2, 25)
plot(prediction2, xlab = "Date", ylab = "Normalized Rentals", main = "Prediction of Bike Rentals")
```

```{r}
fit3 <- auto.arima(norm_cnt, seasonal = TRUE, )
hist(fit3$residuals, xlab = "Residual", ylab = "Distribution", main = "Histogram of Model Errors - Count to Rental Ratio")
shapiro.test(fit3$residuals)
prediction3 <- forecast(fit3, 25)
plot(prediction3, xlab = "Date", ylab = "Normalized Rental Ratio", main = "Prediction of Bike Rentals to Reservations")
```

# Task Six: Findings and Conclusions

After processing the raw data and employing the ARIMA package to model ride share data, I successfully generated predictions for the upcoming 25 days beyond the current dataset. In a qualitative assessment of the data, it becomes evident that as the weather becomes warmer, there is a corresponding increase in the number of bike rentals. Moreover, when considering a two-year timeframe, it is apparent that the number of rentals steadily rises compared to the previous year.

Given that the data cycle concludes at the end of a year, it is reasonable to anticipate that the rental numbers will surge to a level surpassing the figures from the preceding year. This aligns with the projections made by the models. Consequently, the outcomes align with my expectations: the data exhibits a recurring annual pattern of fluctuations with an overall trend toward higher rental numbers.
