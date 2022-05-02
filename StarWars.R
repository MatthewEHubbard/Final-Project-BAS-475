```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(fpp3)
library(patchwork)
library(seasonal)
library(feasts)
library(tsibble)
library(tsibbledata)
library(ggeasy)
```

```{r setup, include=FALSE}
DATA <- read.csv("Credit.csv")
summary(DATA)
DATA$Credit <- DATA$ï..credit_in_millions
DATA$Month <- seq(492,1, by =-1)
DATA <- DATA[order(DATA$Month),]
DATA <- DATA[,2:3]
TimeSeries <- DATA %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

autoplot(TimeSeries)

TimeSeries %>%
  features(Credit, features = guerrero) %>%
  pull(lambda_guerrero) -> lvalue

lvalue

TimeSeries %>%
  mutate(transform = box_cox(Credit,lvalue)) -> TimeSTransformed
#Plot transformed series
TimeSTransformed %>% gg_tsdisplay(transform)

TRAIN <- TimeSTransformed %>%
  filter(Month<=yearmonth('May 2007'))

HOLDOUT <- TimeSTransformed %>%
  filter(Month>yearmonth('May 2007'))

TRAIN <- TRAIN[,2:3]
HOLDOUT <- HOLDOUT[,2:3]
MODELS <- TRAIN %>%
  model(
    NaiveModel = NAIVE(transform),
    tslm = TSLM(~trend(transform)),
    ets = ETS(transform),
    arima210 = ARIMA(transform ~ pdq(2,1,0)),
    arima013 = ARIMA(transform ~ pdq(0,1,0)),
    stepwise = ARIMA(transform),
    search = ARIMA(transform, stepwise = FALSE)
  )

FORECAST <- MODELS %>%
  forecast(h = 12)

autoplot(FORECAST)

accuracy(FORECAST, TimeSTransformed) %>%
  arrange(RMSE)
  
```
