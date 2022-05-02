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

{r setup, include=FALSE}
DATA <- read.csv("Credit.csv")
summary(DATA)
DATA$Month <- seq(492,1, by =-1)
DATA <- DATA[order(DATA$Month),]
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

TimeSTransformed %>%
  features(transform, unitroot_kpss)


TRAIN <- TimeSeries %>%
  filter(Month<=yearmonth('May 2007'))

HOLDOUT <- TimeSeries %>%
  filter(Month>yearmonth('May 2007'))

MODELS <- TRAIN %>%
  model(
    NaiveModel = NAIVE(),
    tslm = TSLM(~trend()),
    ets = ETS(),
    arima210 = ARIMA(credit_in_millions ~ pdq(2,1,0)),
    arima013 = ARIMA(credit_in_millions ~ pdq(0,1,3)),
    stepwise = ARIMA(credit_in_millions),
    search = ARIMA(credit_in_millions, stepwise = FALSE)
  )

FORECAST <- MODELS %>%
  forecast(h = 12)

autoplot(FORECAST)

accuracy(FORECAST, TimeSeries) %>%
  arrange(RMSE)