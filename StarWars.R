```{r setup, include=FALSE}


library(fpp3)
library(patchwork)
library(seasonal)
library(feasts)
library(tsibble)
library(tsibbledata)
library(ggeasy)


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
  mutate(transform = box_cox(Credit,lvalue)) -> TimeSTransformedboxcox

TimeSeries %>%
  mutate(transform = (box_cox(Credit,lvalue)%>%difference(12))) -> TimeSTransformed
#Plot transformed series

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
report(MODELS)

FORECAST <- MODELS %>%
  forecast(h = 12)

autoplot(FORECAST)

accuracy(FORECAST, TimeSTransformed) %>%
  arrange(RMSE)
#Best model (Arima)
bestfit<-TimeSTransformed %>%
  model(Arima210 = ARIMA(transform~pdq(2,1,0)))

pred <- bestfit %>%
  forecast(h = 12)

#untransformed
last12<-tail(TimeSTransformedboxcox$transform,12)
tpred<-pred$.mean
unpred<-pred$.mean%>%diffinv(lag=12,xi=last12)
unpred<-inv_box_cox(unpred[13:24],lvalue)
DATAFINAL<- data.frame(matrix(ncol = 2, nrow = 504))
DATAFINAL[1:492,1] <-DATA$Credit
DATAFINAL[,2] <- seq(1,504, by =1)
DATAFINAL[493:504,1] <- unpred
colnames(DATAFINAL) <- c('Credit1', 'Month')
TimeSeriesFin <- DATAFINAL %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)
autoplot(TimeSeriesFin)


```
