

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
<<<<<<< HEAD
<<<<<<< HEAD
=======
    tslm = TSLM(~trend(transform)),
>>>>>>> 984a55c5a4d3dfa8f4877b1fae4e1d541fc3f2d0
=======
    tslm = TSLM(~trend(transform)),
>>>>>>> 984a55c5a4d3dfa8f4877b1fae4e1d541fc3f2d0
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

DATA[493:504,1]<-unpred

unpred2<-c(1.861565, 1.896442, 1.960625, 1.952855, 1.940564, 1.947550, 1.936839, 1.934770, 1.934962, 1.945662, 1.967409, 1.900367)






