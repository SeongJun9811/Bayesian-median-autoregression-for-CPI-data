# 데이터 불러오기
tr <- read.csv("C:/Users/seongjun/Desktop/cpi.csv")

# AR반복문
forecasts_ar1 <- numeric(158 - 59)
forecasts_ar2 <- numeric(100)
forecasts_ar3 <- numeric(100)
forecasts_ar4 <- numeric(100)

# AR모델 적용
for (i in 60:159) {
  y_sub <- tr$cpi[1:i]
  
  # AR 모델의 order를 기반으로 1시점 이후 값을 예측
  set.seed(2023)
  ar_model <- arima(y_sub, order = c(4, 1, 0),method="ML")
  forecast_ar <- forecast(ar_model, h = 4)
  
  forecast_ar1 <- forecast_ar$mean[1]
  forecast_ar2 <- forecast_ar$mean[2]
  forecast_ar3 <- forecast_ar$mean[3]
  forecast_ar4 <- forecast_ar$mean[4]
  
  
  forecasts_ar1[i - 59] <- forecast_ar1
  forecasts_ar2[i - 59] <- forecast_ar2
  forecasts_ar3[i - 59] <- forecast_ar3
  forecasts_ar4[i - 59] <- forecast_ar4
}

# 성능평가
tem <- matrix(0,100,4)
tr <- read.csv("C:/Users/seongjun/Desktop/cpi.csv")
r <- matrix(0,100,4)
for(h in 1:4){ # h-step ahead true data
  r[,h] <- tr[(60+h):(159+h),2]
}

tem[,1] <- forecasts_ar1
tem[,2] <- forecasts_ar2 
tem[,3] <- forecasts_ar3 
tem[,4] <- forecasts_ar4 

sqrt(apply((tem - r)^2,2,mean))
apply( abs(tem - r), 2, mean)















