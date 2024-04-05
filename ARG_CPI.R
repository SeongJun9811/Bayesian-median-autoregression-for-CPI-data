library(VGAM)
library(mvtnorm)
library(coda)
library(geoR)
library(MCMCpack)
library(tidyverse)
library(ggplot2)
library(forecast)
library(rugarch)
library(progress)

# 데이터 불러오기
tr <- read.csv("C:/Users/seongjun/Desktop/cpi.csv")

# 원래 데이터를 저장할 벡터 생성
original_values <- numeric(100)

for (i in 60:159) {
  y_sub <- tr$cpi[1:i]
  y_diff <- diff(y_sub)  # 1차 차분 적용
  
  set.seed(2023)
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(4, 0)))
  fit <- ugarchfit(spec = spec, data = y_diff)
  arg <- ugarchforecast(fit, n.ahead = 1)
  
  arg_fore <- arg@forecast$seriesFor[1]
  
  # 예측값에 마지막 값을 더하여 원래의 형태로 되돌림
  predicted_original_value <-  arg_fore + y_sub[length(y_sub)]
  
  original_values[i - 59] <- predicted_original_value
}

original_values

write.csv(original_values, 'arg1.csv', row.names = F)

### 2시점 예측
p1 <- read.csv('C:/Users/seongjun/Documents/arg1.csv', header = T)
p1 <- p1$x
p1 <- as.data.frame(p1)

for (i in 60:159) {
  y_sub <- c(tr[1:i, ]$cpi, p1[(i-59),1])
  y_diff <- diff(y_sub)  # 1차 차분 적용
  set.seed(2023)
  
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(4, 0)))
  fit <- ugarchfit(spec = spec, data = y_diff)
  arg <- ugarchforecast(fit, n.ahead = 1)
  
  arg_fore <- arg@forecast$seriesFor[1]
  
  # 예측값에 마지막 값을 더하여 원래의 형태로 되돌림
  predicted_original_value <-  arg_fore + y_sub[length(y_sub)]
  
  original_values[i - 59] <- predicted_original_value

}

original_values

write.csv(original_values, 'arg2.csv', row.names = F)



### 3시점 예측
p2 <- read.csv('C:/Users/seongjun/Documents/arg2.csv', header = T)
p2 <- p2$x
p2 <- as.data.frame(p2)

for (i in 60:159) {
  y_sub <- c(tr[1:i, ]$cpi, p1[(i-59),1], p2[(i-59),1])
  y_diff <- diff(y_sub)  # 1차 차분 적용
  
  set.seed(2023)
  
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(4, 0)))
  fit <- ugarchfit(spec = spec, data = y_diff)
  arg <- ugarchforecast(fit, n.ahead = 1)
  
  arg_fore <- arg@forecast$seriesFor[1]
  
  # 예측값에 마지막 값을 더하여 원래의 형태로 되돌림
  predicted_original_value <-  arg_fore + y_sub[length(y_sub)]
  
  original_values[i - 59] <- predicted_original_value
}

original_values

write.csv(original_values, 'arg3.csv', row.names = F)


### 4시점 예측
p3 <- read.csv('C:/Users/seongjun/Documents/arg3.csv', header = T)
p3 <- p3$x
p3 <- as.data.frame(p3)


for (i in 60:159) {
  y_sub <- c(tr[1:i, ]$cpi, p1[(i-59),1], p2[(i-59),1], p3[(i-59),1])
  y_diff <- diff(y_sub)  # 1차 차분 적용
  
  set.seed(2023)
  
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(4, 0)))
  fit <- ugarchfit(spec = spec, data = y_diff)
  arg <- ugarchforecast(fit, n.ahead = 1)
  
  arg_fore <- arg@forecast$seriesFor[1]
  
  # 예측값에 마지막 값을 더하여 원래의 형태로 되돌림
  predicted_original_value <-  arg_fore + y_sub[length(y_sub)]
  
  original_values[i - 59] <- predicted_original_value
}

original_values

write.csv(original_values, 'arg4.csv', row.names = F)

p4 <- read.csv('C:/Users/seongjun/Documents/arg4.csv', header = T)

# 성능평가
tem <- matrix(0,100,4)
r <- matrix(0,100,4)
for(h in 1:4){ # h-step ahead true data
  r[,h] <- tr[(60+h):(159+h),2]
}

tem[,1] <- p1$p1
tem[,2] <- p2$p2
tem[,3] <- p3$p3 
tem[,4] <- p4$x

sqrt(apply((tem - r)^2,2,mean))
apply( abs(tem - r), 2, mean)






