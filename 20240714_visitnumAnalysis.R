install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)

# データの読み込み
data <- read_csv("visitnum.csv")

# 日付の形式でyearとmonthを結合する
data$Date <- make_date(data$year, data$month)

# 最初の数行を確認
head(data)

# パッケージの読み込み
library(ggplot2)

# 全入館者数のみを用いたライングラフの描画
ggplot(data, aes(x = Date, y = all)) + 
  geom_line(colour = "blue") +  # 青色の線で表示
  labs(title = "Monthly Total Visitors to the Library",
       x = "Date", y = "Number of Visitors") +
  theme_minimal()

# パッケージの読み込み
install.packages("zoo")
library(zoo)

# 12ヶ月移動平均の計算
data$MovingAverage <- rollmean(data$all, 12, fill = NA, align = 'right')

# 元の時系列データと移動平均を用いたライングラフの描画
ggplot(data, aes(x = Date)) + 
  geom_line(aes(y = all), colour = "blue", alpha = 0.5) +  # 元のデータ
  geom_line(aes(y = MovingAverage), colour = "red") +  # 移動平均
  labs(title = "Monthly Total Visitors to the Library with 12-Month Moving Average",
       x = "Date", y = "Number of Visitors") +
  theme_minimal()

# パッケージの読み込み
install.packages("tseries")
install.packages("forecast")
library(tseries)
library(forecast)

# 時系列オブジェクトの作成
ts_data <- ts(data$all, start = c(2012, 4), frequency = 12)

# 季節分解 (STL)
decomposed <- stl(ts_data, s.window = "periodic")

# 分解結果のプロット
plot(decomposed)

