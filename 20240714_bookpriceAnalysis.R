# ライブラリの読み込み
install.packages("tidyverse")
library(tidyverse)

# データの読み込み
bookprice <- read.csv('bookprice.csv')
# データの構造の確認
str(bookprice)

# priceの基本統計量とヒストグラム
summary(bookprice$price)
hist(bookprice$price, breaks = 50, col="blue", xlab="Price", ylab="Frequency", main="Distribution of Book Prices")

# class別のデータ数
table(bookprice$class)
# classとlangのクロス集計表
cross_table <- table(bookprice$class, bookprice$lang)
cross_table

# class別のpriceの基本統計量
aggregate(bookprice$price, by=list(bookprice$class), FUN=summary)
# class別のpriceの箱ひげ図
boxplot(price ~ class, data = bookprice, main="Boxplot of Prices by Class", xlab="Class", ylab="Price")

# 一元配置分散分析
anova_result <- aov(price ~ class, data = bookprice)
summary(anova_result)

# 多重比較
# TukeyのHSDテスト
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
# ボンフェローニで調整した多重比較
bonferroni_result <- pairwise.t.test(bookprice$price, bookprice$class,
                                     p.adjust.method = "bonferroni")
print(bonferroni_result)

# 相関係数の確認
cor_pages_price <- cor(bookprice$pages, bookprice$price, use="complete.obs")
cor_format_cm2_price <- cor(bookprice$format_cm2, bookprice$price, use="complete.obs")
cor_pages_price
cor_format_cm2_price

# 新しい変数 volume を作成
bookprice$volume = bookprice$pages * bookprice$format_cm2
str(bookprice)

# volumeを用いた単回帰分析
simple_model <- lm(price ~ volume, data = bookprice)
summary(simple_model)

# volume, class, color, author, langを用いた重回帰分析
full_model <- lm(price ~ volume + class + color + author + lang, data = bookprice)
summary(full_model)

