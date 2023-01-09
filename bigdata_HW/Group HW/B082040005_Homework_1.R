### 請加載給定的數據集“kbopitchingdata.csv”並回答以下數據管理問題

df1 = read.csv("C:/Users/user/Desktop/bigdata_HW/Dataset/Dataset/kbopitchingdata.csv")
str(df1)

# 1.1 [5分] 請刪除任何 NA 的觀察結果

rm.df1 <- df1[complete.cases(df1), ] # == na.omit(df1)

# sum(complete.cases(df1)) # 184，row
# sum(is.na(df1))          # 695個
str(rm.df1)
summary(rm.df1)

# 1.2 [5分] 請把"year"轉換成多組，
#     “<2005”，“2006-2010”，“2011-2015”、“2016-2020”、“2021-2025”
#     並分配給一個名為的新變量 "year_interval"

rm.df1["year_interval"] = cut(rm.df1$year, c(2000,2005,2010,2015,2020, 2025),c("<2005","2006-2010","2011-2015","2016-2020","2021-2025"))
# rm.df1["year_interval"]   # 提供原始資料，切點，新標籤

# 1.3 [5分] 以 year_interval 計算獲勝者的平均 ERA

# kv1 = split(rm.df1,cut(rm.df1$year, breaks = c(2000,2005,2010,2015,2020,2025),c("<2005","2006-2010","2011-2015","2016-2020","2021-2025")))
# lapply(kv1,function(x) return(mean(x$ERA)))

library(dplyr)

rm.df1 %>% group_by(year_interval) %>% filter(wins>60) %>% summarise(meaneda = mean(ERA))

# 1.4 [5分] 計算每支球隊的總勝場數，並按總勝場數排序升序。請在 SQL 中執行

# install.packages("sqldf")
library(sqldf)
# library(dplyr)
library(data.table)

rm.df1 %>% group_by(team) %>% summarize(Coun = sum(wins)) %>% arrange(Coun)

a = setDT(rm.df1) # "data.table" "data.frame"
sqldf('SELECT team,sum("wins") FROM a
GROUP BY team ORDER BY sum("wins") ASC')

# sqldf("SELECT * FROM sqlite_master;")

# 1.5 [10分] 創建average_age 分佈的密度圖。是不是看起來"normal"？
#     如果您願意，可以使用任何統計方法證明您的答案

plot(density(rm.df1$average_age))

library(ggplot2)
ggplot(data.frame(x=rm.df1$average_age), aes(sample=x)) +
  stat_qq() + stat_qq_line()

shapiro.test(rm.df1$average_age) 
# p-value = 0.2762 > 0.05 跟常態分布無顯著差異

# 1.6 [10分] average_age 與 ERA 相關嗎？
#     使用任何統計數據用簡短的描述證明你的答案的方法。

cor.test(rm.df1$average_age,rm.df1$ERA)

# 他們的 cor:0.06314955 不相關
# p-value = 0.3944 
# 所以我們沒有足夠的證據表明
# 這兩個變數之間的相關性在統計意義上顯著

plot(rm.df1$average_age,rm.df1$ERA)

# 從 plot 上可以看出接近 0 相關

summary(lm(rm.df1$average_age~rm.df1$ERA,rm.df1))

# pvalue 0.394 所以 average_age 跟 ERA 間可說是不互相影響
# 從 Multiple R-squared:0.003988、Adjusted R-squared:-0.001485 
# 也能看出這條線擬合得不好

### 請加載給定的數據集“vaccine 2022 TW.csv”以獲取有關疫苗的新聞。請回答以下問題。

library(readr)

# 嘗試使用 read.csv 的參數，例如 fileEncoding 和 quote 
# 我最終在 readr 包中使用了 read_csv - 只需使用默認參數 - 它立即完美地加載了所有內容！

df2 = read_csv("C:/Users/user/Desktop/bigdata_HW/Dataset/Dataset/vaccine_2022_TW.csv")
str(df2)

# 2.1 [10分] 請從數據集中刪除任何 NA 的觀測值
#     並計算每個域的新聞數量。哪個域的新聞最多？

newdf2 =  na.omit(df2) # df2[complete.cases(df2), ]
# sum(complete.cases(df2)) # 完整資料 20393，row
# sum(is.na(df2))          # 7268個
# str(newdf2)
# summary(newdf2)
newdf2 %>% group_by(domainArticle) %>% summarise(do = n()) %>% arrange(desc(do)) %>% head(1)
# news.sina.com.tw  5332

# 2.2 [5分] 請計算出新聞最多的領域每天的新聞數量(for the most news domain)。

library(lubridate)
floor_date(newdf2$datetimeArticle, "day")

newdf2[newdf2$domainArticle=="news.sina.com.tw",] %>% group_by(floor_date(datetimeArticle, "day")) %>% summarise(num = n())

# 2.3 [10分] 請以移動平均線的方式打印包含最多新聞域的折線圖。
#     請將數字四捨五入為整數並標註每週的日期 x 軸

# install.packages("tseries")
# install.packages("forecast")
library(tseries)
library(forecast)
# library(ggplot2)

library(scales)

AAOI = newdf2[newdf2$domainArticle=="news.sina.com.tw",]

AAOI$MA7 = ma(floor_date(AAOI$datetimeArticle, "day"),order = 7)

ggplot(data = AAOI,aes(x=round_date(datetimeArticle, "week")))+
  geom_line(stat='identity',aes(y=MA7,colour="MA7"),size=0.8)+
  labs(x="date",y="number",title="AAOI Moving Average")+
  theme(legend.position = c(0.2,0.8))+
  theme_bw()+
  scale_x_datetime(breaks = round_date(AAOI$datetimeArticle, "week"))+
  theme(axis.text.x = element_text(angle=45))

# 標註每週的日期 x 軸 
# round_date(AAOI$datetimeArticle, "week")
# https://ggplot2.tidyverse.org/reference/scale_date.html

### 請加載給定的數據集“HW_car_purchasing_final.csv”並回答以下問題

df3 = read.csv("C:/Users/user/Desktop/bigdata_HW/Dataset/Dataset/HW_car_purchasing_final.csv")
str(df3)

# 3.1 [10分] 考慮一系列關於“購車金額”與“剩下”的雙變量分析休息變量。
#     具體來說，繪製您的數據並執行雙變量統計測試以了解變量之間的關係。
#     是“年薪”和“淨資產”與“購車金額”相關聯？使用任何統計方法來證明你的答案。
#     另請注意，您可能會考慮對“汽車”進行任何數據轉換購買金額”，
#     有助於了解關聯或更好地預測“汽車採購量”。

plot(density(df3$car_purchase_amount))
shapiro.test(df3$car_purchase_amount) 
# p-value = 0.5566 > 0.05 跟常態分布無顯著差異

library(car)

par(mar=c(4,3,2,1))
# finaldata2 = scale(finaldata1[c(2:21)])
scatterplotMatrix(df3[,c(3:7)])

barplot(table(df3$gender))
barplot(table(df3$level))

library(corrplot)

cormatrix = cor(df3[,c(3:7)])
# corrplot(corr=cormatrix,method = "ellipse",order = "hclust",
#         tl.col="red",addrect=4,addCoef.col = "black", number.cex = 0.5)

testRes = cor.mtest(df3[,c(3:7)], conf.level = 0.95)
## specialized the insignificant value according to the significant level

corrplot(cormatrix, p.mat = testRes$p,
         insig='blank',addCoef.col ='red',
         number.cex = 1,order = 'hclust', addrect = 2)

# 對，年薪、淨資產、年齡都有著95%的信心跟購車金額相關

# df3 %>% group_by(gender) %>% summarise(mean(car_purchase_amount))
# df3 %>% group_by(level) %>% summarise(mean(car_purchase_amount))
# s = split(df3,df3$gender)
# summary(lapply(s,function(df) lm(car_purchase_amount~level,data=df)))

# 3.2 [10分] 請把數據集分成訓練集（70%）和測試集（30%）set.seed(1)。
#     然後將連續變量重新縮放為從 0 到 1 的值無需集中。 
#    （提示：你可以使用“caret”包，你應該重新縮放具有相同特徵轉換計劃的訓練集和測試集。）

library(caTools)

set.seed(1)

#df3 = (model.matrix(~ ., data = df3)) # dummy
#df3 = as.data.frame(df3)
# df3 = df3[,-c(1)]
str(df3)

spl = sample.split(df3$car_purchase_amount, SplitRatio=0.7)  # 回傳 TRUE & FALSE

# c(nrow(df3), sum(spl), sum(!spl))

train <- df3[spl,] 
test <- df3[!spl,]

library(caret)

# train2 = (train-min(train))/(max(train)-min(train))
# Error: 套件或命名空間載入失敗 ‘caret’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
# 載入了命名空間 ‘rlang’ 1.0.1，但需要的是 >= 1.0.2
# install.packages("caret")
# remove.packages("caret")
# 看起來是套件 rlang 版本的問題，同學可以參考下面的連結
# 1. 先解除安裝 "rlang" 套件
# 2. 再重啟 R，並且重新安裝 rlang，最後再試看看安裝其他套件 (caret, ggplot2)
# https://stackoverflow.com/questions/53542831/how-to-install-specific-version-of-rlang-package-in-r

# https://rpubs.com/SiQingYe/752656 視覺 預處理
pre = preProcess(train, method = "range")

train_tran = predict(pre,train)
test_tran = predict(pre,test)

# 3.3 [5分] 編寫一個計算平均絕對誤差 (MAE) 的 R 函數，即定義為：

MAE = function(x,y){
  return (mean(abs(x-y)))
}

# 3.4 [5分] 用重新縮放的訓練集建立一個通用線性模型，
#     然後報告訓練和測試 MAE（四捨五入到小數點後四位）。

str(train_tran)
# head(model.matrix(car_purchase_amount~ ., data = train_tran)) # dummy

set.seed(1)

model = glm(car_purchase_amount~.,data = train_tran)
summary(model)

newy = predict(model,test_tran)

round(MAE(model$fitted.values, train_tran$car_purchase_amount),4) #  1.565493e-05
round(MAE(test_tran$car_purchase_amount, newy),4) #  1.878118e-05

# 3.5 [5分] 移除 p 值較高（> 0.05）的預測變量，然後構建一個新的一般線性模型。
#     新模型在訓練和訓練方面的誤差是否更低？測試MAE？

newmodel = glm(car_purchase_amount ~ age +annual_Salary +net_worth,data=train_tran)
summary(newmodel)

newy = predict(newmodel,test_tran)

round(MAE(newmodel$fitted.values, train_tran$car_purchase_amount),4) # 1.567099e-05
round(MAE(test_tran$car_purchase_amount, newy),4) # 1.873719e-05

# 3.6 [10分] 同樣，我們想要另一個考慮所有雙向的新模型
#     在不刪除任何預測變量的情況下進行交互。請報告培訓和測試MAE。
#     新模型在訓練和測試 MAE 方面的誤差是否更低？
#     這種具有更多參數的複雜模型能否改善預測？
#     哪個型號你會推薦在現實世界的生產中使用嗎？為什麼？

Imodel = glm(car_purchase_amount ~ (.)^2, data=train_tran)
summary(Imodel)

newy = predict(Imodel,test_tran)

round(MAE(Imodel$fitted.values, train_tran$car_purchase_amount),4) # 1.501058e-05
round(MAE(test_tran$car_purchase_amount, newy),4) # 1.937741e-05

# 訓練集是，測試集因為過擬和有上升情形

# 不一定/幾乎沒有，重點是要是有意義的參數才有用

# 第二個經過P值挑選過的，
# 因為變數太多容易學到噪音，容易過擬和，在資料大時，速度還會明顯變慢

# 助教您好，我懷疑我有用到target的資訊因為MAE超低，但我找不到證據
# 還是是因為 scale 到0-1 的關係? 謝謝助教
