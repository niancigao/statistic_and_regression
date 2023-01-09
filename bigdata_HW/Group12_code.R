
# "Group 12"


##### 載入套件 #####
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, plotly, lubridate, vcd, gridExtra,
               car, readr, sf, tmap, highcharter, RColorBrewer, magrittr, knitr, fastDummies, 
               psych, tidyverse, viridis, geosphere, MatchIt, stargazer, cobalt, eatATA, quantreg)
library("MatchIt","stargazer")
library("quantreg")
library("eatATA")
library(caret)
library(corrplot)
library(readxl)
library(readr)
library(data.table)
library(dplyr)
library(geosphere)
pals17 = c(brewer.pal(9,"GnBu"), rev(brewer.pal(8,"YlGnBu")),rev(brewer.pal(9,"PuBu")))


##### 載入資料 #####
KH <- read_csv("Dataset/KH_housing_data.csv") #高雄房屋交易資料
temple <- read_csv("Dataset/高雄市寺廟基本資料.csv")



##### 資料概述 #####

str(KH)
str(temple)



## 刪除多餘變數
KH <- subset(KH , 
             select = -c(51:114))
KH <- data.table(KH)

## 提取房屋、寺廟座標

KH_location <- data.frame(X = KH$X, Y = KH$Y)
tp_location_chr <- data.frame(X = temple$Longitude, Y = temple$Latitude)
tp_location <- as.data.frame(lapply(tp_location_chr, as.numeric))


## 計算房屋和寺廟的最小距離並新增變數temple_distant

temple_distant <- c()
dis <- function(x){
  
  return(min(distHaversine(x,tp_location[,])))
}
temple_distant<- apply(KH_location,1,dis)
KH$temple_distant <- temple_distant
summary(KH$temple_distant)


## 計算高雄區內寺廟密度 Step1: 載入和篩出需要資料

Land_area = fread("Dataset/population_density.csv", header = T)
str(Land_area)
str(temple)
Land_area = Land_area[,c(2,4)]
temple = temple[,c(5)]

## 計算高雄區內寺廟密度 Step3: 計算高雄各區面積、寺廟數量

KH_Land = subset(Land_area[grepl("高雄市" , site_id),])
KH_ID_area = gsub('高雄市', '', KH_Land$site_id)
KH_ID_area = data.frame(KH_ID_area)
KH_area = cbind(KH_ID_area, KH_Land$area)

temple_num = temple %>% group_by(場所區域) %>% summarise(coun = n())

## 計算高雄區內寺廟密度 Step4: 得出高雄各區寺廟密度
density = merge(KH_area, temple_num, by.x = "KH_ID_area", by.y = "場所區域")
temple_density = density %>% transmute(density[,1], "各區寺廟密度" = (density[,3]/as.numeric(density[,2])))
temple_density %>%  arrange(desc(各區寺廟密度)) %>% head()


## 檢查並刪除高雄房屋有NA值資料

sum(is.na(KH)) #4029
KH <- na.omit(KH)

# ----------------------------單變量分析----------------------------

summary(KH)

## 計算高雄各行政區「交易數量」

trade_number <- KH %>% group_by(Location_Town) %>% 
  summarise(
    number_of_trade = n()
  ) %>% arrange(desc(number_of_trade))

trade_number

## 繪製高雄各行政區「交易數量」

ggplot(data = trade_number)+
  geom_bar(aes(x = Location_Town, y = number_of_trade, fill = Location_Town), stat = 'identity')+
  geom_text(aes(x = Location_Town, y = number_of_trade, label = number_of_trade), vjust = -0.3, size = 5) +
  labs( x = "行政區", y = "交易量", title = "行政區交易量")+
  theme(strip.text = 
          element_text( size = 22, color = "white",  
                        hjust = 0.5, family = "Heiti TC Light",face = "bold", ),
        axis.text.x = element_text(angle = 45), # x軸呈現45度
        text = element_text(size = 20 ,color="black", family = "Heiti TC Light",face = "bold", ),
        axis.title.x = element_text(family = "Heiti TC Light"),
        axis.title.y = element_text(family = "Heiti TC Light"),
        title = element_text(family = "Heiti TC Light"),
        panel.background = element_rect( fill = "#efefef", color = NA ),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2"),
        legend.position = "none")



### 計算行政區交易價格(中位數)

trade_price <- KH %>% group_by(Location_Town) %>% 
  summarise(trade_price = median(Price_total))


### 繪製行政區交易價格(中位數)

ggplot(data = trade_price)+
  geom_bar(aes(x = Location_Town,y = trade_price, fill = Location_Town), stat = 'identity')+
  geom_text(aes(x = Location_Town, y = trade_price, label = trade_price), vjust=-0.3, size = 5) +
  labs( x = "行政區", y = "價格（萬元）",title = "行政區價格（中位數）")+
  theme(  strip.text = element_text(size = 22, color = "white",  
                                    hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          axis.text.x = element_text(angle = 45), # x軸呈現45度
          text = element_text(size = 20 ,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")

## 查看廟宇距離

summary(KH$temple_distant)




## 建物類型
# Building_Apt-general:公寓
# Building_Apt-WS:華夏
# Building_Apt-luxury:大樓
# Building_House:透天
# Building_Suit:套房

### 建物類型[旗津(cijin)區]

type_cijin <- KH %>% summarise(
  公寓 = sum(KH$`Building_Apt-general`==1 & KH$Location_Town =='旗津區'),
  華夏 = sum(KH$`Building_Apt-WS` ==1 & KH$Location_Town =='旗津區'),
  大樓 = sum(KH$`Building_Apt-luxury`== 1 & KH$Location_Town =='旗津區'),
  透天 = sum(KH$Building_House== 1 & KH$Location_Town =='旗津區'),
  套房 = sum(KH$Building_Suit== 1 & KH$Location_Town =='旗津區')
)
type_cijin
type_cijin = type_cijin %>% gather(key = 建物類型, value = 數量, 公寓:套房)



ggplot(data=type_cijin )+
  geom_bar(aes(x=建物類型,y=數量, fill=建物類型),stat = 'identity')+
  geom_text(aes(x=建物類型, y=數量, label =數量),vjust=-0.3, size = 8) +
  labs( x = "建物類型", y = "數量",title = "")+
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")



### 建物類型 [鹽埕(Yancheng)區]

type_Yancheng <- KH %>% summarise(
  公寓 = sum(KH$`Building_Apt-general`== 1 & KH$Location_Town =='鹽埕區'),
  華夏 = sum(KH$`Building_Apt-WS`== 1 & KH$Location_Town =='鹽埕區'),
  大樓 = sum(KH$`Building_Apt-luxury`== 1 & KH$Location_Town =='鹽埕區'),
  透天 = sum(KH$Building_House== 1 & KH$Location_Town =='鹽埕區'),
  套房 = sum(KH$Building_Suit== 1 & KH$Location_Town =='鹽埕區')
)
type_Yancheng
type_Yancheng = type_Yancheng %>% gather(key = 建物類型, value = 數量, 公寓:套房)



ggplot(data=type_Yancheng )+
  geom_bar(aes(x=建物類型,y=數量, fill=建物類型),stat = 'identity')+
  geom_text(aes(x=建物類型, y=數量, label =數量),vjust=-0.3, size = 8) +
  labs( x = "建物類型", y = "數量",title = "")+
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")




### 高雄全區

type_all <- KH %>% summarise(
  公寓 = sum(KH$`Building_Apt-general`== 1 ),
  華夏 = sum(KH$`Building_Apt-WS` == 1 ),
  大樓 = sum(KH$`Building_Apt-luxury`== 1 ),
  透天 = sum(KH$Building_House== 1 ),
  套房 = sum(KH$Building_Suit== 1 )
)
type_all
type_all = type_all %>% gather(key = 建物類型, value = 數量, 公寓:套房)




ggplot(data=type_all )+
  geom_bar(aes(x=建物類型,y=數量, fill=建物類型),stat = 'identity')+
  geom_text(aes(x=建物類型, y=數量, label =數量),vjust=-0.3, size = 8) +
  labs( x = "建物類型", y = "數量",title = "")+
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")




## 交易類型
# Type_park_space:交易類型: 車位
# Type_land:交易類型: 土地
# Type_bulid without PS: 交易類型: 建物 (不包含車位)
# Type_build with PS: 交易類型: 建物 (包含車位)

### 交易類型[旗津(cijin)區]

trade_cijin <- KH %>% summarise(
  車位 = sum(KH$Type_parking.space== 1 & KH$Location_Town =='旗津區'),
  土地 = sum(KH$Type_land == 1 & KH$Location_Town =='旗津區'),
  建物_包含車位= sum(KH$`Type_building with PS`== 1 & KH$Location_Town =='旗津區'),
  建物_不包含車位 = sum(KH$`Type_building without PS`== 1 & KH$Location_Town =='旗津區')   
)
trade_cijin
trade_cijin = trade_cijin %>% gather(key = 交易類型, value = 數量, 車位:建物_不包含車位)

ggplot(data=trade_cijin )+
  geom_bar(aes(x=交易類型,y=數量, fill=交易類型),stat = 'identity')+
  geom_text(aes(x=交易類型, y=數量, label =數量),vjust=-0.3, size = 8) +
  labs( x = "交易類型", y = "數量",title = "")+
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")


### 交易類型[鹽埕(Yancheng)區]


trade_Yancheng <- KH %>% summarise(
  車位 = sum(KH$Type_parking.space== 1 & KH$Location_Town =='鹽埕區'),
  土地 = sum(KH$Type_land == 1 & KH$Location_Town =='鹽埕區'),
  建物_包含車位= sum(KH$`Type_building with PS`== 1 & KH$Location_Town =='鹽埕區'),
  建物_不包含車位 = sum(KH$`Type_building without PS`== 1 & KH$Location_Town =='鹽埕區')   
)
trade_Yancheng
trade_Yancheng = trade_Yancheng %>% gather(key = 交易類型, value = 數量, 車位:建物_不包含車位)



ggplot(data=trade_Yancheng )+
  geom_bar(aes(x=交易類型,y=數量, fill=交易類型),stat = 'identity')+
  geom_text(aes(x=交易類型, y=數量, label =數量),vjust=-0.3, size = 8) +
  labs( x = "交易類型", y = "數量",title = "")+
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")




### 交易類型[高雄全區]


trade_all <- KH %>% summarise(
  車位 = sum(KH$Type_parking.space== 1),
  土地 = sum(KH $Type_land == 1),
  建物_包含車位= sum(KH$`Type_building with PS`== 1),
  建物_不包含車位 = sum(KH$`Type_building without PS`== 1)   
)
trade_all
trade_all= trade_all %>% gather(key = 交易類型, value = 數量, 車位:建物_不包含車位)



ggplot(data=trade_all )+
  geom_bar(aes(x=交易類型,y=數量, fill=交易類型),stat = 'identity')+
  geom_text(aes(x=交易類型, y=數量, label =數量),vjust=-0.3, size = 8) +
  labs( x = "交易類型", y = "數量",title = "")+
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")


## 使用類型

## Using type_L :住宅
## Using type_B：商業
## Using type_Far ：農用
## Using type_Fac ：工業
## Using type_O ：其他

### 使用類型[旗津(Yancheng)區]

use_Yancheng <- KH %>% summarise(
  住宅 = sum(KH$`Using type_L`==1 & KH$Location_Town=='旗津區'),
  商業 = sum(KH$`Using type_B`==1 & KH$Location_Town=='旗津區'),
  農用 = sum(KH$`Using type_Far`==1 & KH$Location_Town=='旗津區'),
  工業 = sum(KH$`Using type_Fac`==1 & KH$Location_Town=='旗津區'),
  其他 = sum(KH$`Using type_O`==1 & KH$Location_Town=='旗津區')
)
use_Yancheng
use_Yancheng = use_Yancheng %>% gather(key = 使用類型, value = 數量, 住宅:其他)

ggplot(use_Yancheng)+
  geom_bar(aes(x=使用類型,y=數量, fill=使用類型),stat = 'identity')+
  geom_text(aes(x=使用類型, y=數量, label =數量),vjust=-0.3, size = 8) +
  labs( x = "使用類型", y = "數量",title = "")+
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")



### 使用類型[鹽埕(Yancheng)區]



use_Yancheng <- KH %>% summarise(
  住宅 = sum(KH$`Using type_L`==1 & KH$Location_Town=='鹽埕區'),
  商業 = sum(KH$`Using type_B`==1 & KH$Location_Town=='鹽埕區'),
  農用 = sum(KH$`Using type_Far`==1 & KH$Location_Town=='鹽埕區'),
  工業 = sum(KH$`Using type_Fac`==1 & KH$Location_Town=='鹽埕區'),
  其他 = sum(KH$`Using type_O`==1 & KH$Location_Town=='鹽埕區')
)
use_Yancheng
use_Yancheng = use_Yancheng %>% gather(key = 使用類型, value = 數量, 住宅:其他)


ggplot(use_Yancheng)+
  geom_bar(aes(x=使用類型,y=數量, fill=使用類型),stat = 'identity')+
  geom_text(aes(x=使用類型, y=數量, label =數量),vjust=-0.3, size = 8) +
  labs( x = "使用類型", y = "數量",title = "")+
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")



### 使用類型[高雄全區]

use_all <- KH %>% summarise(
  住宅 = sum(KH$`Using type_L`==1),
  商業 = sum(KH$`Using type_B`==1 ),
  農用 = sum(KH$`Using type_Far`==1 ),
  工業 = sum(KH$`Using type_Fac`==1 ),
  其他 = sum(KH$`Using type_O`==1)
)
use_all
use_all = use_all %>% gather(key = 使用類型, value = 數量, 住宅:其他)


ggplot(use_all)+
  geom_bar(aes(x=使用類型,y=數量, fill=使用類型),stat = 'identity')+
  geom_text(aes(x=使用類型, y=數量, label =數量),vjust=-0.3, size = 8) +
  labs( x = "使用類型", y = "數量",title = "")+
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")



# 房屋年齡
# Basic density
### 旗津(cijin)區

age_cijin <- KH[KH$Location_Town=='旗津區',]

age_cijin <- ggplot(age_cijin, aes(x=Building_age)) + 
  geom_density(alpha=10, fill = 'darkgreen') +
  geom_vline(aes(xintercept=mean(Building_age)), # Add mean line
             color="blue", linetype="dashed", size=1, fill ='cyan') + 
  xlim(0, 100) +
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")
age_cijin

### 鹽埕(Yancheng)區

age_Yancheng <- KH[KH$Location_Town=='鹽埕區',]

age_Yancheng <- ggplot(age_Yancheng, aes(x=Building_age)) + 
  geom_density(alpha=10, fill = 'lightblue') +
  geom_vline(aes(xintercept=mean(Building_age)), # Add mean line
             color="blue", linetype="dashed", size=1, fill ='cyan') +
  xlim(0, 100) +
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")
age_Yancheng



### 高雄全區

age_all <- ggplot(KH, aes(x=Building_age)) + 
  geom_density(alpha=10, fill = 'darkred') +
  geom_vline(aes(xintercept=mean(Building_age)), # Add mean line
             color="blue", linetype="dashed", size=1, fill ='cyan') + 
  xlim(0, 100) +
  theme(  strip.text = element_text( size = 22, color = "white",  
                                     hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          text = element_text(size = 30,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")

age_all


# 房屋格局

#Pattern_room:房間數

room_number<-KH %>% group_by(Pattern_room) %>% summarise(
  number = n()
) %>% head(10)

room_number$Pattern_room <- as.character(room_number$Pattern_room)



ggplot(data=room_number)+
  geom_bar(aes(x=Pattern_room,y=number, fill=Pattern_room),stat = 'identity')+
  geom_text(aes(x=Pattern_room, y=number, label =number),vjust=-0.3, size = 7) +
  labs( x = "房間數", y ="數量",title = "房屋格局")+
  theme(  strip.text = element_text(size = 35, color = "white",  
                                    hjust = 0.5, family = "Heiti TC Light",face = "bold" ),
          axis.text.x = element_text(angle = 0), # x軸呈現0度
          text = element_text(size = 16,color="black",family = "Heiti TC Light",face = "bold" ),
          axis.title.x = element_text(family = "Heiti TC Light"),
          axis.title.y = element_text(family = "Heiti TC Light"),
          title = element_text(family = "Heiti TC Light"),
          panel.background = element_rect( fill = "#efefef", color = NA ),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line( color = "#b2b2b2"),
          legend.position = "none")





############### 雙變量 ###############

# 雙變量
# Pearson's correlation test


# 建物類型
# Building_Apt.general
cor.test(KH$Price_total, KH$`Building_Apt-general`, alternative = "two.side", method = "pearson", conf.level = .95)
# Building_Apt.luxury
cor.test(KH$Price_total, KH$`Building_Apt-luxury`, alternative = "two.side", method = "pearson", conf.level = .95)
# Building_Apt.WS
cor.test(KH$Price_total, KH$`Building_Apt-WS`, alternative = "two.side", method = "pearson", conf.level = .95)
# Building_House
cor.test(KH$Price_total, KH$Building_House, alternative = "two.side", method = "pearson", conf.level = .95)
# Building_Suit
cor.test(KH$Price_total, KH$Building_Suit, alternative = "two.side", method = "pearson", conf.level = .95)
# Building_Others
cor.test(KH$Price_total, KH$Building_Others, alternative = "two.side", method = "pearson", conf.level = .95)


# 樓層
# TtFloor
cor.test(KH$Price_total, KH$TtFloor, alternative = "two.side", method = "pearson", conf.level = .95)


# 格局（房間數）
# Pattern_room
cor.test(KH$Price_total, KH$Pattern_room, alternative = "two.side", method = "pearson", conf.level = .95)


# 交易類型
# Type_parking.space
cor.test(KH$Price_total, KH$`Type_parking space`, alternative = "two.side", method = "pearson", conf.level = .95)
# Type_land
cor.test(KH$Price_total, KH$Type_land, alternative = "two.side", method = "pearson", conf.level = .95)
# Type_building.without.PS
cor.test(KH$Price_total, KH$`Type_building with PS`, alternative = "two.side", method = "pearson", conf.level = .95)
# Type_building.with.PS
cor.test(KH$Price_total, KH$`Type_building without PS`, alternative = "two.side", method = "pearson", conf.level = .95)
# Type_building.price.inclued.PS
cor.test(KH$Price_total, KH$`Type_building price inclued PS`, alternative = "two.side", method = "pearson", conf.level = .95)


# 使用類型
# Using.type_L
cor.test(KH$Price_total, KH$`Using type_L`, alternative = "two.side", method = "pearson", conf.level = .95)
# Using.type_B
cor.test(KH$Price_total, KH$`Using type_B`, alternative = "two.side", method = "pearson", conf.level = .95)
# Using.type_Far
cor.test(KH$Price_total, KH$`Using type_Far`, alternative = "two.side", method = "pearson", conf.level = .95)
# Using.type_Fac
cor.test(KH$Price_total, KH$`Using type_Fac`, alternative = "two.side", method = "pearson", conf.level = .95)
# Using.type_O
cor.test(KH$Price_total, KH$`Using type_O`, alternative = "two.side", method = "pearson", conf.level = .95)

# 屋齡
cor.test(KH$Price_total, KH$Building_age, alternative = "two.side", method = "pearson", conf.level = .95)




# 建物類型與房價
# Building_Apt-general:公寓
# Building_Apt-WS:華夏
# Building_Apt-luxury:大樓
# Building_House:透天
# Building_Suit:套房


KH<-read_csv("Dataset/finaldata.csv")
KH <- na.omit(KH)
price_class <- KH[c("Price_total","房屋類別")]


colnames(price_class)[colnames(price_class) == "房屋類別"] ="building_type"

price_class$building_type = str_replace(price_class$building_type, "公寓", "general")
price_class$building_type = str_replace(price_class$building_type, "華夏", "WS")
price_class$building_type = str_replace(price_class$building_type, "大樓", "luxury")
price_class$building_type = str_replace(price_class$building_type, "透天", "House")
price_class$building_type = str_replace(price_class$building_type, "套房", "Suit")
price_class$building_type = str_replace(price_class$building_type, "其他", "others")


qplot(x=log10(Price_total),                             
      data=price_class,                     
      geom="density",        # 圖形=density
      xlab="log10(Price_total)",
      ylab="density",                         
      color= building_type,
      fill= building_type,
      alpha = 0.2,
      family = "KaiTi"
      # 以顏色標註月份，複合式的機率密度圖
)+theme_bw()


# 樓層與房價
# 1-7 樓
# 8-15 樓
# 16-20 樓
# 20 樓

price_type <- KH[c('Price_total', 'TtFloor')] 
price_type$build_level <- ifelse( price_type$TtFloor<=7,"1-7",ifelse( price_type$TtFloor<=15 &price_type$TtFloor>=8,"8-15",ifelse( price_type$TtFloor<=20 &price_type$TtFloor>=16,"16-20",ifelse( price_type$TtFloor>20,">20","others"))))




qplot(x=log10(Price_total),                             
      data=price_type,                     
      geom="density",        # 圖形=density
      xlab="log10(Price_total)",
      ylab="density",                         
      color= build_level,
      fill= build_level,
      alpha = 0.2,
      family = "KaiTi"
      # 以顏色標註月份，複合式的機率密度圖
)+theme_bw() 





### 屋齡與價格
# 10年以下
# 11 - 20 年
# 20 - 30 年
# 30 年


price_age <-  KH[c('Price_total', 'Building_age')] 
price_age$Building_age<- ifelse( price_age$Building_age<=10,'>10 years',ifelse( price_age$Building_age<=20 &price_age$Building_age>10,"11-20 years",ifelse( price_age$Building_age<=30 &price_age$Building_age>=21,"21-30 years",ifelse( price_age$Building_age>30,">30 years","others"))))

qplot(x=log10(Price_total),                             
      data=price_age,                     
      geom="density",        # 圖形=density
      xlab="log10(Price_total)",
      ylab="density",                         
      color= Building_age,
      fill= Building_age,
      alpha = 0.2,
      family = "KaiTi"
      # 以顏色標註月份，複合式的機率密度圖
)+theme_bw() 


### 使用類型與房價
# Using type_L :住宅
# Using type_B：商業
# Using type_Far ：農用
# Using type_Fac ：工業
# Using type_O ：其他


price_Usingtype <- KH[,c('Price_total', 'Using type_L','Using type_B','Using type_Far','Using type_Fac','Using type_O')]
price_Usingtype$Using_type <- ifelse(price_Usingtype$`Using type_L`==1,'Residential',ifelse(price_Usingtype$`Using type_B`==1,'Business',ifelse(price_Usingtype$`Using type_Far`==1,'agricultural',ifelse(price_Usingtype$`Using type_Fac`==1,'industry','others'))))

qplot(x=log10(Price_total),                             
      data=price_Usingtype,                     
      geom="density",        # 圖形=density
      xlab="log10(Price_total)",
      ylab="density",                         
      color= Using_type,
      fill= Using_type,
      alpha = 0.2,
      family = "KaiTi"
      # 以顏色標註月份，複合式的機率密度圖
)+theme_bw() 


### 交易類型與價格
# Type_park_space:交易類型: 車位
# Type_land:交易類型: 土地
# Type_bulid without PS: 交易類型: 建物 (不包含車位)
# Type_build with PS: 交易類型: 建物 (包含車位)


price_Type_trade <- KH[,c('Price_total', 'Type_parking space','Type_land','Type_building without PS','Type_building with PS')]


price_Type_trade$trade_type <-ifelse(price_Type_trade$`Type_parking space`==1,'Park_space',ifelse(price_Type_trade$Type_land==1,'Type_Land',ifelse(price_Type_trade$`Type_building without PS`==1,'building_without PS',ifelse(price_Type_trade$`Type_building with PS` ==1,'building_with PS','others'))))

qplot(x=log10(Price_total),                             
      data=price_Type_trade,                     
      geom="density",        # 圖形=density
      xlab="log10(Price_total)",
      ylab="density",                         
      color= trade_type,
      fill= trade_type,
      alpha = 0.2,
      family = "KaiTi"
      # 以顏色標註月份，複合式的機率密度圖
)+theme_bw()


### 格局與價格


price_room <- KH[,c('Price_total','Pattern_room')]

price_room$room_level<-ifelse(price_room$Pattern_room==1,'1',ifelse(price_room$Pattern_room==2,'2',ifelse(price_room$Pattern_room==3,'3',ifelse(price_room$Pattern_room==4,'4',ifelse(price_room$Pattern_room==5,'5',ifelse(price_room$Pattern_room==6,'6','< 6'))))))

qplot(x=log10(Price_total),                             
      data=price_room,                     
      geom="density",        # 圖形=density
      xlab="log10(Price_total)",
      ylab="density",                         
      color= room_level,
      fill= room_level,
      alpha = 0.2,
      family = "KaiTi"
      # 以顏色標註月份，複合式的機率密度圖
)+theme_bw()


############### anova  ###############



# Building_Apt-general:公寓
# Building_Apt-WS:華夏
# Building_Apt-luxury:大樓
# Building_House:透天
# Building_Suit:套房


### 根據前面分析，設距離廟宇200、350、500

### 公寓

new_df <- KH[KH$temple_distant<=200|KH$temple_distant<=350|KH$temple_distant<=500,]

kh_df_公寓 <- new_df[new_df$房屋類別=='公寓',]

test1<-aov(Price_total ~ temple_distant,data =kh_df_公寓)
summary(test1)


### 大樓

new_df <- KH[KH$temple_distant<=200|KH$temple_distant<=350|KH$temple_distant<=500,]

kh_df_大樓 <- new_df[new_df$房屋類別=='大樓',]



test1<-aov(Price_total ~ temple_distant,data =kh_df_大樓)
summary(test1)

### 華夏

new_df <- KH[KH$temple_distant<=200|KH$temple_distant<=350|KH$temple_distant<=500,]

kh_df_華夏 <- new_df[new_df$房屋類別=='華夏',]

test1<-aov(Price_total ~ temple_distant,data =kh_df_華夏)
summary(test1)




### 透天

new_df <- KH[KH$temple_distant<=200|KH$temple_distant<=350|KH$temple_distant<=500,]

kh_df_透天 <- new_df[new_df$房屋類別=='透天',]

test1<-aov(Price_total ~ temple_distant,data =kh_df_透天)
summary(test1)

#### 使用類型

KH$Using_level<- ifelse(KH$`Using type_B` ==1,'商業',ifelse(KH$`Using type_Fac`==1,'工業',ifelse(KH$`Using type_Far`==1,'農用',ifelse(KH$`Using type_L`==1,'住宅','Other'))))


new_df <-KH[KH$temple_distant<=200|KH$temple_distant<=350|KH$temple_distant<=500,]


### 商業
df<-new_df[new_df$Using_level=='商業',]
test1<-aov(Price_total ~ temple_distant,data =df )
summary(test1)


### 工業

df<-new_df[new_df$Using_level=='工業',]
test1<-aov(Price_total ~ temple_distant,data =df )
summary(test1)


### 農用

df<-new_df[new_df$Using_level=='農用',]
test1<-aov(Price_total ~ temple_distant,data =df )
summary(test1)



### 住宅
df<-new_df[new_df$Using_level=='住宅',]
test1<-aov(Price_total ~ temple_distant,data =df )
summary(test1)

### 其他

df<-new_df[new_df$Using_level=='Other',]
test1<-aov(Price_total ~ temple_distant,data =df )
summary(test1)


#年齡


KH$Building_age<- ifelse(KH$Building_age<=10,'>10 years',ifelse(KH$Building_age<=20 &KH$Building_age>10,"11-20 years",ifelse(KH$Building_age<=30 &KH$Building_age>=21,"21-30 years",ifelse( KH$Building_age>30,">30 years","others"))))

new_df <- KH[KH$temple_distant<=200|KH$temple_distant<=350|KH$temple_distant<=500,]



### >10 years

df<-new_df[new_df$Building_age=='>10 years',]
test1<-aov(Price_total ~ temple_distant,data =df )
summary(test1)


### 11-20 years

df<-new_df[new_df$Building_age=='11-20 years',]
test1<-aov(Price_total ~ temple_distant,data =df )
summary(test1)



### 21-30 years

df<-new_df[new_df$Building_age=='21-30 years',]
test1<-aov(Price_total ~ temple_distant,data =df )
summary(test1)






