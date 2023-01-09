rm(list = ls());gc()
pacman::p_load(data.table,dplyr,tidyr,stringr,ggplot2,plotly,lubridate,vcd,gridExtra,
               car,readr, sf, tmap,highcharter,RColorBrewer,magrittr,knitr,fastDummies)
pacman::p_load(MatchIt,stargazer,cobalt,eatATA)
pacman::p_load(quantreg)
library("MatchIt","stargazer")
library("quantreg")
pals16 = c(brewer.pal(9,"Set1"),brewer.pal(8,"Set1")[1:4])
library(readxl)
library("eatATA")
setwd("C:/Users/88697/Desktop/房地產資料")

#####讀入高雄市房市交易資料#####  
hp <- fread("KH_v51.csv", encoding = 'UTF-8', 
            header = T)
house <- st_as_sf(hp, coords=c("X","Y"), crs=4326) %>% 
  st_transform(crs=3826)

###清house欄位資料
which(colnames(house) %in% c("U_ID1","distance_airport"))
colnames(house)[58:91]
house=house[,-c(58:91)]
which(colnames(house) %in% c("Town_Year","Li_SSSim"))
colnames(house)[61:65]
house=house[,-c(61:65)]
#load("distancedata.rdata")
####欄位名稱重新整理####
colnames(house)[14]="Type_parking_space"
colnames(house)[16]="Type_building_without_PS"
colnames(house)[17]="Type_building_with_PS"
colnames(house)[18]="Type_building_price_inclued_PS"
colnames(house)[19]="Parking_space_total_area"
colnames(house)[20]="Parking_space_price"
colnames(house)[21]="Transfer_area_land"
colnames(house)[22]="Transfer_area_building"
colnames(house)[23]="Using_type_L"
colnames(house)[24]="Using_type_B"
colnames(house)[25]="Using_type_Far"
colnames(house)[26]="Using_type_Fac"
colnames(house)[27]="Using_type_O"
colnames(house)[28]="Land_usage_city"
colnames(house)[29]="Land_usage_nonecit"
colnames(house)[30]="Land_usage_mt_preservation"
colnames(house)[36]="Building_Apt_general"
colnames(house)[37]="Building_Apt_luxury"
colnames(house)[38]="Building_Apt_WS"
gsub("\\-|\\ ","_",colnames(house))#正規表達式

#######併入距離工業區##########
# load("dis_mx.rdata")
# remove_f=c( "南部科學園區橋頭園區","裕鐵企業路竹產業園區","大井泵浦工業股份有限",
#             "新材料循環產業園區","仁武產業園區","林園高值化產業園區","楠梓加工出口園區二期" )
# mx=mx[,which(!colnames(mx) %in% remove_f)]
# small_dis=matrix(ncol=2,nrow=nrow(mx))
# colnames(small_dis)=c("distance_small","distance_airport")
# for(j in 1:nrow(mx)){
#   sid=row.names(mx)[j]%>%as.integer()
#   year=hp$TradeYear[hp$`Seq_no-old`==sid]
#   dis=mx[j,1:(ncol(mx)-1)]
#   if(year>104&&!is.na(year))
#   {dis=dis[-which(colnames(mx)=="後勁工業區")]}
#   small_dis[j,1]=min(dis,na.rm = T);
#   small_dis[j,2]=mx[j,ncol(mx)];
# }
# 
# house=bind_cols(house,as.data.frame(small_dis))
####工業區原始資料####
g1= st_read("pic.shp", crs=3826,options = "ENCODING=UTF8")
g2=g1%>%filter(str_extract_all(g1$FD,regex("E", ignore_case=F))=="E")

zhuyin= st_read("8.shp", crs=3826,options = "ENCODING=UTF8")  
zhuyin=zhuyin[,6:12];colnames(zhuyin)=colnames(g2)
zhuyin$FNAME="後勁工業區"
g2=rbind(g2,zhuyin)


####分類工業區種類後再選取距離####
g3=g2
g3$geometry=NULL
g3$level="other"
g3$level[g3$FD %in% c("E02","E10","E11","E14","E15")]="SIZ"
g3$level[g3$FD %in% c("E03","E04","E06","E07","E08","E09","E16")]="EPZ"
g3$level[42]="SIZ"

load("dis_mx.rdata")
#含後勁
remove_f=c( "南部科學園區橋頭園區","裕鐵企業路竹產業園區","大井泵浦工業股份有限",
            "新材料循環產業園區","仁武產業園區","林園高值化產業園區")
mx=mx[,which(!colnames(mx) %in% remove_f)]

#不含後勁
remove_f1=c( "南部科學園區橋頭園區","裕鐵企業路竹產業園區","大井泵浦工業股份有限",
            "新材料循環產業園區","仁武產業園區","林園高值化產業園區", "後勁工業區")
mx1=mx[,which(!colnames(mx) %in% remove_f1)]

small_dis=matrix(ncol=1+n_distinct(g3$level),nrow=nrow(mx1))
small_dis_name=matrix(ncol=n_distinct(g3$level),nrow=nrow(mx1))
colnames(small_dis)=c(unique(g3$level),"distance_airport")
colnames(small_dis_name)=c(unique(g3$level))
level=unique(g3$level)
for(i in 1:n_distinct(g3$level)){
  level_c=level[i]
  mx2=mx1[,which(colnames(mx1) %in% g3$FNAME[g3$level==level_c])]
for(j in 1:nrow(mx2)){
  sid=row.names(mx2)[j]%>%as.integer()
  year=hp$TradeYear[hp$`Seq_no-old`==sid]
  dis=mx2[j,]
  small_dis[j,which(colnames(small_dis)==level_c)]=min(dis,na.rm = T)
  small_dis_name[j,which(colnames(small_dis_name)==level_c)]=colnames(mx2)[which(dis==min(dis,na.rm = T))[1]]
}
}

for(j in 1:nrow(mx1)){
  small_dis[j,ncol(small_dis)]=mx1[j,ncol(mx1)]
  
}
house=bind_cols(house,as.data.frame(small_dis))

small_dis_name<-small_dis_name %>% as.data.frame()
####單抓取距離後勁工業區距離####
dim(mx)
house=bind_cols(house,mx[,which(colnames(mx)== "後勁工業區")])
colnames(house)[77]="後勁工業區"

####研究用#####
house<-house %>% mutate(SIZ_y=ifelse(TradeYear<=104&`後勁工業區`<SIZ,`後勁工業區`,SIZ))
house<-house %>% mutate(dsf=ifelse((EPZ<other)&(EPZ<SIZ_y),EPZ,ifelse(other<SIZ_y,other,SIZ_y)))
house<-house %>% mutate(dsf_type=ifelse((EPZ<other)&(EPZ<SIZ_y),"EPZ",ifelse(other<SIZ_y,"other","SIZ_y")))

house<-house %>% mutate(dsf_f=ifelse(dsf<=500,500,ifelse(dsf<=1000,1000,ifelse(dsf<=1500,1500,2000))))#增加距離虛擬變數

house$dsf_f_type=paste0(house$dsf_f,house$dsf_type)
house=fastDummies::dummy_cols(house, select_columns = "dsf_f_type")

#house[house$dsf_f_type_NANA==1,]%>%View

#######併入距離交流道##########
load("dis_road2.rdata")
small_dis_r=matrix(ncol=1,nrow=nrow(mx))
colnames(small_dis_r)=c("distance_small_r")
for(j in 1:nrow(mx)){
  small_dis_r[j,]=min(mx[j,])
}

house=bind_cols(house,as.data.frame(small_dis_r))

#######併入距離火車站##########
load("dis_train2.rdata")
small_dis_t=matrix(ncol=1,nrow=nrow(mx))
colnames(small_dis_t)=c("distance_small_t")
remove_t=c( "臺鐵內惟車站" ,"臺鐵美術館車站","臺鐵鼓山車站" ,
            "臺鐵三塊厝車站" ,"臺鐵民族車站" ,"臺鐵科工館車站","臺鐵正義車站" )
for(j in 1:nrow(mx)){
  sid=row.names(mx)[j]%>%as.integer()
  month=hp$TradeYM[hp$`Seq_no-old`==sid]
  dis=mx[j,]
  if(month<10711&&!is.na(month))
  {dis=dis[-which(colnames(mx)%in%remove_t)]}
  small_dis_t[j,]=min(dis)
}

house=bind_cols(house,as.data.frame(small_dis_t))

#######併入距離輕軌站##########
load("dis_Light_Rail2.rdata")
# colnames(house)
# colnames(house)[80:103]
# house=house[,-c(80:103)]

remove_f2=c("高雄環狀輕軌壽山公園站_C15","高雄環狀輕軌文武聖殿站_C16","高雄環狀輕軌鼓山區公所站_C17",
            "高雄環狀輕軌鼓山站_C18(興建中)","高雄環狀輕軌九如四路站_C19(興建中)","高雄環狀輕軌台鐵美術館站_C20(興建中)",
            "高雄環狀輕軌美術園區站_C21(興建中)","高雄環狀輕軌聯合醫院站_C22(興建中)","高雄環狀輕軌龍德路站_C23(興建中)",
            "高雄環狀輕軌新市政中心站_C24(興建中)","高雄環狀輕軌龍華國中站_C25(興建中)","高雄環狀輕軌大統新世紀站_C26(興建中)",
            "高雄環狀輕軌鼎山站_C27(興建中)","高雄環狀輕軌灣子內站_C28(興建中)","高雄環狀輕軌建興路站_C29(興建中)",
            "高雄環狀輕軌大順九如站_C30(興建中)","高雄環狀輕軌環球影城站_C31(興建中)","高雄環狀輕軌凱旋公園站_C32" ,
            "高雄環狀輕軌衛生局站_C33","高雄環狀輕軌五權國小站_C34", "高雄環狀輕軌凱旋武昌站_C35","高雄環狀輕軌凱旋二聖站_C36",
            "高雄環狀輕軌台鐵美術館站_C20(興建中)","高雄環狀輕軌聯合醫院站_C22(興建中)", "高雄環狀輕軌新市政中心站_C24(興建中)",
            "高雄環狀輕軌機廠站_C37"  )

mx=mx[,which(!colnames(mx) %in% remove_f2)]
small_dis_LR=matrix(ncol=1,nrow=nrow(mx))
colnames(small_dis_LR)=c("distance_small_LR")
remove_lt1=c( "高雄環狀輕軌籬仔內站_C1","高雄環狀輕軌凱旋瑞田站_C2","高雄環狀輕軌前鎮之星站_C3", "高雄環狀輕軌凱旋中華站_C4" )
remove_lt2=c( "高雄環狀輕軌夢時代站_C5" ,"高雄環狀輕軌經貿園區站_C6","高雄環狀輕軌軟體園區站_C7","高雄環狀輕軌高雄展覽館站_C8"
              ,"高雄環狀輕軌旅運中心站_C9" ,"高雄環狀輕軌光榮碼頭站_C10" ,"高雄環狀輕軌真愛碼頭站_C11", "高雄環狀輕軌駁二大義站_C12" )
remove_lt3=c( "高雄環狀輕軌駁二蓬萊站_C13","高雄環狀輕軌哈瑪星站_C14" )

for(j in 1:nrow(mx)){
  sid=row.names(mx)[j]%>%as.integer()
  month=hp$TradeYM[hp$`Seq_no-old`==sid]
  dis=mx[j,]
  if(month<10411&&!is.na(month)){dis=dis[-which(names(dis)%in%remove_lt1)]}
  if(month<10507&&!is.na(month)){dis=dis[-which(names(dis)%in%remove_lt2)]}
  if(month<10610&&!is.na(month)){dis=dis[-which(names(dis)%in%remove_lt3)]}
  if(length(dis)==0){dis=10000}
  small_dis_LR[j,]=min(dis)
}

house=bind_cols(house,as.data.frame(small_dis_LR))
#colnames(house)[80]="small_dis_LR"
#colnames(house)[84]="small_dis_LR_Star10411"
#save(house,file = "distancedata.Rdata")
#load("distancedata.Rdata")

####選取距離區間lnhouse101_109D<- lnhouse101_109D%>% filter(distance<=1000)####
# dsizin500=ifelse((house$SIZ>=0)&(house$SIZ<500),1,0)#增加距離虛擬變數
# dsizin1000=ifelse((house$SIZ>=500)&(house$SIZ<1000),1,0)
# dsizin1500=ifelse((house$SIZ>=1000)&(house$SIZ<1500),1,0)
# depzin500=ifelse((house$EPZ>=0)&(house$EPZ<500),1,0)
# depzin1000=ifelse((house$EPZ>=500)&(house$EPZ<1000),1,0)
# depzin1500=ifelse((house$EPZ>=1000)&(house$EPZ<1500),1,0)
# dotherin500=ifelse((house$other>=0)&(house$other<500),1,0)
# dotherin1000=ifelse((house$other>=500)&(house$other<1000),1,0)
# dotherin1500=ifelse((house$other>=1000)&(house$other<1500),1,0)
# dsizout1500=ifelse((house$SIZ>=1500),1,0)
# depzout1500=ifelse((house$EPZ>=1500),1,0)
# dotherout1500=ifelse((house$other>=1500),1,0)
# dout1500=ifelse((dsizout1500+depzout1500+dotherout1500==3),1,0)
dAin500=ifelse((house$distance_airport<500),1,0)
dRin500=ifelse((house$distance_small_r<500),1,0)
dTin500=ifelse((house$distance_small_t<500),1,0)
dLRin500=ifelse((house$distance_small_LR<500),1,0)
distance <- data.frame(dAin500=dAin500,dRin500=dRin500,dTin500=dTin500,dLRin500=dLRin500)
house=bind_cols(house,distance)
#save(Total_no_na,file = "Total_no_na.Rdata")
#########匯入整併計算距離及選取時間區間##########
house101_109<- house%>% filter(TradeYear%in%101:109)#選取區間年分
house101_109D=house101_109#串接2份資料在房價資料增加distance欄位
#########設區虛擬變數########
house101_109D=fastDummies::dummy_cols(house101_109D, select_columns = "Location_Town")
removetown=paste0("Location_Town_",c("田寮區","旗山區","六龜區","甲仙區","杉林區","美濃區","內門區","茂林區","桃源區","那瑪夏區"))
house101_109D=select(house101_109D,-removetown)

#########變數重分類##########
removetype=c("Using_type_Far","Using_type_Fac","Using_type_O")
removetype_using=house101_109D[,removetype]
removetype_using$geometry=NULL
house101_109D$Using_type_O=rowSums(removetype_using[,c(1:3)])
table(house101_109D$Using_type_O)
house101_109D=select(house101_109D,-removetype[1:2])

#is.na(house101_109D$Price_total)%>%sum()###檢查有沒有NA值
#range(lnhouse101_109D$lnPrice_total)
#lnhouse101_109D[lnhouse101_109D$lnPrice_total==0,]%>%View()###檢查資料有無0
###踢掉0
house101_109D=filter(house101_109D,Price_total>30)
lnhouse101_109D<-mutate(house101_109D,lnPrice_total=log(Price_total))#房價取ln
lnhouse101_109D=fastDummies::dummy_cols(lnhouse101_109D, select_columns = "TradeYQ")#增加時間固定效果
Totalall<-lnhouse101_109D %>% select(-c("TradeYQ_10904"))

###踢掉土地買賣
Total1=filter(Totalall,Type_parking_space!=1)
Total2=filter(Total1,Type_land!=1)
Total3=filter(Total2,Building_age_nodata!=1)
Total4=filter(Total3,Building_Others!=1)
Total5=filter(Total4,TtFloor_nodata!=1)
Total6=filter(Total5,Pattern_bath<10)
Total7=filter(Total6,Pattern_hall<10)
Total8=filter(Total7,Pattern_room<10)
Total8=filter(Total8,Price_total<20000)
#Total=filter(Total,Building_age>0)
#sort(Total$Price_unit)
#看資料筆數
# table(Total$Building_Apt_WS)#華廈
# table(Total$Building_Apt_luxury)#大樓
# table(Total$Building_House)#透天
# table(Total$TradeYear)#看年資料筆數
###年區間
t1=ifelse((Total8$TradeYear>=101)&(Total8$TradeYear<102),1,0)#增加時間固定效果
t2=ifelse((Total8$TradeYear>=102)&(Total8$TradeYear<103),1,0)
t3=ifelse((Total8$TradeYear>=103)&(Total8$TradeYear<104),1,0)
t4=ifelse((Total8$TradeYear>=104)&(Total8$TradeYear<105),1,0)
t5=ifelse((Total8$TradeYear>=105)&(Total8$TradeYear<106),1,0)
t6=ifelse((Total8$TradeYear>=106)&(Total8$TradeYear<107),1,0)
t7=ifelse((Total8$TradeYear>=107)&(Total8$TradeYear<108),1,0)
t8=ifelse((Total8$TradeYear>=108)&(Total8$TradeYear<109),1,0)
TradeYear <- data.frame(t1=t1, t2=t2, t3=t3, t4=t4, t5=t5, t6=t6, t7=t7, t8=t8)
Total_year<-bind_cols(Total8,TradeYear)

#檢查NA值
lapply(Total_year,function(x){
  y=is.na(x);return(table(y))
})

Total_no_na= na.omit(Total_year)

colnames(Total_no_na)

removetown2=c("田寮區","旗山區","六龜區","甲仙區","杉林區","美濃區","內門區","茂林區","桃源區","那瑪夏區")
Total_no_na=filter(Total_no_na,!Location_Town %in% removetown2)

house=filter(house,!is.na(TradeYear))

####star####

#Total_no_na2=filter(Total_no_na,!distance_small <=500)#排除500M以內的
#Total_no_na3=filter(Total_no_na2,!distance_small <=1000)#排除1000M以內的
save(Total_no_na ,small_dis_name,g2,file = "Total_no_na.Rdata")
#write.csv(Total_no_na,file="Total_no_na0518.csv")
load("distancedata.Rdata")
load("Total_no_na.Rdata")
write.csv(house,file="house0615.csv")

