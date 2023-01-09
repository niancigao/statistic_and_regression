rm(list = ls());gc()
pacman::p_load(data.table,dplyr,tidyr,stringr,ggplot2,plotly,lubridate,vcd,gridExtra,
               car,readr, sf, tmap,highcharter,RColorBrewer,magrittr,knitr,fastDummies)
pals16 = c(brewer.pal(9,"Set1"),brewer.pal(8,"Set1")[1:4])
library(readxl)
setwd("C:/Users/88697/Desktop/房地產資料")

#####讀入高雄市房市交易資料#####  
hp <- fread("KH_v51.csv", encoding = 'UTF-8', 
               header = T)
house <- st_as_sf(hp, coords=c("X","Y"), crs=4326) %>% 
  st_transform(crs=3826)

###清house欄位資料
which(colnames(house) %in% c("U_ID1","distance_LRT"))
colnames(house)[58:94]
house=house[,-c(58:94)]

which(colnames(house) %in% c("Town_Year","Li_SSSim"))
colnames(house)[58:62]
house=house[,-c(58:62)]

#######工業區及機場資料#####
g1= st_read("pic.shp", crs=3826,options = "ENCODING=UTF8")
g2=g1%>%filter(str_extract_all(g1$FD,regex("E", ignore_case=F))=="E")

###讀入後勁區###
zhuyin= st_read("8.shp", crs=3826,options = "ENCODING=UTF8")  
zhuyin=zhuyin[,6:12];colnames(zhuyin)=colnames(g2)
zhuyin$FNAME="後勁工業區"

###讀入高雄機場###
airport= st_read("MARK_101208.shp", crs=3826,options = "ENCODING=UTF8")    
kao_airport=airport%>%filter(str_extract_all(airport$MARKID, regex("E", ignore_case=F))=="E")
kao_airport=kao_airport[,3:9];colnames(kao_airport)=colnames(g2)
kao_airport[,4:6]=NA
###併入工業區檔###
g2=rbind(g2,zhuyin,kao_airport)

#####併入距離工業區####
house=house
factory=g2
mx=matrix(ncol=nrow(factory),nrow=nrow(house))
colnames(mx)=factory$FNAME;row.names(mx)=house$`Seq_no-old`
for(i in 1:nrow(house)){
  dis_arr=st_distance(house[i,],factory)%>%as.numeric()%>%as.integer()
  mx[i,]=dis_arr
  if(i%%5000==0||i==nrow(house)){
    cat(format(Sys.time(), "%X"),sprintf("[Round:%d]",i),"\n")
    save(mx,file ="dis_mx.rdata",compress = T)
  }
}

#mx[1:10,1:10]看資料形式
load("dis_mx.rdata")
remove_f=c( "南部科學園區橋頭園區","裕鐵企業路竹產業園區","大井泵浦工業股份有限",
            "新材料循環產業園區","仁武產業園區","林園高值化產業園區","楠梓加工出口園區二期" )
mx=mx[,which(!colnames(mx) %in% remove_f)]
small_dis=matrix(ncol=2,nrow=nrow(mx))
colnames(small_dis)=c("distance_small","distance_airport")
for(j in 1:nroType_trading_volume_ratiw(mx)){
  small_dis[j,1]=min(mx[j,1:(ncol(mx)-1)],na.rm = T);
  small_dis[j,2]=mx[j,ncol(mx)];
}

house=bind_cols(house,as.data.frame(small_dis))

######併入距離交流道##########
road1= st_read("ROAD_1110208.shp", crs=3826,options = "ENCODING=UTF8")
road2=road1%>%filter(str_extract_all(road1$ROADSEGID,regex("E", ignore_case=F))=="E")%>%
  filter(str_extract_all(ROADNAME,regex("交流道", ignore_case=F))=="交流道")#選取交流道
#write.csv(road2,file="交流道清單.csv")
house=house
factory=road2
mx=matrix(ncol=nrow(factory),nrow=nrow(house))
colnames(mx)=factory$ROADNAME;row.names(mx)=house$`Seq_no-old`
for(i in 1:nrow(house)){
  dis_arr=st_distance(house[i,],factory)%>%as.numeric()%>%as.integer()
  mx[i,]=dis_arr
  if(i%%5000==0||i==nrow(house)){
    cat(format(Sys.time(), "%X"),sprintf("[Round:%d]",i),"\n")
    save(mx,file ="dis_road2.rdata",compress = T)
  }
}

load("dis_road2.rdata")
small_dis_r=matrix(ncol=1,nrow=nrow(mx))
colnames(small_dis_r)=c("distance_small_r")
for(j in 1:nrow(mx)){
  small_dis_r[j,]=min(mx[j,])
}

house=bind_cols(house,as.data.frame(small_dis_r))

#######併入台鐵車站計算距離#####
train1= st_read("MARK_臺鐵車站_1101208.shp", crs=3826,options = "ENCODING=UTF8")
train2=train1%>%filter(str_extract_all(train1$MARKID,regex("E", ignore_case=F))=="E")
  
house=house
factory=train2
mx=matrix(ncol=nrow(factory),nrow=nrow(house))
colnames(mx)=factory$MARKNAME1;row.names(mx)=house$`Seq_no-old`
for(i in 1:nrow(house)){
  dis_arr=st_distance(house[i,],factory)%>%as.numeric()%>%as.integer()
  mx[i,]=dis_arr
  if(i%%5000==0||i==nrow(house)){
    cat(format(Sys.time(), "%X"),sprintf("[Round:%d]",i),"\n")
    save(mx,file ="dis_train2.rdata",compress = T)
  }
}

load("dis_train2.rdata")
small_dis_t=matrix(ncol=1,nrow=nrow(mx))
colnames(small_dis_t)=c("distance_small_t")
for(j in 1:nrow(mx)){
  small_dis_t[j,]=min(mx[j,])
}

house=bind_cols(house,as.data.frame(small_dis_t))

#######併入輕軌車站計算距離##########
Light_Rail1= st_read("MARK_輕軌捷運車站_1101208.shp", crs=3826,options = "ENCODING=UTF8")
Light_Rail2=Light_Rail1%>%filter(str_extract_all(Light_Rail1$MARKID,regex("E", ignore_case=F))=="E")

house=house
factory=Light_Rail2
mx=matrix(ncol=nrow(factory),nrow=nrow(house))
colnames(mx)=factory$MARKNAME1;row.names(mx)=house$`Seq_no-old`
for(i in 1:nrow(house)){
  dis_arr=st_distance(house[i,],factory)%>%as.numeric()%>%as.integer()
  mx[i,]=dis_arr
  if(i%%5000==0||i==nrow(house)){
    cat(format(Sys.time(), "%X"),sprintf("[Round:%d]",i),"\n")
    save(mx,file ="dis_Light_Rail2.rdata",compress = T)
  }
}

load("dis_Light_Rail2.rdata")

remove_f2=c( "高雄環狀輕軌環球影城站_C31(興建中)","高雄環狀輕軌大順九如站_C30(興建中)","高雄環狀輕軌鼓山站_C18(興建中)",
            "高雄環狀輕軌建興路站_C29(興建中)","高雄環狀輕軌九如四路站_C19(興建中)","高雄環狀輕軌灣子內站_C28(興建中)" ,
            "高雄環狀輕軌台鐵美術館站_C20(興建中)	","高雄環狀輕軌鼎山站_C27(興建中)","高雄環狀輕軌龍德路站_C23(興建中)",
            "高雄環狀輕軌聯合醫院站_C22(興建中)	","高雄環狀輕軌美術園區站_C21(興建中)","高雄環狀輕軌大統新世紀站_C26(興建中)",
            "高雄環狀輕軌新市政中心站_C24(興建中)	","高雄環狀輕軌龍華國中站_C25(興建中)")
mx=mx[,which(!colnames(mx) %in% remove_f2)]

small_dis_LR=matrix(ncol=1,nrow=nrow(mx))
colnames(small_dis_LR)=c("distance_small_LR")
for(j in 1:nrow(mx)){
  small_dis_LR[j,]=min(mx[j,])
}

house=bind_cols(house,as.data.frame(small_dis_LR))

####檢查NA值####
lapply(house,function(x){
  y=is.na(x);return(table(y))
})


