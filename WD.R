library(readr)
library(dplyr)
library(tidyr)

setwd("E:/R/_cp2022/saha/data")

train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")
all0<-rbind(train,test)


WD<-read_csv("WorkingDay.csv")
WD<-unique(WD[WD$id%in%c(all0$id),])


WD.grp<-WD%>%group_by(id)%>%summarise(
  WD_budni=mean(`Вых/Будни`=="Будни"),
  WD_vyh=mean(`Вых/Будни`!="Будни"),
  WD_budni_time_mon=mean((monitorTime)[`Вых/Будни`=="Будни"]),
  WD_vyh_time_mon=mean((monitorTime)[`Вых/Будни`!="Будни"]),
  WD_budni_time_act=mean((activeTime)[`Вых/Будни`=="Будни"]),
  WD_vyh_time_act=mean((activeTime)[`Вых/Будни`!="Будни"]),
  # TN_vyh_time=mean(`Вых/Будни`!="Будни"),
  
  WD_n=n())

WD.grp[is.na(WD.grp)]<-0


WD.grp$WD_percBud<-WD.grp$WD_budni_time_act/WD.grp$WD_budni_time_mon
WD.grp$WD_percvyh<-WD.grp$WD_vyh_time_act/WD.grp$WD_vyh_time_mon
WD.grp[is.na(WD.grp)]<-999999

save(WD.grp, file="WD.RDA")


