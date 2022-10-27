library(readr)
library(dplyr)
library(tidyr)

setwd("E:/R/_cp2022/saha/data")

train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")
all0<-rbind(train,test)


TN<-read_csv("TimenNetwork.csv")
TN<-unique(TN[TN$id%in%c(all0$id),])
#tTN<-unique(TN)

TN.grp<-TN%>%group_by(id)%>%summarise(
  TN_budni=mean(`Вых/Будни`=="Будни"),
  TN_vyh=mean(`Вых/Будни`!="Будни"),
  TN_budni_time=mean((monitor_Time)[`Вых/Будни`=="Будни"]),
  TN_vyh_time=mean((monitor_Time)[`Вых/Будни`!="Будни"]),
 # TN_vyh_time=mean(`Вых/Будни`!="Будни"),
  
  TN_n=n())

TN.grp[is.na(TN.grp)]<-0

save(TN.grp, file="TN.RDA")

