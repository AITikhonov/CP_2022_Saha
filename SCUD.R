library(readr)
library(dplyr)
library(tidyr)

setwd("E:/R/_cp2022/saha/data")

train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")
all0<-rbind(train,test)


skud<-read_csv("skud.csv")
skud<-unique(skud[skud$id%in%c(all0$id),])
skud$`Длительность общая`<-as.numeric(gsub(",",".",skud$`Длительность общая`))
skud$`Длительность раб.дня без обеда`<-as.numeric(gsub(",",".",skud$`Длительность раб.дня без обеда`))
#skud$dlit<-(as.POSIXct(skud$Уход.1)-as.POSIXct(skud$Приход.1))/60


skud.grp<-skud%>%group_by(id)%>%
  summarise(skud_mean_dlit=mean(`Длительность общая`),
            skud_mean_dlit_woDin=mean(`Длительность раб.дня без обеда`),
            skud_perc_work=mean(`Длительность раб.дня без обеда`/`Длительность общая`), 
            skud_perc_budn=mean(`Вых/Будни`=="Будни"),
            skud_N=n())

save(skud.grp, file="scud.RDA")
