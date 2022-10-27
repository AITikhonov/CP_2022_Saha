library(readr)
library(dplyr)
library(tidyr)

setwd("E:/R/_cp2022/saha/data")
train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")
all0<-rbind(train,test)

####
calls<-read_csv("Calls.csv")
calls<-unique(calls[calls$id%in%all0$id,])
#calls$Date2<-as.Date(calls$Date)
#calls$wd<-wday(calls$Date2)

calls$CallTime<-as.numeric(gsub(",",".",calls$CallTime))

calls$`Вид учета времени`[is.na(calls$`Вид учета времени`)]<-"unknown"

calls.grp<-calls%>%group_by(id)%>%summarise(Time=mean(CallTime,na.rm=T),
                                            Number=mean(NumberOfCalls,na.rm=T),
                                            Budn=mean(`Вид учета времени`=="Будни",na.rm=T),
                                            Vyh=mean(`Вид учета времени`=="Выходные дни",na.rm=T)  ,
                                            unknown=mean(`Вид учета времени`=="unknown",na.rm=T)  ,
                                            In=mean(InOut=="ToUser",na.rm=T),
                                            Out=mean(InOut!="ToUser",na.rm=T),
                                            nCalls=n())
                                                      

colnames(calls.grp)[-1]<-paste0("calls_",colnames(calls.grp)[-1])

save(calls.grp, file="calls.RDA")
