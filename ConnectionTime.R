library(readr)
library(dplyr)
library(tidyr)

setwd("E:/R/_cp2022/saha/data")
train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")
all0<-rbind(train,test)

####
CT<-read_csv("ConnectionTime.csv")
CT<-unique(CT[CT$id%in%all0$id,])
CT$dateNum<-as.POSIXct(substr(CT$dateNum,1,10))
CT$maxLogOff<-as.POSIXct(CT$maxLogOff)
CT$`Нормативное время начала раб.дня`<-as.POSIXct(paste(CT$dateNum, 
                                             substr(CT$`Нормативное время начала раб.дня`,12,19)))

CT$`Фактич. время начала раб.дня`<-as.POSIXct(paste(CT$dateNum, 
                                                        substr(CT$`Фактич. время начала раб.дня`,12,19)))

#CT$opozd<-(CT$`Фактич. время начала раб.дня`-CT$`Нормативное время начала раб.дня`)/3600

CT$worktime<-as.numeric((CT$maxLogOff-CT$`Фактич. время начала раб.дня`)/60)

CT[is.na(CT)]<-"0"


CT$`Время опоздания`<-as.numeric(gsub(",",".",CT$`Время опоздания`))


CT.grp<-CT%>%group_by(id)%>%summarise(OpozdMean1=mean(`Время опоздания`),
                                      OpozdMean2=mean(`Время опоздания`[`Время опоздания`>0]),
                                      OpozdPerc=mean(`Признак опоздания`!=0),
                                      BudnPerc=mean(`Вых/Будни`=="Будни"),
                                      WorkTime=mean(worktime),
                                      nCT=n())
                                                      

colnames(CT.grp)[-1]<-paste0("CT_",colnames(CT.grp)[-1])


save(CT.grp, file="CT.RDA")
