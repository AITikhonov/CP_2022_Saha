library(readr)
library(dplyr)
library(tidyr)

setwd("E:/R/_cp2022/saha/data")

train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")
all0<-rbind(train,test)


tasks<-read_csv("tasks.csv")
tasks<-unique(tasks[tasks$id%in%c(all0$id),])


tasks$`Срок плановый`[is.na(tasks$`Срок плановый`)]<-"unknown"
tasks$`Вид документа`[is.na(tasks$`Вид документа`)]<-"unknown"

tasks$`Дата старта задания`<-as.Date(tasks$`Дата старта задания`)
tasks$`Дата завершения задания плановая`<-as.Date(tasks$`Дата завершения задания плановая`)
tasks$`Дата завершения задания фактическая`<-as.Date(tasks$`Дата завершения задания фактическая`)

tasks$srok<-as.numeric(tasks$`Дата завершения задания плановая`-tasks$`Дата старта задания`)
tasks$srok[is.na(tasks$srok)]<- -9999999

tasks$prosrok<-as.numeric(tasks$`Дата завершения задания фактическая`-tasks$`Дата завершения задания плановая`)
tasks$prosrok[is.na(tasks$prosrok)]<- -9999999


tasks.grp<-tasks%>%group_by(id)%>%
  summarise(mean_Srok_out=mean(`Статус по просрочке`=="С нарушением срока"),
            mean_Srok_unknown=mean(`Срок плановый`=="unknown"),
            mean_srok_plan=mean(srok),
            mean_prosrok_ih=mean(`Просрочено, дней`),
            mean_prosrok_my=mean(prosrok),
            mean_proskok7=mean(ДлительностьПросрочки=="до 7 дней"),
            mean_proskok30=mean(ДлительностьПросрочки=="до 30 дней"),
            mean_proskok31=mean(ДлительностьПросрочки=="более 30 дней"),  
            mean_WO_proskok=mean(ДлительностьПросрочки=="без нарушения срока"),

            
                        
                                
  tasks_n=n())



tasks.grp2<-tasks%>%group_by(id, `Вид документа`)%>%
  summarise(n=n())
tasks.grp2_wide<-tasks.grp2%>%
  pivot_wider(names_from =  `Вид документа`, values_from = n, values_fill = 0)
Nall<-apply(tasks.grp2_wide[,-c(1)],1, sum)
Nuni<-apply(tasks.grp2_wide[,-c(1)],1, function (x) sum(x>0))
tasks.grp2_wide[,-1]<-tasks.grp2_wide[,-1]/Nall

#tasks.grp2_wide$Nall_Doc<-Nall
tasks.grp2_wide$Nuni_Doc<-Nuni
#############
tasks.grp3<-tasks%>%group_by(id, `Состояние задания`)%>%
  summarise(n=n())
tasks.grp3_wide<-tasks.grp3%>%
  pivot_wider(names_from = `Состояние задания`, values_from = n, values_fill = 0)
Nall<-apply(tasks.grp3_wide[,-c(1)],1, sum)
Nuni<-apply(tasks.grp3_wide[,-c(1)],1, function (x) sum(x>0))
tasks.grp3_wide[,-1]<-tasks.grp3_wide[,-1]/Nall

#tasks.grp3_wide$Nall_Status<-Nall
tasks.grp3_wide$Nuni_Status<-Nuni


tasks.grp<-tasks.grp%>%left_join(tasks.grp2_wide)

tasks.grp<-tasks.grp%>%left_join(tasks.grp3_wide)

colnames(tasks.grp)[-1]<-paste0("tasks_",colnames(tasks.grp)[-1])
                                
save(tasks.grp, file="tasks.RDA")



