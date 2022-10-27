library(readr)
library(dplyr)
library(tidyr)

setwd("E:/R/_cp2022/saha/data")

train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")
all0<-rbind(train,test)


ed<-read_csv("Education.csv")
ed<-ed[ed$id%in%c(all0$id),]
ed[is.na(ed)]<-"Ed_Unknown"

#ed.g<-ed%>%group_by(id,`Табельный номер руководителя`, type=`Вид образования`, Специальность)%>%summarise(n=n())



ed.g<-ed%>%group_by(id,`Табельный номер руководителя`, type=`Вид образования`)%>%summarise(n=n())
ed.g.wide<-ed.g%>%pivot_wider(names_from = type, values_from = n, values_fill = 0 )
Nall<-apply(ed.g.wide[,-c(1,2)],1, sum)
Nuni<-apply(ed.g.wide[,-c(1,2)],1, function (x) sum(x>0))

ed.g.wide$Nall<-Nall
ed.g.wide$Nuni<-Nuni
ed.g.wide$boss<-ifelse(ed.g.wide$`Табельный номер руководителя`==ed.g.wide$id,1,0)

colnames(ed.g.wide)[-1]<-paste0("education_",colnames(ed.g.wide)[-1])


save(ed.g.wide, file="educ.RDA")
