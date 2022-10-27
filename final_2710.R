library(readr)
library(dplyr)
library(caret)
library(tidyr)
#library(randomForest)

Rec.all <- function(data, lev = NULL, model = NULL) {
  uni<-unique(data$obs)
  k<-length(uni)
  b<-0
  for  (i in 1:k)
  {
    a<-MLmetrics::Recall (data$obs, data$pred, positive=uni[i])
    if (is.na(a)) a<-0
    b<-b+a
  }
  c(Rec.all=b/i)
  
}  

set.seed(123)
ctrl <- trainControl(method = "cv", summaryFunction = Rec.all,
                     number = 5)




setwd("E:/R/_cp2022/saha/data")

train<-read_csv("train_dataset_train.csv")
test<-read_csv("test_dataset_test.csv")
all0<-rbind(train,test)
all0$type<-as.factor(all0$type)
all0$code<-as.numeric(substr(all0$id,4,4))-1

####JOIN

load("educ.RDA")
length(intersect(train$id, ed.g.wide$id))/ nrow(train) #87%
length(intersect(test$id, ed.g.wide$id))/ nrow(test)

all<-all0%>%left_join(ed.g.wide)
all$`education_Табельный номер руководителя`[is.na(all$`education_Табельный номер руководителя`)]<-"nodata"

load("calls.RDA")
length(intersect(train$id, calls.grp$id))/ nrow(train)#60%
length(intersect(test$id, calls.grp$id))/ nrow(test)

all<-all%>%left_join(calls.grp)

load("CT.RDA")
length(intersect(train$id, CT.grp$id))/ nrow(train)#89%
length(intersect(test$id, CT.grp$id))/ nrow(test)

all<-all%>%left_join(CT.grp)

load("scud.RDA")
length(intersect(train$id, skud.grp$id))/ nrow(train)#40%
length(intersect(test$id, skud.grp$id))/ nrow(test)

all<-all%>%left_join(skud.grp)

load("tasks.RDA")
length(intersect(train$id, tasks.grp$id))/ nrow(train)#74%
length(intersect(test$id, tasks.grp$id))/ nrow(test)

all<-all%>%left_join(tasks.grp)

load("TN.RDA")
length(intersect(train$id, TN.grp$id))/ nrow(train)#87%
length(intersect(test$id, TN.grp$id))/ nrow(test)

all<-all%>%left_join(TN.grp)

load("WD.RDA")
length(intersect(train$id, WD.grp$id))/ nrow(train)#87%
length(intersect(test$id, WD.grp$id))/ nrow(test)

all<-all%>%left_join(WD.grp)
all[is.na(all)]<- -9999999


###############MODEL
allF<-all
allF$`education_Табельный номер руководителя`<-
  as.numeric  (as.factor(all$`education_Табельный номер руководителя`))


tr<-allF[1:550,-1]
tst<-allF[551:812,-1]

tr.gr<-tr%>%group_by(type)%>%summarise(n=n()/nrow(tr))
wei<-tr[,1]%>%left_join(tr.gr)
tr2<-tr%>%select(type,code, calls_unknown, `education_Табельный номер руководителя`)


set.seed(123)
mod<-train(type~., 
           data=tr2, 
           method="xgbTree",  
           metric="Rec.all",
           verbosity=0,
           weights=1/wei$n,
           #num.trees = 1000,
           trControl = ctrl)
max(mod$results$Rec.all)
test$type<-predict(mod,tst)

table(test$type)
table(train$type)

setwd("E:/R/_cp2022/saha/")
write_csv(test, "final_2710.csv")



'Experiments

#tr2<-tr%>%select(type, code,starts_with("calls"))

#tr2<-tr%>%select(type, code,starts_with("education"))
#tr2<-tr%>%select(type, code,starts_with("CT"))
#tr2<-tr%>%select(type, code,starts_with("skud"))
#tr2<-tr%>%select(type, code,starts_with("tasks"))
#tr2<-tr%>%select(type, code,starts_with("TN"))
#tr2<-tr%>%select(type, code,starts_with("WD"))

#tr2<-tr2[,-2]

#tr2<-tr%>%select(type, code,starts_with(c("education","calls")))
#tr2<-tr%>%select(type, code,starts_with(c("education","CT")))

#load("best_feat.RDA")
#k<-10
#feat<-best_feat$`colnames(tr3)[2]1`[1:k]

ans2<-NULL

for (i in 1:2) {
allF$`education_Табельный номер руководителя`<-
  (as.factor(all$`education_Табельный номер руководителя`))


 if (i==1) allF$`education_Табельный номер руководителя`<-
    as.numeric  (as.factor(all$`education_Табельный номер руководителя`))
  
  
 tr<-allF[1:550,-1]
# val<-allF[451:550,-1]
 tst<-allF[551:812,-1]
 
 tr.gr<-tr%>%group_by(type)%>%summarise(n=n()/nrow(tr))
 wei<-tr[,1]%>%left_join(tr.gr)
 
print (i)

#tr2<-tr%>%select(type,all_of(feat))



#tr2<-tr

#отобранны руками
#tr2<-tr%>%select(type,code, calls_unknown,
#                        CT_OpozdMean2,
#                        `education_Табельный номер руководителя`,
#                        skud_N,
#                        tasks_Договор, 
#                 TN_budni,
#WD_vyh
#                 )

tr2<-tr%>%select(type,code, calls_unknown, `education_Табельный номер руководителя`)

#tr2<-tr%>%select(type,code, calls_Budn, `education_Табельный номер руководителя`)

#tr2<-tr%>%select(type,code, calls_Vyh, `education_Табельный номер руководителя`)




#tr2<-tr%>%select(type,code, calls_unknown, `education_Табельный номер руководителя`, skud_N)

#tr2<-tr%>%select(type, calls_unknown, `education_Табельный номер руководителя`)

#tr2<-tr%>%select(type, `education_Табельный номер руководителя`)

#папоротник отобрал
#tr2<-tr%>%select(type, `education_Табельный номер руководителя`, calls_Time, 
#                 calls_Budn, calls_Vyh, calls_In, calls_Out, 
#                 skud_mean_dlit_woDin, tasks_mean_WO_proskok, TN_budni_time)

#xgb Numeric
tr2<-tr%>%select(type,calls_Time,                                                          
`education_Табельный номер руководителя`,
TN_budni_time,                                                        
tasks_unknown,                                                        
skud_mean_dlit,                                                       
education_Nuni,                                                       
tasks_mean_proskok31,                                                 
calls_Budn)  


#xgb Factor
tr2<-tr%>%select(type,calls_Time,                                                          
                 `education_Табельный номер руководителя`,
                 TN_budni_time,                                                        
                 tasks_unknown,                                                        
                 skud_mean_dlit,                                                       
                 education_Nuni,                                                       
                 tasks_mean_proskok31,                                                 
                 calls_Budn,
                 code)    

#xgb
#tr2<-tr%>%select(type,calls_Time,                                                          
#                 `education_Табельный номер руководителя`,
#                 TN_budni_time)    



#tr2<-tr

 ####################

set.seed(123)
mod<-train(type~., 
           data=tr2, 
           method="ranger",  
           metric="Rec.all",
           verbosity=0,
           num.trees = 1000,
         #  tuneGrid=expand.grid(.mtry=1, 
          #                      .splitrule=c("gini","extratrees"), 
           #                     .min.node.size=1),
           trControl = ctrl)


md<-mod$results[which.max(mod$results$Rec.all),]
mean<-md$Rec.all
sd<-md$Rec.allSD
min<-min(mod$resample$Rec.all)
max<-max(mod$resample$Rec.all)
median<-median(mod$resample$Rec.all)

ans<-c("ranger-woweight",mean,sd,min,max,median)
ans2<-rbind(ans2,ans)

print (ans2)

####
set.seed(123)
mod<-train(type~., 
           data=tr2, 
           method="ranger",  
           metric="Rec.all",
           verbosity=0,
           class.weights=1/as.vector(table(tr2$type)/nrow(tr2)),
           num.trees = 1000,
        #   tuneGrid=expand.grid(.mtry=1, 
         #                       .splitrule=c("gini","extratrees"), 
          #                      .min.node.size=1),
           trControl = ctrl)


md<-mod$results[which.max(mod$results$Rec.all),]
mean<-md$Rec.all
sd<-md$Rec.allSD
min<-min(mod$resample$Rec.all)
max<-max(mod$resample$Rec.all)
median<-median(mod$resample$Rec.all)

ans<-c("ranger-weight-class",mean,sd,min,max,median)
ans2<-rbind(ans2,ans)

print (ans2)

#######
set.seed(123)
mod<-train(type~., 
           data=tr2, 
           method="ranger",  
           metric="Rec.all",
           verbosity=0,
           weights=1-wei$n,
           num.trees = 1000,
        #   tuneGrid=expand.grid(.mtry=1, 
         #                       .splitrule=c("gini","extratrees"), 
          #                      .min.node.size=1),
           trControl = ctrl)


md<-mod$results[which.max(mod$results$Rec.all),]
mean<-md$Rec.all
sd<-md$Rec.allSD
min<-min(mod$resample$Rec.all)
max<-max(mod$resample$Rec.all)
median<-median(mod$resample$Rec.all)

ans<-c("ranger-weight-minus",mean,sd,min,max,median)
ans2<-rbind(ans2,ans)

print (ans2)

###########3
set.seed(123)
mod<-train(type~., 
           data=tr2, 
           method="ranger",  
           metric="Rec.all",
           verbosity=0,
           weights=1/wei$n,
           num.trees = 1000,
      #     tuneGrid=expand.grid(.mtry=1, 
       #                         .splitrule=c("gini","extratrees"), 
        #                        .min.node.size=1),
           trControl = ctrl)


md<-mod$results[which.max(mod$results$Rec.all),]
mean<-md$Rec.all
sd<-md$Rec.allSD
min<-min(mod$resample$Rec.all)
max<-max(mod$resample$Rec.all)
median<-median(mod$resample$Rec.all)

ans<-c("ranger-weight-div",mean,sd,min,max,median)
ans2<-rbind(ans2,ans)

print (ans2)

############
#######

set.seed(123)
mod<-train(type~., 
           data=tr2, 
           method="xgbTree",  
           metric="Rec.all",
           verbosity=0,
         #  weights=1-wei$n,
           #num.trees = 1000,
           trControl = ctrl)


md<-mod$results[which.max(mod$results$Rec.all),]
mean<-md$Rec.all
sd<-md$Rec.allSD
min<-min(mod$resample$Rec.all)
max<-max(mod$resample$Rec.all)
median<-median(mod$resample$Rec.all)

ans<-c("xgb-weight-NO",mean,sd,min,max,median)
ans2<-rbind(ans2,ans)

print (ans2)

###########3
set.seed(123)
mod<-train(type~., 
           data=tr2, 
           method="xgbTree",  
           metric="Rec.all",
           verbosity=0,
           weights=1-wei$n,
           #num.trees = 1000,
           trControl = ctrl)


md<-mod$results[which.max(mod$results$Rec.all),]
mean<-md$Rec.all
sd<-md$Rec.allSD
min<-min(mod$resample$Rec.all)
max<-max(mod$resample$Rec.all)
median<-median(mod$resample$Rec.all)

ans<-c("xgbTree-weight-minus",mean,sd,min,max,median)
ans2<-rbind(ans2,ans)

print (ans2)

############
set.seed(123)
mod<-train(type~., 
           data=tr2, 
           method="xgbTree",  
           metric="Rec.all",
           verbosity=0,
           weights=1/wei$n,
           #num.trees = 1000,
           trControl = ctrl)


md<-mod$results[which.max(mod$results$Rec.all),]
mean<-md$Rec.all
sd<-md$Rec.allSD
min<-min(mod$resample$Rec.all)
max<-max(mod$resample$Rec.all)
median<-median(mod$resample$Rec.all)

ans<-c("xgbTree-weight-div",mean,sd,min,max,median)
ans2<-rbind(ans2,ans)

print (ans2)

######

}
write.table(ans2, "clipboard", 
            dec=",", sep="\t", col.names = F, row.names = F)


write.table(t(c(mod$times$everything[3],md$Rec.all, md$Rec.allSD)), "clipboard", 
            dec=",", sep="\t", col.names = F, row.names = F)



#Rec.all(data.frame(obs=val$type, pred=predict(mod,val)))

test$type<-predict(mod,tst)

table(test$type)
table(train$type)

setwd("E:/R/_cp2022/saha/")
write_csv(test, "ferns_bes_fea.csv")
'