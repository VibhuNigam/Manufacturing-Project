library(dplyr)
library(tidyr)
library(visdat)
library(MASS)




setwd('D:\\Edvancer\\R\\Manufacturing')



#Read Data
manu_train= read.csv('product_train.csv', stringsAsFactors = F)
manu_test= read.csv('product_test.csv', stringsAsFactors = F)


#Creating NAs for manu_test and creating new data columns for manu_test and manu_train

manu_test$went_on_backorder= NA
manu_test$data="test"
manu_train$data="train"

#binding test and train column
final=rbind(manu_train, manu_test)

#Data information training

summary(final)

table(final$went_on_backorder)

vis_dat(final, warn_large_data = FALSE)

#Cleaning data and creating dummy variables

#Create dummies function

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}



#Creating dummies for the 6 variables excluding went_on_backorder

table(final$potential_issue)
final= CreateDummies(final, "potential_issue")

table(final$deck_risk)
final= CreateDummies(final,'deck_risk')

table(final$ppap_risk)
final= CreateDummies(final, 'ppap_risk')

table(final$stop_auto_buy)
final= CreateDummies(final, 'stop_auto_buy')

table(final$rev_stop)
final= CreateDummies(final, 'rev_stop')

table(final$oe_constraint)
final= CreateDummies(final, 'oe_constraint')



vis_dat(final, warn_large_data = FALSE)
summary(final)
glimpse(final)

#splitting data back into test and train 

manu_train=final %>% filter(data=='train') %>% select(-data)

vis_dat(manu_train, warn_large_data = FALSE)


manu_test= final %>% filter(data=='test') %>% select (-data,-went_on_backorder)


vis_dat(manu_test, warn_large_data = FALSE)

##Loading automl

library(h2o)

h2o.init(nthreads= 6)

#import a sample binary outcome trai/test set into H2o

train=as.h2o(manu_train)
test=as.h2o(manu_test)

#variable to predict

y= "went_on_backorder"


##All other columns

x=setdiff(names(train), y)

#for binary classification, response should be a factor
train[,y] = as.factor(train[,y])

##Running automl for 20 base models

aml= h2o.automl(x=x, y=y, training_frame= train, max_models= 10)

lb = aml@leaderboard
print(lb, n=nrow(lb))

m=h2o.get_best_model(aml)

x=as.data.frame(h2o.predict(m,test))

x$sku=manu_test$sku

colnames(x)[1]="went_on_backorder"

x=x[,c("sku", "went_on_backorder")]

write.csv(x,"Vibhu_nigam_p3_part2.csv", row.names=F)
           