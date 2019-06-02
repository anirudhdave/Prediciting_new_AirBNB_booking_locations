##Project-2## RF Model##

library(base)
library(stats)
library(dplyr)
library(lubridate)
library(caret)
library(dataQualityR)
library(rpart)
library(rpart.plot)
library(xgboost)
library(magrittr)
library(Matrix)
library(readr)
library(stringr)
library(car)
library(tidyr)
library(randomForest)
library(tree)
library(ISLR)

set.seed(1234)
###Loading Session.csv###
string_na = c("", " ", "-unknown-", "NA")
sessionsdata <- read.csv(file="/Users/navneetpoddar/Desktop/Academics/2nd Sem/BA/Project2/Datasets/sessions.csv", header = T, na.strings = string_na)

###Loading Train.csv###
traindata <- read.csv("/Users/navneetpoddar/Desktop/Academics/2nd Sem/BA/Project2/Datasets/train_users_2.csv", header = T)

####Test.csv###
##testdata <- read.csv("/Users/navneetpoddar/Desktop/Academics/2nd Sem/BA/Project2/Datasets/test_users.csv", header = T)
#str(testdata)
#summary(testdata)

###cleaning session time##
sessionsdata <- subset(sessionsdata, user_id != "")
sesexp <- sessionsdata
sesexp$device_type <- as.character(sesexp$device_type)

sesexp$windows <- 0
index1 <- which(sesexp$device_type ==  "Windows Desktop")
sesexp$windows[index1] <- 1
index2 <- which(sesexp$device_type ==  "Windows Phone")
sesexp$windows[index2] <- 1

sesexp$ios <- 0
index3 <- which(sesexp$device_type ==  "Mac Desktop")
sesexp$ios[index3] <- 1
index4 <- which(sesexp$device_type ==  "iPhone")
sesexp$ios[index4] <- 1
index5 <- which(sesexp$device_type ==  "iPad Tablet")
sesexp$ios[index5] <- 1
index6 <- which(sesexp$device_type ==  "iPodtouch")
sesexp$ios[index6] <- 1

sesexp$android <- 0
index7 <- which(sesexp$device_type ==  "Android Phone")
sesexp$android[index7] <- 1
index8 <- which(sesexp$device_type ==  "Android App Unknown Phone/Tablet")
sesexp$android[index8] <- 1
index9 <- which(sesexp$device_type ==  "Chromebook")
sesexp$android[index9] <- 1

sesexp$otherdevices <- 0
index10 <- which(sesexp$device_type ==  "-unknown-")
sesexp$otherdevices[index10] <- 1
index11 <- which(sesexp$device_type ==  "Linux Desktop")
sesexp$otherdevices[index11] <- 1
index12 <- which(sesexp$device_type ==  "Tablet")
sesexp$otherdevices[index12] <- 1
index13 <- which(sesexp$device_type ==  "Blackberry")
sesexp$otherdevices[index13] <- 1
index14 <- which(sesexp$device_type ==  "Opera Phone")
sesexp$otherdevices[index14] <- 1

sesexp$action_type <- as.character(sesexp$action_type)
sesexp$actiontype.unknown <- 0
index15 <- which(sesexp$action_type == "")
sesexp$actiontype.unknown[index15] <- 1
index15 <- which(sesexp$action_type == "-unknown-")
sesexp$actiontype.unknown[index15] <- 1

sesexp$actiontype.click <- 0
index15 <- which(sesexp$action_type == "click")
sesexp$actiontype.click[index15] <- 1

sesexp$actiontype.data <- 0
index15 <- which(sesexp$action_type == "data")
sesexp$actiontype.data[index15] <- 1

sesexp$actiontype.view <- 0
index15 <- which(sesexp$action_type == "view")
sesexp$actiontype.view[index15] <- 1

sesexp$actiontype.submit <- 0
index15 <- which(sesexp$action_type == "submit")
sesexp$actiontype.submit[index15] <- 1

sesexp$actiontype.messagepost <- 0
index15 <- which(sesexp$action_type == "message_post")
sesexp$actiontype.messagepost[index15] <- 1

sesexp$actiontype.bookingrequest <- 0
index15 <- which(sesexp$action_type == "booking_request")
sesexp$actiontype.bookingrequest[index15] <- 1

sesexp$actiontype.partnercallback <- 0
index15 <- which(sesexp$action_type == "partner_callback")
sesexp$actiontype.partnercallback[index15] <- 1

sesexp$actiontype.bookingresponse <- 0
index15 <- which(sesexp$action_type == "booking_response")
sesexp$actiontype.bookingresponse[index15] <- 1

sesexp$actiontype.modify <- 0
index15 <- which(sesexp$action_type == "modify")
sesexp$actiontype.modify[index15] <- 1

x <- data.frame(sesexp$user_id, sesexp$action, sesexp$action_detail, sesexp$windows, sesexp$ios, sesexp$android, sesexp$otherdevices, sesexp$actiontype.unknown, sesexp$actiontype.click, sesexp$actiontype.data, sesexp$actiontype.view, sesexp$actiontype.submit, sesexp$actiontype.messagepost, sesexp$actiontype.bookingrequest, sesexp$actiontype.bookingresponse, sesexp$actiontype.partnercallback, sesexp$actiontype.modify, sesexp$secs_elapsed)
train <- subset(x, is.na(sesexp$secs_elapsed) == F)
test <- subset(x, is.na(sesexp$secs_elapsed) == T)
time <- lm(sesexp.secs_elapsed ~ sesexp.windows + sesexp.ios + sesexp.android + sesexp.otherdevices + sesexp.actiontype.unknown + sesexp.actiontype.click + sesexp.actiontype.data + sesexp.actiontype.view + sesexp.actiontype.submit + sesexp.actiontype.messagepost + sesexp.actiontype.bookingrequest + sesexp.actiontype.bookingresponse + sesexp.actiontype.partnercallback + sesexp.actiontype.modify, data = train)
test$sesexp.secs_elapsed <- predict(time, test)
sessionsdata <- rbind(train,test)
sessionsdata <- sessionsdata[,-2]
sessionsdata <- sessionsdata[,-2]

session_visitors <- sessionsdata %>% group_by(sesexp.user_id) %>% summarize(time_min = (mean(na.omit(sesexp.secs_elapsed)))/60)
colnames(session_visitors)[1] <- 'id'

df_all <- merge(traindata,session_visitors, by = "id", all.x = TRUE)

###handle time_min###
df_all$time_min[is.na(df_all$time_min) == T] <- median(df_all$time_min,na.rm = TRUE)

###Cleaning the dates###
df_all = df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]
df_all$date_account_created <- as.numeric(df_all$date_account_created)

###Handling Age###
summary(df_all$age)
df_all$age[df_all$age<=10 | df_all$age>=90]<- NA
train_age <- subset(df_all, is.na(df_all$age) == F)
test_age <- subset(df_all, is.na(df_all$age) == T)
m_age <- lm(age ~ gender + affiliate_channel + signup_app + first_device_type, train_age)
test_age$age <- predict(m_age,test_age)
df_all <- rbind(train_age,test_age)
df_all$age <- round(df_all$age, 0)

df_all[is.na(df_all)] <- -1


split<-(.8)
trainingRowIndex <- sample(1:nrow(df_all),(split)*nrow(df_all))  
train <- df_all[trainingRowIndex, ] 
test  <- df_all[-trainingRowIndex, ]
str(train)


RF_M = randomForest(as.factor(country_destination) ~ gender + signup_method
                    +signup_flow + language + affiliate_channel + affiliate_provider  + first_affiliate_tracked + signup_app
                      +first_device_type + first_browser + timestamp_first_active,  importance = TRUE,
                      ntrees = 10, data=train)

##ranPred = predict(ranOut, newdata = test2,type = 'prob')

##ranPred = as.data.frame(ranPred)

RF_P = predict(RF_M, newdata=test, type='prob')

RF_P = as.data.frame(RF_P)

predictions1 <- colnames(RF_P)[max.col(RF_P,ties.method="first")]

confusionMatrix(predictions1, test$country_destination)


