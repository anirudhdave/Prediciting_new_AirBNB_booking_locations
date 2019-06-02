####Business Analytics Project Group Number 5###
###Members: Anirudh Dave, Devang Jain, Navneet Poddar, Pallavi Varandani, Yilin Guan
###Mentor: Jeancarlo Bonilla


###setting working directory###
setwd("D:\\CourseWork\\BA\\Projects\\Project 2")


###Loading Libraries####
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

set.seed(1234)



###Loading the datasets###
###Session.csv###
string_na = c("", " ", "-unknown-", "NA")
sessionsdata <- read.csv("D:\\CourseWork\\BA\\Projects\\Project 2\\DATA\\sessions.csv", header = T, na.strings = string_na)
str(sessionsdata)
summary(sessionsdata)

###Train.csv###
traindata <- read.csv("D:\\CourseWork\\BA\\Projects\\Project 2\\DATA\\train_users_2.csv", header = T)
str(traindata)
summary(traindata)

####Loading Test.csv###
testdata <- read.csv("D:\\CourseWork\\BA\\Projects\\Project 2\\DATA\\test_users.csv", header = T)
str(testdata)
summary(testdata)


###Pre Processiing data sets####
###Separting the coutnry column from the train set into the labeldataset####
labels = traindata['country_destination']
traindata = traindata[-grep('country_destination', colnames(traindata))]


###creating Dummy Variables for sessiondata for predicting null values of column secs_elapsed##
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

###prediction for secs.elapsed###
train <- subset(x, is.na(sesexp$secs_elapsed) == F)
test <- subset(x, is.na(sesexp$secs_elapsed) == T)
time <- lm(sesexp.secs_elapsed ~ sesexp.windows + sesexp.ios + sesexp.android + sesexp.otherdevices + sesexp.actiontype.unknown + sesexp.actiontype.click + sesexp.actiontype.data + sesexp.actiontype.view + sesexp.actiontype.submit + sesexp.actiontype.messagepost + sesexp.actiontype.bookingrequest + sesexp.actiontype.bookingresponse + sesexp.actiontype.partnercallback + sesexp.actiontype.modify, data = train)
test$sesexp.secs_elapsed <- predict(time, test)
sessionsdata <- rbind(train,test)
sessionsdata <- sessionsdata[,-2]
sessionsdata <- sessionsdata[,-2]

###Grouping sessiondata to merge secs_elapsed time in minutes to the train and test data###
session_visitors <- sessionsdata %>% group_by(sesexp.user_id) %>% summarize(time_min = (mean(na.omit(sesexp.secs_elapsed)))/60)
colnames(session_visitors)[1] <- 'id'

###Merging the train, test and session data###
df_train <- merge(traindata,session_visitors, by = "id", all.x = TRUE)
df_test <- merge(testdata, session_visitors, by = "id", all.x = TRUE)
df_all = rbind(df_train,df_test)  ### merged the 

###After the left join there were users with no session time because they were exclusive for train and test which was handle by replacing the median of the remaining
###handle time_min###
df_all$time_min[is.na(df_all$time_min) == T] <- median(df_all$time_min,na.rm = TRUE)

###Cleaning the dates###
### handling dates###
df_all = df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]

dac = as.data.frame(str_split_fixed(df_all$date_account_created, '-', 3))
df_all['dac_year'] = dac[,1]
df_all['dac_month'] = dac[,2]
df_all['dac_day'] = dac[,3]
df_all = df_all[,-c(which(colnames(df_all) %in% c('date_account_created')))]


df_all[,'tfa_year'] = substring(as.character(df_all[,'timestamp_first_active']), 1, 4)
df_all['tfa_month'] = substring(as.character(df_all['timestamp_first_active']), 5, 6)
df_all['tfa_day'] = substring(as.character(df_all['timestamp_first_active']), 7, 8)
df_all = df_all[,-c(which(colnames(df_all) %in% c('timestamp_first_active')))]

###Handling Age###
summary(df_all$age)
df_all$age[df_all$age<=10 | df_all$age>=90]<- NA
train_age <- subset(df_all, is.na(df_all$age) == F)
test_age <- subset(df_all, is.na(df_all$age) == T)
m_age <- lm(age ~ gender + affiliate_channel + signup_app + first_device_type, train_age)
test_age$age <- predict(m_age,test_age)
df_all <- rbind(train_age,test_age)
df_all$age <- round(df_all$age, 0)

###making rest of the null values of 
df_all[is.na(df_all)] <- -1
test_pre <- df_all[df_all$id %in% df_test$id,]

####One Hot Encoding###
ohe_feats = c('gender', 'signup_method','signup_flow', 'language','affiliate_channel','affiliate_provider','first_affiliate_tracked', 'signup_app', 'first_device_type','first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language +  affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser , data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)


X = df_all_combined[df_all_combined$id %in% df_train$id,]
y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
y <-(as.integer(y)-1)
X_test = df_all_combined[df_all_combined$id %in% df_test$id,]


####xgboost Model###
xgb <- xgboost(data = data.matrix(X[,-1]), 
               label = y, 
               eta = 0.1,
               max_depth = 20, 
               nround=250,
               early_stopping_rounds = 100,
               n_estimators = 100,
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)


###evaluation logs of the xgboost
e <- data.frame(xgb_cv$evaluation_log)

###Importance of top 20 variables variables###
importance_matrix <- xgb.importance(colnames(X[,-1]),model = xgb)
xgb.plot.importance(importance_matrix = importance_matrix[1:20,])

###Prediction###
y_pred <- predict(xgb, data.matrix(X_test[,-1]))
predictions <- as.data.frame(matrix(y_pred, ncol =12))
colnames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
predictions1 <- colnames(predictions)[max.col(predictions,ties.method="first")]

###Mapping predicted country destination to the userid and making the scoring file
submission <- NULL
submission$id <- X_test$id
submission$country <- predictions1
submission <- as.data.frame(submission)

###scoreboard of countries from the scoring file(count of people grouped by countries)##
df2 <- ddply(submission, .(country) , function(submission) c(count = nrow(submission)))

write.csv(submission,"D:\\CourseWork\\BA\\Projects\\Project 2\\DATA\\submission.csv")
write.csv(df2,"D:\\CourseWork\\BA\\Projects\\Project 2\\DATA\\scoreboard.csv")

###joining scoring file and the test file for inference generation
final <- merge(test_pre, submission, by = 'id', all.x = TRUE)
write.csv(final,"D:\\CourseWork\\BA\\Projects\\Project 2\\DATA\\xgboost.csv")


