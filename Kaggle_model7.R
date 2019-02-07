##Model 7 - boosting with re-processed feature selection
##import data
data=read.csv("~/Desktop/analysisData_reprocessed.csv")

##Cleaning data
####"host-since"
data$host_since <- as.numeric(data$host_since)

####remove "street"
data$street <- NULL

####clean "neighbourhood"
data <- data[!is.na(data$neighbourhood),]

####remove "city"
data$city <- NULL

####clean "zipcode"
data <- data[!is.na(data$zipcode),]

####clean "beds"
data$beds[is.na(data$beds)] <- 0

####clean "cleaning_fee"
data$cleaning_fee[is.na(data$cleaning_fee)] <- 0

####process host_verifications
# wordcloud of host_verifications
data$host_verifications=as.character(data$host_verifications)
words2 <- lapply(data$host_verifications,strsplit,",")
wordsNum2 <- table(unlist(words2))
wordsNum2 <- sort(wordsNum2)
wordsData2 <- data.frame(words =names(wordsNum2), freq = wordsNum2)
wordsData2[] <- lapply(wordsData2, gsub, pattern="\\[", replacement='')
wordsData2[] <- lapply(wordsData2, gsub, pattern='\\]', replacement='')
wordsData2[] <- lapply(wordsData2, gsub, pattern="'", replacement='')
#wordsData2[] <- lapply(wordsData2, gsub, pattern='` ', replacement='')
#wordsData2[] <- lapply(wordsData2, gsub, pattern=' `', replacement='')

wordsData2$freq.Freq <- as.numeric(wordsData2$freq.Freq)
wordsData2 <- aggregate(freq.Freq~words+freq.Var1,data=wordsData2,FUN=sum)

wordsData2

library(plyr)

for (i in 1:22) {
  data$i=as.factor(grepl(as.factor(wordsData2$words[i]),data$host_verifications,ignore.case = T))
  a=as.character(wordsData2$words[i])
  data=rename(data,c(i=a))
}

data$photographer <- NULL
data$zhima_selfie <- NULL
summary(data)

####process "amenities"
# wordcloud of amenities
data$amenities=as.character(data$amenities)
words3 <- lapply(data$amenities,strsplit,",")
wordsNum3 <- table(unlist(words3))
wordsNum3 <- sort(wordsNum3)
wordsData3 <- data.frame(words =names(wordsNum3), freq = wordsNum3)
wordsData3[] <- lapply(wordsData3, gsub, pattern="\\{", replacement='')
wordsData3[] <- lapply(wordsData3, gsub, pattern='\\}', replacement='')
wordsData3[] <- lapply(wordsData3, gsub, pattern='"', replacement='')

wordsData3$freq.Freq <- as.numeric(wordsData3$freq.Freq)
wordsData3 <- aggregate(freq.Freq~words+freq.Var1,data=wordsData3,FUN=sum)
#wordsData3 = wordsData3[-1,]
wordsData3

for (i in 2:120) {
  data$i=as.factor(grepl(as.character(wordsData3$words[i]),data$amenities,ignore.case = T))
  b=as.character(wordsData3$words[i])
  data=rename(data,c(i=b))
}

summary(data)
str(data)

data$host_verifications <- NULL
data$amenities <- NULL

##clean variables for scoringData
scoringData=read.csv("~/Desktop/Kaggle/scoringData.csv")
scoringData$host_since <- as.numeric(scoringData$host_since)
scoringData$calendar_updated <- as.numeric(scoringData$calendar_updated)
scoringData$first_review <- as.numeric(scoringData$first_review)
scoringData$last_review <- as.numeric(scoringData$last_review)

# wordcloud of host_verifications
scoringData$host_verifications=as.character(scoringData$host_verifications)
words4 <- lapply(scoringData$host_verifications,strsplit,",")
wordsNum4 <- table(unlist(words4))
wordsNum4 <- sort(wordsNum4)
wordsData4 <- data.frame(words =names(wordsNum4), freq = wordsNum4)
wordsData4[] <- lapply(wordsData4, gsub, pattern="\\[", replacement='')
wordsData4[] <- lapply(wordsData4, gsub, pattern='\\]', replacement='')
wordsData4[] <- lapply(wordsData4, gsub, pattern="'", replacement='')

wordsData4$freq.Freq <- as.numeric(wordsData4$freq.Freq)
wordsData4 <- aggregate(freq.Freq~words+freq.Var1,data=wordsData4,FUN=sum)
wordsData4

for (i in 1:21) {
  scoringData$i=as.factor(grepl(as.character(wordsData4$words[i]),scoringData$host_verifications,ignore.case = T))
  d=as.character(wordsData4$words[i])
  scoringData=rename(scoringData,c(i=d))
}
#summary(scoringData)

# wordcloud of amenities
scoringData$amenities=as.character(scoringData$amenities)
words5 <- lapply(scoringData$amenities,strsplit,",")
wordsNum5 <- table(unlist(words5))
wordsNum5 <- sort(wordsNum5)
wordsData5 <- data.frame(words =names(wordsNum5), freq = wordsNum5)
wordsData5[] <- lapply(wordsData5, gsub, pattern="\\{", replacement='')
wordsData5[] <- lapply(wordsData5, gsub, pattern='\\}', replacement='')
wordsData5[] <- lapply(wordsData5, gsub, pattern='"', replacement='')

wordsData5$freq.Freq <- as.numeric(wordsData5$freq.Freq)
wordsData5 <- aggregate(freq.Freq~words+freq.Var1,data=wordsData5,FUN=sum)
#wordsData5 = wordsData5[-1,]
wordsData5

for (i in 2:120) {
  scoringData$i=as.factor(grepl(as.character(wordsData5$words[i]),scoringData$amenities,ignore.case = T))
  e=as.character(wordsData5$words[i])
  scoringData=rename(scoringData,c(i=e))
}
summary(scoringData)
str(scoringData)

scoringData <- scoringData[,colSums(is.na(scoringData))<nrow(scoringData)]
#summary(scoringData)

####remove "square_feet"
data$square_feet <- NULL

####clean "security_deposit"
data$security_deposit[is.na(data$security_deposit)] <- 0

####clean "calendar_updated"
class(data$calendar_updated)
data$calendar_updated <- as.numeric(data$calendar_updated)

####remove "has_availability"
data$has_availability <- NULL

####clean "first_review"
class(data$first_review)
data$first_review <- as.numeric(data$first_review)

####clean "last_review"
class(data$last_review)
data$last_review <- as.numeric(data$last_review)

####examine text variables
#class(data$cancellation_policy)
#class(data$property_type)
#class(data$room_type)
#class(data$cleaning_fee)

##feature selection - lasso
library(glmnet)
data <- data[, !duplicated(colnames(data))]
data <- data[,colSums(is.na(data))<nrow(data)]

summary(data)
x = model.matrix(price~.-1,data=data)
y = data$price

lassoModel = glmnet(x,y, alpha=1)
lassoModel

cv.lasso = cv.glmnet(x,y,alpha=1) # 10-fold cross-validation
coef(cv.lasso)

##Model 7 - Boosting
library(gbm)
model7_boost = gbm(price~., data=data, distribution="gaussian",n.trees = 1000,interaction.depth = 5,shrinkage = 0.01)

pred=predict(model7_boost,n.trees = 1000)
rmse7=sqrt(mean((pred-data$price)^2))
rmse7

#predict scoringData
pred_model7 = predict(model7_boost, newdata=scoringData, n.trees = 1000)

##### Construct submision from predictions
submissionFile = data.frame(id = scoringData$id, price = pred_model6)
write.csv(submissionFile, "~/Desktop/Kaggle/Kaggle_model6_submission.csv",row.names = F)
