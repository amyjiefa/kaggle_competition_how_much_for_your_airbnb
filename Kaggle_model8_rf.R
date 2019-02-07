##Model 8 - random forest
##import data
data=read.csv("~/Desktop/analysisData_reprocessed.csv")

##Cleaning data
####"host-since"
data$host_since <- as.numeric(data$host_since)

####remove "street"
data$street <- NULL

####clean "neighbourhood"
#data <- data[!is.na(data$neighbourhood),]

####remove "city"
data$city <- NULL

####clean "zipcode"
data <- data[!is.na(data$zipcode),]

####clean "beds"
data$beds[is.na(data$beds)] <- 0

####clean "cleaning_fee"
data$cleaning_fee[is.na(data$cleaning_fee)] <- 0

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

####clean "host_verification" and "amenities"
data$host_verification <- NULL
data$amenities <- NULL

library(randomForest)
model8_rf <- randomForest(price~neighbourhood_group_cleansed+longitude+
                       room_type+accommodates+bathrooms+bedrooms+
                       cleaning_fee+guests_included+minimum_nights+
                       availability_30+availability_60+availability_90+availability_365+
                       review_scores_rating+review_scores_cleanliness+
                       review_scores_location+review_scores_value+is_business_travel_ready+reviews_per_month+cancellation_policy
                     ,data=data,ntree=1000)

##test model
scoringData=read.csv("~/Desktop/Kaggle/scoringData.csv")
scoringData$host_since <- as.numeric(scoringData$host_since)
scoringData$calendar_updated <- as.numeric(scoringData$calendar_updated)
scoringData$first_review <- as.numeric(scoringData$first_review)
scoringData$last_review <- as.numeric(scoringData$last_review)
scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <- 0
scoringData$reviews_per_month[is.na(scoringData$reviews_per_month)] <- 0

#predict scoringData
library(ROCR)
predForest = predict(model8_rf,newdata=scoringData)

##### Construct submision from predictions
submissionFile = data.frame(id = scoringData$id, price = predForest)
write.csv(submissionFile, "~/Desktop/Kaggle/Kaggle_model8_submission.csv",row.names = F)
