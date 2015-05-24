####### kaggle pizza altruism
### version 1.1
### 2015-05-22 Friday

setwd("C:/Users/YabbaMan/Documents/fun/kagglePizza")

library(rjson)
library(stringi)
library(lubridate)
library(randomForest)


######### load data
trainDF2 <- as.data.frame(do.call("rbind", lapply(fromJSON(file='train.json'), 
    function(z)unlist(lapply(z, function(x) ifelse(is.null(x) || length(x) == 0, NA, ifelse(length(x)>1, paste(x, collapse='; '),x)))))), stringsAsFactors = FALSE )
testDF2 <- as.data.frame(do.call("rbind", lapply(fromJSON(file='test.json'), 
    function(z)unlist(lapply(z, function(x) ifelse(is.null(x) || length(x) == 0, NA, ifelse(length(x)>1, paste(x, collapse='; '),x)))))), stringsAsFactors = FALSE )

for ( x in c(2,3,10:22,24:28,31,32)) trainDF2[,x] <- as.numeric(trainDF2[,x] ) # convert numerics
trainDF2[,23] <- as.logical(trainDF2[,23])# convert logicals
for ( x in c(5:11,13,14,16,17)) testDF2[,x] <- as.numeric(testDF2[,x] ) # convert numerics

str(trainDF2 )   # examine
str(testDF2 )    # examine
names(trainDF2 ) # examine
names(testDF2 )  # examine

trainDF2 <- trainDF2[ order(trainDF2$unix_timestamp_of_request_utc), ] # sort by time
testDF2 <- testDF2[ order(testDF2$unix_timestamp_of_request_utc), ] # sort by time

# maybe time influences outcome? 
# maybe donors become weary of me-too requests, 
# or more encouraged by growing frenzy of requests
# make engineered variable showing difference in time between last request 

trainDF2$unix_timestamp_difference <- NA #initialize new column for difference
for(x in 1:dim(trainDF2 )[1]-1) trainDF2$unix_timestamp_difference[x] <- trainDF2$unix_timestamp_of_request_utc[x+1] - trainDF2$unix_timestamp_of_request_utc[x] # calculate difference since last post
trainDF2$unix_timestamp_difference[1] <- mean(trainDF2$unix_timestamp_difference, na.rm=T  ) # set last value to avg, assume similar time

testDF2$unix_timestamp_difference <- NA #initialize new column for difference
for(x in 1:dim(testDF2 )[1]-1) testDF2$unix_timestamp_difference[x] <- testDF2$unix_timestamp_of_request_utc[x+1] - testDF2$unix_timestamp_of_request_utc[x] # calculate difference since last post
testDF2$unix_timestamp_difference[1] <- mean(testDF2$unix_timestamp_difference, na.rm=T  ) # set last value to avg, assume similar time

source("textTDMMaker.R")  # support function to generate TDM from text entries
trainTDM <- textTDMMaker(trainDF2$request_text, 1) # make TDM for train text
testTDM  <- textTDMMaker( testDF2$request_text_edit_aware,1) # make TDM for test  text

##### make TDM for train, test
source("wordStrengthMaker.R")  # load support fucntion 
bigDF <- cbind(as.matrix(trainDF2$requester_received_pizza), as.matrix(trainTDM ))# bind TDM to original data frame
bigDF <- data.frame(bigDF )  # make sure still data frame
names(bigDF )[1] <-"requester_received_pizza" # rename first column 

wordStrengthTrain <- wordStrengthMaker(bigDF ,3) # compute word strengths of train, for post body text
# can't make word strength for test, since pizza unknown

source("weight_sentiment.R" )
# compute sentiment score based on pr(pizza|word) 
predictedSentimentTrain <- weight.sentiment(trainDF2$request_text, wordStrengthTrain$word, wordStrengthTrain$pr.pizza.word ) 
# assume words in TEST play same role in TEST 
predictedSentimentTest  <- weight.sentiment( testDF2$request_text_edit_aware, wordStrengthTrain$word, wordStrengthTrain$pr.pizza.word )


trainDF2<- cbind(trainDF2, score = predictedSentimentTrain$score) # add scores as column to main DF
testDF2  <- cbind(testDF2, score = predictedSentimentTest$score) # add scores as column to main DF

# add columns for text length
trainDF2$text_length <- nchar(trainDF2$request_text) 
testDF2$text_length <- nchar(testDF2$request_text_edit_aware) 

z.cols <-  cut(as.numeric(trainDF2$requester_received_pizza), 2, labels = c("black", "green")) # make color map for T/F predicted
# plot character length of post VS score
# shows good separation between T/F 
plot(x=trainDF2$score, y=trainDF2$text_length, col = as.character(z.cols))

str(trainDF2)
# can't plot test data vs T/F since unknown, show all points in same color
plot(x=testDF2$score, y=testDF2$text_length)# plot test data

# create random forest model for relevant variables 
modelRF1Train <- randomForest(as.factor(requester_received_pizza) ~ score +text_length +unix_timestamp_difference+ requester_account_age_in_days_at_request +requester_number_of_comments_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request, data=trainDF2, mtry= 4, ntree=500, na.action=na.omit)


# =======sandbox=======
trainDF2 <- subset(trainDF2 , select = -c(35))
str(wordStrengthTrain)
maxTerm <- max(wordStrengthTrain$count.word) # find most common term

wordStrengthTrain[wordStrengthTrain$count.word >= maxTerm-1500,]

wordStrengthTrain$commonPercent <- wordStrengthTrain$count.word / maxTerm

# compute sentiment score based on pr(pizza|word) 
predictedSentimentTrain <- weight.sentiment(trainDF2$request_text, wordStrengthTrain$word, wordStrengthTrain$commonPercent) 
str(predictedSentimentTrain )

trainDF2<- cbind(trainDF2, scoreCommon = predictedSentimentTrain$score) # add scores as column to main DF
str(trainDF2)
plot(x=trainDF2$scoreCommon, y=trainDF2$text_length, col = as.character(z.cols))

# assume words in TRAIN play same role in TEST 
predictedSentimentTest  <- weight.sentiment( testDF2$request_text_edit_aware, wordStrengthTrain$word, wordStrengthTrain$pr.pizza.word )


linearModel <- lda(as.factor(requester_received_pizza)~ score +text_length +unix_timestamp_difference, data =trainDF2)
plot(linearModel )

ksvmTrain <- ksvm(as.factor(requester_received_pizza) ~ score +text_length +unix_timestamp_difference, data =trainDF2,type="C-svc" )#### TODO add SVM model
xData=cbind(trainDF2$score , trainDF2$text_length, trainDF2$unix_timestamp_difference)
plot(ksvmTrain , xData)
str(ksvmTrain )
str(xData)

modelRF1Train <- randomForest(as.factor(requester_received_pizza) ~ score +text_length +unix_timestamp_difference+ requester_account_age_in_days_at_request +requester_number_of_comments_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request, data=trainDF2, mtry= 4, ntree=500, na.action=na.omit)

