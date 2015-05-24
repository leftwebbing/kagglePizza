wordStrengthMaker <- function(bigDF,minPosts) {

#inputs: 
#bigDF = data frame, col1 = T/F, other cols = word counts from TDM
#minPosts= int, minimum number of posts containing word 
#output:data frame,
#col1 = count(posts got pizza)/count(posts containing word) = p(pizza|word)
#col2 = word name (text)
#col3 = count posts have word col3
#col4 = pr(pizza|word)*count posts / total posts


wordStrength <- matrix(0,nrow = dim(bigDF)[2], ncol = 4)# initialize empty matrix
colnames(wordStrength) <- c("pr(pizza|word)", "word", "count(word)","weight") # set column names
wordStrength <- data.frame(wordStrength)  # convert to data.frame
x<- 0 #initialize counter
numPosts=dim(trainDF2)[1]
for(x in 2:dim(bigDF)[2]){ # run over columns ###### FUNCTION for word strength
  subSetWord<-bigDF[bigDF[,x]>0 ,c(1,x)] #pick posts containing word 
  subSetPizzaYes<-subSetWord[subSetWord[,1]==1,] #pick posts got pizza
  wordStrength[x,1] <-dim(subSetPizzaYes)[1]/dim(subSetWord)[1] # count(posts got pizza)/count(posts containing word) col1
  wordStrength[x,2] <- names(subSetWord)[2] # copy word name col2
  wordStrength[x,3] <- dim(subSetWord)[1] # count posts have word col3
  wordStrength[x,4] <- wordStrength[x,1]*wordStrength[x,3]/numPosts # pr(pizza|word)*count posts / total posts
}

wordStrengthSub <-wordStrength[wordStrength[,3]>=minPosts,] # select only where word appears > threshold posts
return(wordStrengthSub )

}
