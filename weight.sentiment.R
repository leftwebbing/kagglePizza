weight.sentiment = function(sentences, allWords, allWeights, .progress='none')
#find indices of matches according to word
#inputs:
#sentences = vector of sentences
#allWords = vector of each term to find
#allWeights = vector of weights (Credit) to apply to each found word
#output: vector of scores for each sentence 

#sum probabilities: totally unscientific, but maybe useful proxy


#relies on code from Jeffry Breen http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment

{ 
    require(plyr)
    require(stringr)
    require(tm) # using tm_map 
    #scoreScaler <- function(x){ y<- sign(x-0.5)*(5*(x-0.5))^2; return(y)}# give more weight to upper/lower ends
    scoreScaler <- function(x){ y<- sign(x-0.5)*(3*(x-0.5))^4; return(y)}# emphasize upper/lower ends

    allWeights <- scoreScaler(allWeights)

    sentences <- Corpus(VectorSource(sentences)) # convert to corpus for tm_map

    # run all transforms first
    sentences <- tm_map(sentences, content_transformer(tolower)) #xform to lower case
    sentences <- tm_map(sentences , removePunctuation ) # remove Punctuation
    sentences <- tm_map(sentences , removeWords, stopwords("english")) #remove stopwords
    sentences <- tm_map(sentences , stemDocument) #remove suffixes
    sentences <- tm_map(sentences , function(x) gsub("(([a-z]){10})(([a-z]|[A-Z]|[0-9]){1,})", "\\1", x)  )#truncate strings >10 chars 
    sentences <- tm_map(sentences , stripWhitespace) #remove whitespace
    sentences <- tm_map(sentences , PlainTextDocument) #make PlainTextDocument

    sentences <- unlist(sapply(sentences, "content")) #convert back to vector 

    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, allWords, allWeights) { # TODO mod for word & weight

        #find indices of matches according to word
        matchIdx <-match(unlist(str_split(sentence, '\\s+')), allWords )

        # sum weights: totally unscientific, but maybe useful proxy
        score = sum(allWeights[matchIdx], na.rm=TRUE) 

        return(score)
    }, allWords, allWeights, .progress = .progress )

    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}
