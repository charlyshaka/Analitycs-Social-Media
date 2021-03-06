###Entra a Twiterr

library(twitteR)
library(ROAuth)
api_key <- "TEqtGTHhGpl0gaEj0YlacXSMP"
api_secret <- "C2Uyt9dN8REdTlTkRBY7gFtMhLgVeNtbpEjefKpiBKO8Ux7ns0"
access_token <- "831261005850120192-LQ39DWYq0KZOXqwldxMWHwB1AYUgdUv"
access_token_secret <- "z5PBlltSCwh5wuiXfgE0BHI5aR19QUmvPg0d5TzNeblBF"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##### Limpieza de Texto

####### 
tweets <- userTimeline("lopezobrador_", n = 4000)
tweets.df <- twListToDF(tweets)
library(tm)

### Convierte un vector de Caracteres
myCorpus <- Corpus(VectorSource(tweets.df$text))
### convierte a minisculas
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
###
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords

# Remueve paralbras inecesarias
myStopwords <- c(stopwords("spanish"), "un", "no","la","el")

myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra el espacio extra
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument)

#########

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm

(freq.terms <- findFreqTerms(tdm, lowfreq=3))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=100)
df <- data.frame(term = names(term.freq), freq = term.freq)



#########Grafico de Frecuencias###############
library(ggplot2)   
   
p<- ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Palabra") + ylab("Frecuencia") +coord_flip()
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p
############################################


########### Lluvia de palabras
library(wordcloud)
set.seed(1234)


m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#########################################################









############################################################################
# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
rect.hclust(fit, k = 6,border="red") # cut tree into 6 clusters 

m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 10 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

##############################
library(fpc)
library(cluster)
d <- dist(t(tdm2), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(tdm2), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
###############################################



###########################################



library(fpc)
# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, metric="manhattan")
k <- pamResult$nc # number of clusters identified

pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
  cat("cluster", i, ": ",
      colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
}
# plot clustering result
layout(matrix(c(1, 2), 1, 2)) # set to two graphs per page
plot(pamResult, col.p = pamResult$clustering)

for (i in 1:k){
  cat(paste("cluster",i,": ", sep =""))
  s <- sort(kmeansResult$centers[i, ],decreasing = T)
  cat(names(s)[1:5], "\n")
}
  
(n.tweet <- length(tweets))






################################TopicModels


dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(dtm, k = 5)
term <- terms(lda, 5) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
library("lda")

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
library(data.table) #fore IDate
topic <- topics(lda, 1)
topics <- data.frame(date=as.IDate(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")
#############################################################



######### Analisis de Sentimientos
require(devtools)
install_github("sentiment140", "okugami79")
######### sentiment analysis
library(sentiment)
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)
##
t
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")
############################
