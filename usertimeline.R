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
tweets <- userTimeline("EPN", n = 3000)
tweets.df <- twListToDF(tweets)
library(tm)

myCorpus <- Corpus(VectorSource(tweets.df$text))

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))


# Remueve paralbras inecesarias
myStopwords <- c(stopwords("spanish"), "un", "no","la","el")

myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra el espacio extra
myCorpus <- tm_map(myCorpus, stripWhitespace)

myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument)

#########

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm

(freq.terms <- findFreqTerms(tdm, lowfreq=3))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=3)
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()

##### Lluvia de palabras
library(wordcloud)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
#######Analisis Cluster
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
rect.hclust(fit, k = 6) # 6 Nucleos

m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers
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