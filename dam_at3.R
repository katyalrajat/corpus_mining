rm(list=ls())
dev.off()
#set working directory
getwd() 
library(tm) 
library(SnowballC)
docs <- VCorpus(DirSource("./docs"))

print(docs)
class(docs)
#Volatile corpus consisting of 30 documents
#examine contents
docs[1]
class(docs[1])
#Volatile corpus consisting of 1 documents
#To access the actual contents we need to use the [[]] notation
class(docs[[1]])
docs[[1]]$meta
docs[[1]]$content

#Preprocessing
docs <- tm_map(docs, removePunctuation)
#Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))
#Remove punctuation - replace punctuation marks with " "
docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))
#inspect
docs <- tm_map(docs, stripWhitespace)
#inspect output
#writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs,stemDocument)
#writeLines(as.character(docs[[30]]))
#end of preprocessing
docs <- tm_map(docs, removeWords, c('â???','â???¦','â','???¦','???','???"',"ie"))


#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#summary
dtm
#inspect segment of document term matrix
inspect(dtm[1:3,1:10])
#collapse matrix by summing over columns - this gets total counts (over all docs) for each term
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (asc)
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]
#write to disk and inspect file
write.csv(file="freq.csv",freq[ord])
#inspect least frequently occurring terms
freq[tail(ord)]
#list most frequent terms. Lower bound specified as second argument
findFreqTerms(dtm,lowfreq=80)
#correlations

#order by frequency
library(ggplot2)

wf=data.frame(term=names(freq),occurrences=freq)
p <- ggplot(subset(wf, occurrences>200), aes( reorder(term,occurrences), occurrences , fill = factor(term))) +
geom_bar(stat="identity" ,  show.legend = FALSE) + theme_bw() + theme(axis.text.x=element_text(angle=45, hjust=1, size = 15)) +
  xlab("Words")
p
#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(43)
#limit words by specifying min frequency
#wordcloud(names(freq),freq, max.words=40)
#...add color
wordcloud(names(freq),freq, max.words=80,colors=brewer.pal(6,"Dark2"))
#play with different values of max.words

BigramTokenizer <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#create DTM
dtmbi <- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer))
freqbi <- colSums(as.matrix(dtmbi))
#length should be total number of terms
length(freqbi)
#create sort order (asc)
ordbi <- order(freqbi,decreasing=TRUE)
#inspect most frequently occurring terms
freqbi[head(ordbi)]

wfb=data.frame(term=names(freqbi),occurrences=freqbi)
p <- ggplot(subset(wfb, occurrences>25), aes( reorder(term,occurrences), occurrences , fill = factor(term))) +
  geom_bar(stat="identity" , show.legend = FALSE) + theme_bw() + theme(axis.text.x=element_text(angle=45, hjust=1)) +
    theme(text = element_text(size=20)) + xlab("Word Pairs")
p

wordcloud(names(freqbi),freqbi, max.words=35,colors=brewer.pal(6,"Dark2"))

BigramTokenizer <-  function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
#create DTM
dtmtri <- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer))
freqtri <- colSums(as.matrix(dtmtri))
#length should be total number of terms
length(freqtri)
ordtri <- order(freqtri,decreasing=TRUE)
#inspect most frequently occurring terms
freqtri[head(ordbi)]

wordcloud(names(freqtri),freqtri, max.words=15,colors=brewer.pal(6,"Dark2"))

###############################################################
################################# WEIGHTED WORDS ##############
##assumes tm, ggplot and wordcloud libraries are loaded
dtm_tfidf <- DocumentTermMatrix(docs,control = list(weighting = weightTfIdf))
#note that the weighting is normalised by default (that is, the term frequencies in a
#document are normalised by the number of terms in the document)
#summary
dtm_tfidf
#collapse matrix by summing over columns - this gets total weights (over all docs) for each term
wt_tot_tfidf <- colSums(as.matrix(dtm_tfidf))
#length should be total number of terms
length(wt_tot_tfidf )
#create sort order (asc)
ord_tfidf <- order(wt_tot_tfidf,decreasing=TRUE)
#inspect most frequently occurring terms
wt_tot_tfidf[head(ord_tfidf)]
#write to disk and inspect file
write.csv(file="wt_tot_tfidf.csv",wt_tot_tfidf[ord_tfidf])
#inspect least weighted terms
wt_tot_tfidf[tail(ord_tfidf)]


#histogram
wf=data.frame(term=names(wt_tot_tfidf),weights=wt_tot_tfidf)
#library(ggplot2)
p <- ggplot(subset(wf, wt_tot_tfidf>.15), aes(reorder(term,weights), weights,fill = factor(term)))+ theme_bw() +
 geom_bar(stat="identity" , show.legend = FALSE) + theme(axis.text.x=element_text(angle=45, hjust=1)) + theme(text = element_text(size=20)) + xlab("Words")
p
#wordcloud
set.seed(42)

wordcloud(names(wt_tot_tfidf),wt_tot_tfidf,max.words=100,colors=brewer.pal(6,"Dark2"))
#play with different values of max.words
#try specifying min.freq instead of max.words

###################################################################
################################ Dendogram ##############################

dtm <- DocumentTermMatrix(docs)
## start clustering specific code
#convert dtm to matrix (what format is the dtm stored in?)
m<-as.matrix(dtm)
#write as csv file
write.csv(m,file="dtmAsMatrix.csv")
#shorten rownames for display purposes
#rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
#                     substring(rownames(m),
#                               nchar(rownames(m))-12,nchar(rownames(m))-4))
#compute distance between document vectors
d <- dist(m)
#run hierarchical clustering using Ward's method (explore other options later)
groups <- hclust(d,method="ward.D")
#plot, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
rect.hclust(groups,2)
hclusters <- cutree(groups,2)
write.csv(hclusters,"hclusters.csv")

dev.off()
#try another distance measure
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs

#run hierarchical clustering using cosine distance
groups <- hclust(cd,method="ward.D")
#plot, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
#cut into 2 subtrees.
rect.hclust(groups,5)
hclusters_cosine <- cutree(groups,3)
write.csv(hclusters_cosine,"hclusters_cosine.csv")

############################ LSA ##############################
###############################################################
tdm <- TermDocumentMatrix(docs)
tdm.matrix <- as.matrix(tdm)
#check class
dim(tdm.matrix)

#weight terms and docs
tdm.matrix.lsa <- lw_tf(tdm.matrix) * gw_idf(tdm.matrix)
dim(tdm.matrix.lsa)

#compute the Latent semantic space
lsaSpace <- lsa(tdm.matrix.lsa, dimcalc_share()) # create LSA space
#examine output
names(lsaSpace)

LSAMat <- as.textmatrix(lsaSpace)

#Calculate similarity of documents in LSA space
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#Similarity matrix
cs.lsa <- as.matrix(cosineSim(t(LSAMat)))
write.csv(cs.lsa,"cs_lsa.csv")

library(corrplot)
corrplot(cs.lsa)
corrplot(cs.lsa, method = "square")


###############################################################
######################### Topic Modelling #####################
  
library(topicmodels)
install.packages("tidytext")
library(tidytext)
library(tidyverse)

#create model fit
ap_lda <- LDA(dtm, k = 3, control = list())
ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) + theme_bw() +
  facet_wrap(~ topic, scales = "free") + theme(text = element_text(size=20)) +
  coord_flip() + ylab("Groups")

ldaOut.topics <-as.matrix(topics(ap_lda))
write.csv(ldaOut.topics,file=paste("DocTopics.csv"))

df<- read.csv("DocTopics.csv")
df <- rename( df , doc  = X , topic = V1)

ggplot(df, aes(x=doc , y=factor(topic))) + geom_point(aes(colour = factor(topic) , shape = factor(topic), size =5), show.legend = FALSE) +
  theme_bw()  +  theme(text = element_text(size=20)) + coord_flip()


############################################################
########################## Sentimental Analysis ###################

sentiments

t_corpus <- docs %>% tidy()
t_corpus

tidy_df <- t_corpus %>%
  unnest_tokens(word, text)

tidy_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_df %>% 
  inner_join(tidy_joy) %>%
  count(word, sort = TRUE)

tidy_df_sent <- tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(id, sentiment) %>%
  spread(sentiment, n, fill = 0)

#mutate(sentiment = positve - negative)

tidy_df_sent <- tidy_df_sent %>%
  mutate(sentiment = positive - negative)

tidy_df_sent

ggplot( tidy_df_sent, aes(id, sentiment , fill = sentiment)) +
  geom_col(show.legend = FALSE) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15)) + xlab("Docs")


df1 <- tidy_df_sent %>% slice(1:10)
df2 <- tidy_df_sent %>% slice(11:17)
df3 <- tidy_df_sent %>% slice(23:34)
df4 <- tidy_df_sent %>% slice(35:41)
df5 <- tidy_df_sent %>% slice(18:22)

p1 <-   ggplot(df1, aes(id, sentiment , fill = sentiment)) +
  geom_col(show.legend = FALSE) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15)) + xlab("Group 1")

p2 <-  ggplot(df2, aes(id, sentiment , fill = sentiment)) +
     geom_col(show.legend = FALSE) + theme_bw() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15)) + xlab("Group 2") + ylab("")
   
p3 <-  ggplot(df3, aes(id, sentiment , fill = sentiment)) +
    geom_col(show.legend = FALSE) + theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15)) + xlab("Group 3") + ylab("")

p4 <-  ggplot(df4, aes(id, sentiment , fill = sentiment)) +
    geom_col(show.legend = FALSE) + theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15)) + xlab("Group 4") + ylab("")

p5 <-  ggplot(df5, aes(id, sentiment , fill = sentiment)) +
  geom_col(show.legend = FALSE) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15)) + xlab("Group 5") + ylab("")


library("ggpubr")

ggarrange(p1,p2,p3,p4, p5 , ncol = 3, nrow = 2)

  
   
   