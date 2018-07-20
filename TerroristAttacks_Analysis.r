install.packages("leaflet")
install.packages("treemap")
install.packages("tm")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("kableExtra")
install.packages("formattable")



library(ggplot2)
library(leaflet)
library(treemap)
library(corrplot)
library(tm)
library(tidytext)
library(tidyr)
library(wordcloud)
library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)
options(knitr.table.format = "html")


df <- read.csv("C:/Users/prana/OneDrive/Documents/globalterrorismdb_0617dist.csv", stringsAsFactors=FALSE)

head(df)

df %>% filter(nkill > 0) -> dfk
treemap(dfk, 
        index = c("iyear"),
        vSize="nkill",
        palette="Blues",
        title="Killing in Global terrorism",
        fontsize.title = 14)

dev.off()

treemap(dfk, 
        index = c("country_txt","iyear"),
        type="value",
        vSize="nkill",
        vColor = "nwound",
        palette="Reds",
        title="Killings in Global terrorism  (Countries/Years) - size is proportional with the number of killings",
        title.legend = "Number of wounded",
        fontsize.title = 14)


treemap(dfk, 
        index = c("iyear","country_txt"),
        type="value",
        vSize="nkill",
        vColor = "nwound",
        palette="Reds",
        title="Killings in Global terrorism  (Years/Countries) - size is proportional with the number of killings",
        title.legend = "Number of wounded",
        fontsize.title = 14)




ds <- subset(df,weapdetail != "" & iyear != "")
nrow(ds)
nrow(df)
head(ds$weapdetail)
#Number of people killed and wounded using different weapons each year where,
#size of box is proportional to number of people killed and darkness of color is 
#proportional to number of people wounded
treemap(ds,
        index = c("weapdetail","iyear"),
        type="value",
        vSize="nkill",
        vColor = "nwound",
        palette="RdYlBu",
        title="Killings in Global terrorism per weapon",
        title.legend = "Number of wounded",
        fontsize.title = 14)




leaflet(data = dfk) %>%
  addTiles() %>% 
  addMarkers(lat=dfk$latitude, lng=dfk$longitude, clusterOptions = markerClusterOptions(),
             popup=paste("<strong>Date: </strong>", dfk$iday,"/",dfk$imonth,"/", dfk$iyear,
                         "<br><br><strong>Place: </strong>", dfk$city,"-",dfk$country_txt,
                         "<br><strong>Killed: </strong>", dfk$nkill,
                         "<br><strong>Wounded: </strong>", dfk$nwound
             ))

dev.off()

dfk %>% group_by(iyear) %>% summarise(n = sum(nkill)) %>% ungroup() -> dfy
colnames(dfy)<-c("Year","Killed")
ggplot(data = dfy, aes(x = Year, y = Killed)) +       
  geom_line() + geom_point() + theme_bw()


dfk %>% group_by(iyear,region_txt) %>% summarise(nkills = sum(nkill)) %>% ungroup() -> dfyr
colnames(dfyr)<-c("Year","Region","Killed")
ggplot(data = dfyr, aes(x = Year, y = Killed, colour = Region)) +       
  geom_line() + geom_point() + theme_bw()



df %>% filter(nkillus > 0) -> dfk_us

treemap(dfk_us,
        
        index=c("country_txt", "iyear"),  
        
        type = "value",
        
        vSize = "nkillus",  
        
        vColor="nwoundus",
        
        palette = "RdBu",  
        
        title="Killings in Global terrorism - US Citizens (Countries/Years) - size is proportional with the number of killings", 
        
        title.legend = "Number of wounded US citizens",
        
        fontsize.title = 12
        
)

dfw_us %>% group_by(iyear,region_txt) %>% summarise(nwounds = sum(nwoundus)) %>% ungroup() -> dfyr



colnames(dfyr)<-c("Year","Region","Wounded")



ggplot(data = dfyr, aes(x = Year, y = Wounded, colour = Region)) +       
  
  geom_line() + geom_point() + theme_bw()



#Type of attack
df %>% group_by(iyear,attacktype1_txt) %>% summarise(n = length(iyear)) %>% ungroup() -> dfya



colnames(dfya)<-c("Year","Type of attack","Number of events")



ggplot(data = dfya, aes(x = Year, y = `Number of events`, colour = `Type of attack`)) + 
  
  geom_line() + geom_point() + theme_bw()





#Correlation
terrorCor <- df[,c("iyear","imonth","iday","country", "nkill", "ransompaid","ransompaidus",
                   
                   "nhostkidus","nhours","ndays")]



terrorCor <- na.omit(terrorCor)



correlations <- cor(terrorCor)

p <- corrplot(correlations, method="circle")


#Text Analysis

df %>% filter(!is.na(ransomnote)) -> dfn0

dfn0 %>% filter(ransomnote != "") -> dfn

text <- dfn$ransomnote

myCorpus <- Corpus(VectorSource(text))

myCorpus <- tm_map(myCorpus,content_transformer(tolower))


myCorpus <- tm_map(myCorpus,removePunctuation)

myCorpus <- tm_map(myCorpus,removeNumbers)

myCorpus <- tm_map(myCorpus,removeWords,c(stopwords("english"),stopwords("SMART")))

myDtm = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

freqTerms <- findFreqTerms(myDtm, lowfreq=1)

m <- as.matrix(myDtm)

v <- sort(rowSums(m), decreasing=TRUE)

myNames <- names(v)

d <- data.frame(word=myNames, freq=v)


wctop <-wordcloud(d$word, d$freq, min.freq=5, colors=brewer.pal(9,"Set1"))


mydata.df <- as.data.frame(inspect(removeSparseTerms(myDtm, sparse=0.99)))





#Extra text analysis
install.packages('tm')

library(tm)

review_source <- VectorSource(dfn$ransomnote)
corpus <- Corpus(review_source)


corpus<- tm_map(corpus,content_transformer(tolower))
corpus<- tm_map(corpus,removePunctuation)
corpus<- tm_map(corpus,stripWhitespace)

#corpus <- tm_map(corpus,removeWords,c(stopwords("english"),stopwords("SMART")))



dtm2 <- DocumentTermMatrix(corpus)
dtm1 <- as.matrix(dtm2)


frequency <- colSums(dtm1)

frequency

frequency <- sort(frequency,decreasing = TRUE)

nfrequency <- head(frequency, n=20)


words <- names(nfrequency)

w1 <- wordcloud(words[1:100],frequency[1:100])
 

dandogram <- scale(nfrequency)

d <- dist(dandogram, method = "euclidean")

fit <- hclust(d, method="ward.D")

plot(fit, xaxt = 'n', yaxt='n', xlab = "Word clustering using ward.D method", ylab = "",
     
     main="Cluster Dendogram for words used in summary description")


groups <- cutree(fit, k=5)

rect.hclust(fit, k=5, border="blue")