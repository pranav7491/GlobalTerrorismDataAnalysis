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
        vColor = "iyear",
        palette=c("#ff0000","#ffffff", "#ffffff", "#ffffff", "#ffffff", "#ffffff", "#ffffff", "#ffffff", "#ffffff", "#ffffff", "#ffffff", "#ffffff","#ffffff", "#ffffff", "#ffffff", "#ffffff", "#ffffff"),
        
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




# Test for Variance 
#Explosive
dfexplosive<- subset(df,weapdetail == "Explosive" & iyear != "" & nkill != "", select = c(weapdetail,iyear,nkill))
head(dfexplosive)
nrow(dfexplosive)
sum(dfexplosive$nkill)
#Automatic firearm
dfAutomaticfirearm<- subset(df,weapdetail == "Automatic firearm" & iyear != "" & nkill != "", select = c(weapdetail,iyear,nkill))
head(dfAutomaticfirearm)
nrow(dfAutomaticfirearm)
sum(dfAutomaticfirearm$nkill)
#Firearm
dfFirearm<- subset(df,weapdetail == "Firearm" & iyear != "" & nkill != "", select = c(weapdetail,iyear,nkill))
head(dfFirearm)
nrow(dfFirearm)
sum(dfFirearm$nkill)

#normalized
dfAutomaticfirearm[sample(nrow(dfAutomaticfirearm),3000),] -> dfAutomaticfirearmSample
nrow(dfAutomaticfirearmSample)
sum(dfAutomaticfirearmSample$nkill)

head(dfAutomaticfirearmSample)

dfexplosive[sample(nrow(dfexplosive),3000),] -> dfexplosiveSample
nrow(dfexplosiveSample)
sum(dfexplosiveSample$nkill)

head(dfexplosiveSample)



dfFirearm[sample(nrow(dfFirearm),3000),] -> dfFirearmSample
nrow(dfFirearmSample)
sum(dfFirearmSample)

head(dfFirearmSample)




#combining all 3

head(dfFirearmSample$weapdetail)

a <- data.frame(b=c(dfFirearmSample$weapdetail,dfexplosiveSample$weapdetail,dfAutomaticfirearmSample$weapdetail))

head(a)
tail(a)

x <- data.frame(y=c(dfFirearmSample$nkill,dfexplosiveSample$nkill,dfAutomaticfirearmSample$nkill))


head(x)
tail(x)

finalFile <- data.frame(a,x)

head(finalFile)

tail(finalFile)


AutomaticF_F <- subset(finalFile,b != "Explosive")


head(AutomaticF_F)
tail(AutomaticF_F)

var.test(AutomaticF_F$y ~ AutomaticF_F$b, 
         alternative = "two.sided")

# F test to compare two variances
# 
# data:  AutomaticF_F$y by AutomaticF_F$b
# F = 4.1479, num df = 2999, denom df = 2999, p-value < 2.2e-16
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   3.861333 4.455766
# sample estimates:
#   ratio of variances 
# 4.147915 



#for 2999 degrees of freedom F value of 4.147915 is very large so population means 
#of number of people died all over the world through Firearm and Automatic firearm 
#cannot be equal. This means various things such as locality, impacts of weapon used
#also come into picture. Further analysis such as country wise Analysis of variance 
#could also be done to see whether the population density, lack of governance affects
#number of people killed or not(crime rate in a country could be used to see governance).


Explosive_F <- subset(finalFile,b != "Automatic firearm")


head(Explosive_F)
tail(Explosive_F)

var.test(Explosive_F$y ~ Explosive_F$b, 
         alternative = "two.sided")




# data:  Explosive_F$y by Explosive_F$b
# F = 1.4065, num df = 2999, denom df = 2999, p-value < 2.2e-16
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   1.30930 1.51086
# sample estimates:
#   ratio of variances 
# 1.406474 



#Since the F ratio value is 1.4064, which falls well within the F table value 
#for 2999 degrees of freedom. We can conclude that external parameters wont play 
#any role in killings through Explosives and Firearm. 




#Now checking variance for number of people wounded and killed for a type of attack in USA.
# Test for Variance 
#Explosive
dfexplosive<- subset(df,weapdetail == "Explosive" & iyear != "" & nkillus != "" & nwoundus != "", select = c(weapdetail,iyear,nkillus,nwoundus))
head(dfexplosive)
nrow(dfexplosive)
sum(dfexplosive$nkillus)
sum(dfexplosive$nwoundus)

# > nrow(dfexplosive)
# [1] 624



list1 <- 1:624
list2 <- rep("Enkillus",length(list1))
list3 <- 1:624
list4 <- rep("Enwoundus",length(list3))

head(list4)


head(E_nkillusnwoundus)
tail(E_nkillusnwoundus)
nrow(E_nkillusnwoundus)

E_nkill_wound_us <- data.frame(n=c(dfexplosive$nkillus,dfexplosive$nwoundus))

head(E_nkill_wound_us,50)
tail(E_nkill_wound_us,50)
nrow(E_nkill_wound_us)

Final_Data <- data.frame(E_nkillusnwoundus,E_nkill_wound_us)

head(Final_Data)
ss <- na.omit(Final_Data)
nrow(ss)


colnames(Final_Data)<- c("E_nkillusnwoundus","E_nkill_wound_us")


var.test(Final_Data$E_nkill_wound_us ~ Final_Data$E_nkillusnwoundus, 
         alternative = "two.sided")

# 
# F test to compare two variances
# 
# data:  Final_Data$E_nkill_wound_us by Final_Data$E_nkillusnwoundus
# F = 0.19678, num df = 623, denom df = 623, p-value < 2.2e-16
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.1681588 0.2302795
# sample estimates:
#   ratio of variances 
# 0.196783


# Since the F test value is very less for 623 degrees of freedom,
# it could be estimated that, population means of number of people kiiled and 
# wounded with Explosives attack in USA are equal. It indicates that most of the 
# attacks with explosives are done in densely populated areas and not in 
#sparsely populated areas.


#Automatic Firearm
dfAuto<- subset(df,weapdetail == "Automatic firearm" & iyear != "" & nkillus != "" & nwoundus != "", select = c(weapdetail,iyear,nkillus,nwoundus))
head(dfAuto)
nrow(dfAuto)
sum(dfAuto$nkillus)
sum(dfAuto$nwoundus)


list11 <- 1:189
list12 <- rep("Autonkillus",length(list11))

list13 <- 1:189
list14 <- rep("Autonwoundus",length(list13))


Auto_nkill_wound_us <- data.frame(m=c(list12,list14))


Auto_nkillwoundus <- data.frame(n=c(dfAuto$nkillus,dfAuto$nwoundus))

head(Auto_nkillwoundus,50)
tail(Auto_nkillwoundus,50)
nrow(Auto_nkillwoundus)

Final_Data2 <- data.frame(Auto_nkill_wound_us,Auto_nkillwoundus)


colnames(Final_Data2)<- c("Auto_nkill_wound_us","Auto_nkillwoundus")


var.test(Final_Data2$Auto_nkillwoundus ~ Final_Data2$Auto_nkill_wound_us, 
         alternative = "two.sided")


# 
# F test to compare two variances
# 
# data:  Final_Data2$Auto_nkillwoundus by Final_Data2$Auto_nkill_wound_us
# F = 0.13772, num df = 188, denom df = 188, p-value < 2.2e-16
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.1033865 0.1834592
# sample estimates:
#   ratio of variances 
# 0.1377214 


#just like for explosive for Automatic firearms F test values is very low for 
#623 degrees of freedom hich indicates populations means of people killed and 
#people wounded are not equal.



#Analysing Date

Dataforreg <- subset(df,iday != "0" & imonth != "0" & nkill != "" & nkillus !="" & nwound !="" & nwoundus !="" ,select = c(iday,imonth,iyear,nkill,nkillus,nwound,nwoundus,country_txt))

length(Dataforreg)

nrow(Dataforreg)

head(Dataforreg)

dev.off()



plot(Dataforreg$iday,Dataforreg$nkill, col ="Orange")

plot(Dataforreg$imonth,Dataforreg$nkill, col = "Red")

plot(Dataforreg$iyear,Dataforreg$nkill, col ="Blue")


DataforregUSA <- subset(Dataforreg, country_txt == "United States")

plot(DataforregUSA$iday , DataforregUSA$nkillus, col ="Orange")

plot(DataforregUSA$imonth,DataforregUSA$nkillus, col = "Red")

plot(DataforregUSA$iyear,DataforregUSA$nkillus, col ="Blue")




#df <- na.omit(df)

dff1 <- subset(df,select = c(success, targtype1_txt, country_txt))
head(dff1)

dff2 <- subset(dff1,success != "Unknown" & success != "" & success != "NA" & success != "na" &  country_txt == "United States" & targtype1_txt != ""  &
                 targtype1_txt != "NA" & targtype1_txt != "na" & targtype1_txt != "Unknown")

head(dff2)

library(plyr)

chisq.test(table(dff2$targtype1_txt,dff2$success))

# Pearson's Chi-squared test
# 
# data:  table(dff2$targtype1_txt, dff2$success)
# X-squared = 55.497, df = 20, p-value = 3.459e-05












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
     
     main="Cluster Dendrogram for words used in summary description")


groups <- cutree(fit, k=5)

rect.hclust(fit, k=5, border="blue")