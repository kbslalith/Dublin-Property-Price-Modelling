setwd("/Users/kbslalith/Desktop/Bhaskar")

# my data is saved as datax
datax <- read.csv("textmining analysis.xlsx")
names(datax)
datax$NewCol <- do.call(paste, c(datax[c("Facilitis", "Features")], sep = " ")) 
datax$NewCol <- do.call(paste, c(datax[c("NewCol", "Description")], sep = " ")) 
main <- datax$NewCol

library(NLP)
library(tm)
library(qdap)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Find the stopwords
main <- tolower(main)
main <- removePunctuation(main)
frequent_terms <- freq_terms(main, 30)
plot(frequent_terms)
#Remove the stopwords
stop_words <- c("to","a","m","with","t")
main <- removeWords(main, stopwords("english"))
main <- stripWhitespace(main)
new_stop_words <- c("m","x","bedroom","floor","double","extras","ad","property","kitchen","re","within","end","side","dublin","dontcutbelowhere","two","room","c","dontcuthere","also","l","rear","maintenance","facing")

main <- removeWords(main,new_stop_words)

frequent_terms <- freq_terms(main, 41)
plot(frequent_terms)
#Getting rid of "m" in a different way, Standard way not working. 
frequent_terms <- frequent_terms[-1,]

#Visualisation 
plot(frequent_terms)


###*******Generate word cloud*******####
wordcloud(words = frequent_terms$WORD, freq = frequent_terms$FREQ, min.freq = 50,
          max.words=400, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

table(main)

words <- stringr::str_extract(main, "([^\\s]+\\s){1}garden")
words <- as.character(words)

words <- as.data.frame(table(words))
words$words


table(words)
wordcloud(words = words$words, freq = words$Freq, min.freq = 5,
          max.words=5000, random.order=FALSE, rot.per=0.4, 
          colors=brewer.pal(8, "Dark2"))

words
# Frequency of rare garden - rear garden 1497 times
plot(words$Freq)
