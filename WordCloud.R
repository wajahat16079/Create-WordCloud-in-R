
## packages required 

library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)

## loading text
Tweets = readRDS("tweets.rds")


## Extracting words out of the text
Words <- Tweets %>% 
  unnest_tokens(word, text)%>%
  select(name,word)

## get count of each word
WordsCount= Words %>%
  group_by(word)%>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

## your own list you can add words in this list: these wont appear inthe wordcloud
Removelist = c(
  "سی","کے","the","کی","میں","t.co","https", "of","کا","ہے" , "to" ,
  "hai","ch","that’s" ,"two","then","dr","etc","b","د","تے","که","that",
  "پر" ,"مزید","و","ہیں","لئے" ,"fb.me" ,"in", "is","a",
  "کو","اور","http","سے","i","خان","نے" ,
  "تو","کیا","اس" ,    "پروگرام","دیکھیں","لنک"   ,
  "it","آئی","پہ"  , "with","ھے","ہو","we"  ,"تھے","بھی","on" ,
  "and","amp" , "یہ"  ,"کر",
  "کہ" ,"w" ,"for","my","this",
  "gt","lt",
  "جو","گا","ایک","تھا" ,
  "by","have","he","گیا","ھیں","u","آج","ساتھ" ,"i’ll","us","from"    
)




WordsFiltered = WordsCount %>%
  filter(!word %in% Removelist & 
           !word %in% stopwords("en")&  
           !word %in% stopwords("SMART"))



dev.new(width = 1000, height = 1000, unit = "px")

wordcloud(WordsFiltered$word, WordsFiltered$Count, scale=c(5,0.5),
          min.freq = 5, max.words= 200,
          colors=brewer.pal(8, "Dark2"))










