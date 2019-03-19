load("CasellaBergerTrain.Rdata")
require(dplyr)
require(stringr)
require(sentimentr)


#str(train)
train$date<-lubridate::mdy(train$date)
train<-mutate(train, text=as.character(train$review.text))%>%select(-one_of("review.text"))

#hist(train$stars)
#table(train$stars, train$verified)


#head(train)


sent=sentiment_by(train$text)

train<- mutate(train, wordCount=sent$word_count, sentiment=sent$ave_sentiment,
               stars=factor(train$stars, ordered = T))

rm(sent)

train<-mutate(train, mentionsPaper=grepl( "([Pp][Aa][Pp][Ee][Rr])|([Pp][Rr][Ii][Nn][Tt])",train$text))
train$helpful[is.na(train$helpful)]<-.5
train = mutate(train, cover= factor(case_when(
  !hardcover & !paperback ~'neither',
  hardcover ~ 'hardcover',
  paperback~'paperback'
))) %>% select(-one_of(c("paperback","hardcover")))
#train%>%group_by(verified)%>%summarise(a= mean(as.numeric(stars)-as.numeric(bestGuess)))
#train%>%group_by(paperback)%>%summarise(a= length(stars))


# ggplot(train, aes(x=mentionsPaper, y=stars))+geom_boxplot()
# 
# ggplot(train, aes(x=sentiment, y=stars))+geom_point()
# ggplot(train, aes(x=wordCount, y=stars))+geom_point()
# 
# 
# for (i in 1:68){
#   print(train[i, c(1,9,11)])
# }
# 
# 
# boxplot(log(sent$word_count)~train$stars)


#ggplot(train, aes(x=date, y=stars))+geom_point()+geom_smooth()
