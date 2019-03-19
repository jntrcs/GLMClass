#Requires the packages dplyr, stringr, sentimentr, and vglm
casellaberger<-function(train, test){
  require(dplyr)
  require(stringr)
  require(sentimentr)
  require(VGAM)
  ggplotAvailable=require(ggplot2)
  
  
  #str(train)
  train<-mutate(train, text=as.character(train$review.text))%>%select(-one_of("review.text"))
  test<-mutate(test, text=as.character(test$review.text))%>%select(-one_of("review.text"))
  
  #hist(train$stars)
  #table(train$stars, train$verified)
  
  
  #head(train)
  
  
  sent=sentiment_by(train$text)
  sent_test=sentiment_by(test$text)
  
  train<- mutate(train, wordCount=sent$word_count, sentiment=sent$ave_sentiment,
                 stars=factor(train$stars, ordered = T))
  
  test<- mutate(test, wordCount=sent_test$word_count, sentiment=sent_test$ave_sentiment,
                stars=factor(test$stars, ordered = T))
  train<-mutate(train, mentionsPaper=grepl( "([Pp][Aa][Pp][Ee][Rr])|([Pp][Rr][Ii][Nn][Tt])",train$text))
  test<-mutate(test, mentionsPaper=grepl( "([Pp][Aa][Pp][Ee][Rr])|([Pp][Rr][Ii][Nn][Tt])",test$text))
  
  train$helpful[is.na(train$helpful)]<-.5
  test$helpful[is.na(test$helpful)]<-.5
  
  train = mutate(train, cover= factor(case_when(
    !hardcover & !paperback ~'neither',
    hardcover ~ 'hardcover',
    paperback~'paperback'
  ))) %>% select(-one_of(c("paperback","hardcover")))
  
  test = mutate(test, cover= factor(case_when(
    !hardcover & !paperback ~'neither',
    hardcover ~ 'hardcover',
    paperback~'paperback'
  ))) %>% select(-one_of(c("paperback","hardcover")))
  
  mod<-vglm(stars~verified+
              log(wordCount)+ sentiment, data=train, cumulative(parallel = T))
  
  MSE<-function(preds, actual){
    mean((as.numeric(preds)-as.numeric(actual))^2)
  }
  
  preds<-apply(predict(mod, test, type="response"), 1, function(x){
    round(sum(x*1:5))})
  test$bestGuess<-preds
  if(ggplotAvailable)print(ggplot(test, aes(x=stars, y=1, fill=factor(bestGuess)))+geom_col()+ylab("Count")+
          labs(fill="Prediction"))
  MSE(preds, test$stars)
  
}
#casellaberger(train[1:68,], train[1:68,])
