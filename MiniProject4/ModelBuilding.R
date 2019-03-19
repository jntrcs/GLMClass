source("DataCleaning.R")

require(VGAM)

# mod<-vglm(stars~verified+log(helpful)+
#             log(wordCount)+ sentiment+mentionsPaper+
#             cover, cumulative(parallel = T),  data=train)
# summary(mod)
# preds<-predict(mod, train, type="response")
# prediction=apply(preds,1, which.max)
# plot(jitter(prediction)~jitter(as.numeric(train$stars)))

MSE<-function(preds, actual){
  mean((preds-as.numeric(actual))^2)
}

fiveFoldCV<-function(formula, data,fam =cumulative, para=T, plot=T){
  data$group<-sample(1:5, nrow(data), replace=T)
  data$bestGuess<-0
  for(i in 1:5){
    test<-filter(data, group==i)
    train<-filter(data, group!=i)
    mod<-vglm(formula, data=train, family=fam(parallel=para))
    data$bestGuess[data$group==i]<-apply(predict(mod, test, type="response"), 1, function(x){
      round(sum(x*1:5))
    })
    
  }
  #print(ggplot(data, aes(x=bestGuess, y=stars))+geom_boxplot())
  #print(select(data, -one_of("text")))
  #print(str(data))
  if(plot)print(ggplot(data, aes(x=stars, y=1, fill=factor(bestGuess)))+geom_col()+ylab("Count")+
    labs(fill="Prediction"))
  return(MSE(data$bestGuess, data$stars))
}

dist_best<-replicate(30, fiveFoldCV(stars~verified+
             log(wordCount)+ sentiment, train, plot=F))
dist_challenge<-replicate(30, fiveFoldCV(stars~verified+
             log(wordCount)+ sentiment, train, plot=F))

par(mfrow=c(2,1))
hist(dist_best)
hist(dist_challenge)
t.test(dist_best, dist_challenge)
fiveFoldCV(stars~verified+log(helpful)+
             log(wordCount)+ sentiment+mentionsPaper+ date +log(helpful):date+
             cover, train)
fiveFoldCV(stars~verified+log(helpful)+
             wordCount+ sentiment+mentionsPaper+
             cover, train)
fiveFoldCV(stars~verified+helpful+
             log(wordCount)+ sentiment+mentionsPaper+
             cover, train)
fiveFoldCV(stars~verified+helpful+
             log(wordCount)+ sentiment+
             cover, train)
fiveFoldCV(stars~verified+log(helpful)+
             log(wordCount)+ sentiment+mentionsPaper+
             cover, train, para=T, fam=acat)

mod<-vglm(stars~verified+
            log(wordCount)+ sentiment,
          data=train, cumulative(parallel = T))
summary(mod)

train$bestGuess<-apply(predict(mod, train, type="response"), 1, function(x){
  round(sum(x*1:5))})

preds<-predict(mod, train, type="response")
print(ggplot(train, aes(x=stars, y=1, fill=factor(bestGuess)))+geom_col()+ylab("Count")+
        labs(fill="Prediction"))

MSE(train$bestGuess, train$stars)



leaveOneOut<-function(train){
  for (i in 1:nrow(train)){
    mod<-vglm(stars~verified+
                log(wordCount)+ sentiment,
              data=train[-1,], cumulative(parallel = T))

    train$bestGuess[i]<-apply(predict(mod, train[i,,drop=F], type="response"), 1, function(x){
      round(sum(x*1:5))})
  }
  return(train)
}

a=leaveOneOut(train)
MSE(a$bestGuess, a$stars)
ggplot(a, aes(x=stars, y=1, fill=factor(bestGuess)))+geom_col()+ylab("Count")+
        labs(fill="Prediction")+theme(text = element_text(size=15))+ggsave("LOOResults.pdf")



