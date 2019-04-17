

##Test the proportional odds assumption by fitting a logistic model using VGAM

require(VGAM)

prop.odds<-vglm(Elective~
                  Age+Sex+ReligionImportance+PoliticallyActive+as.numeric(Income)+AbortionActive+Married+Children, 
                 acat(parallel = T, reverse=T), data=scaled_dat)
summary(prop.odds)

no.prop.odds<-vglm(Elective~
                     Age+Sex+ReligionImportance+PoliticallyActive+as.numeric(Income)+AbortionActive+Married+Children,
                   acat(parallel = FALSE~AbortionActive, reverse=T),  data=scaled_dat)
summary(no.prop.odds)

delta.d<-deviance(prop.odds)-deviance(no.prop.odds)
pchisq(delta.d,3, lower.tail = F)

no.prop.odds<-vglm(Elective~
                     Age+Sex+ReligionImportance+PoliticallyActive+as.numeric(Income)+AbortionActive+Married+Children,
                   cumulative(parallel = FALSE~PoliticallyActive, reverse=T),  data=scaled_dat)
summary(no.prop.odds)

delta.d<-deviance(prop.odds)-deviance(no.prop.odds)
pchisq(delta.d,3, lower.tail = F)




###Generate data that fits the theory I'm talking about
AA<-runif(20007, 0,2)

Y=rep(0, 20007)
for (i in 1:20007){
  Y[i]= sample(1:3, 1, prob = exp(c(AA[i], .5, AA[i])))
}
             
a=vglm(Y~AA, acat(parallel=T))
summary(a)
b=vglm(Y~AA, acat(parallel=F, reverse = T))
summary(b)
pchisq(deviance(a)-deviance(b), 1, lower.tail = F)


aa=0.1
predict(b, newdata=data.frame(AA=aa), type="response")
predict(a, newdata=data.frame(AA=aa), type="response")

