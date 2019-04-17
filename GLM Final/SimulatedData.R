##Simulation study
cutpoint<-c(-Inf, 0, 1, 2.5, 3, Inf)

b0s<- c(2,2,0, .2)
b1s<-c(0, 0, 1, -2)
b2s<-c(.5,.5,-1,-.5)

x1<-rnorm(207)
x2<-rnorm(207)

errors<-diag(.1,4)+.03

exresponse<-rep(1, 207)%*%t(b0s) +x1 %*%t(b1s) + x2%*% t(b2s)
library(mvtnorm)
library(mvord)
observed=exresponse+rmvnorm(207, mean=rep(0,4), errors)

data=data.frame(Y1 = sapply(observed[,1], FUN=function(x)sum(x>cutpoint)),
             Y2 = sapply(observed[,2], FUN=function(x)sum(x>cutpoint)),
             Y3 = sapply(observed[,3], FUN=function(x)sum(x>cutpoint)),
             Y4 = sapply(observed[,4], FUN=function(x)sum(x>cutpoint)),
             X1 = x1, X2 = x2)


mod= mvord(formula=MMO2(Y1, Y2, Y3, Y4)~X1+X2,
                     data=data, 
                     error.structure = cov_general())
summary(mod)

library(reshape2)
library(dplyr)
library(tibble)
library(tidyr)
gates=melt(rownames_to_column(data.frame(mod$theta), "Break"))
names(gates)[2]<-c("Situation")
cutoffs=gates%>%mutate(ID= rep(1:4, 4) )%>% group_by(Situation) %>%
  separate(Break, c("Name", "Trash"), "\\|")%>%
  rename(EndPoint= value)%>%mutate(StartPoint=lag(EndPoint, default=-2)
                                   )%>%
  mutate(MidPoint=(StartPoint+EndPoint)/2)%>%
  select(-c(ID, Trash))%>%ungroup()%>%
  add_row(Name="5", Situation=unique(.$Situation), StartPoint=filter(., Name=="4")$EndPoint,
          EndPoint=StartPoint+1, MidPoint=NA)%>%mutate(MidPoint=(StartPoint+EndPoint)/2)


source("Prediction.R")
predictions=predict(mod)

preds=melt(predictions%>%mutate(ID=1:207), id="ID")%>% rename(Prediction=value, Situation=variable)
truth=melt(data%>%select(Y1, Y2, Y3, Y4)%>%mutate(ID=1:207), id="ID")%>%
  rename(Truth = value, Situation=variable)

predDat<-full_join(truth, preds, by=c("ID", "Situation"))%>%
  mutate(y=(as.numeric(Truth)-.5)
         /5+runif(nrow(.),-.1,.1))

predDat=predDat%>%filter(Situation=="Y4")
cutoffs<-cutoffs%>%filter(Situation=="Y4")

cutoffs$StartPoint[1]<--6.5
cutoffs$EndPoint[5]<-7.3

ggplot(cutoffs)+facet_wrap(~Situation)+geom_rect(aes(xmin=StartPoint, xmax=EndPoint, fill=Name,
                                                     ymin=-.1, ymax=1.1), alpha=.3)+
  scale_y_continuous(name="True Response", breaks=1:5/5, 
                     labels=1:5)+
  scale_x_continuous(name="Latent Variable Score", 
                     limits=c(min(predDat$Prediction)-1, .4+max(predDat$Prediction)))+
  geom_point(data=predDat,aes(x=Prediction, y=y, color=factor(Truth)))+
  guides(fill=guide_legend("Latent Cutoffs"))# +ggsave("IdealCutoffPlot.pdf")
