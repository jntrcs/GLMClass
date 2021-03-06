#Model Exploration
source("DataCleaning3.R")

library(corrplot)


apply(dat, 2, FUN=function(x)sum(is.na(x)))



cor_mat<-cor(mutate_all(select(dat, 10:13), as.numeric), method="spearman", use="pairwise")  


corrplot(cor_mat, method = "square",#, main="Correlation of Emotional Reactions",
         mar=c(0,0,.1,0),addCoef.col = "white", number.cex = .8)


covars=dat%>%mutate(Male=as.numeric(.$Sex=="Male"), Married=as.numeric(.$Married=="Yes"), 
                    Children = as.numeric(.$Children=="Yes"), Income=as.numeric(.$Income))%>%select(-(10:15))%>%
  select(-c(Time,Sex))
covariate_mat<-cor(covars, method="spearman", use="pairwise")  


corrplot(covariate_mat, method = "square",#, main="Correlation of Emotional Reactions",
         mar=c(0,0,.1,0),addCoef.col = "black", number.cex = .6)



#ggplot(dat, aes(y=AbortionActive, x=as.numeric(Angry)))+geom_point()+geom_smooth()

library(reshape2)

melted=melt(select(dat,Elective,  Rape, DownSyndrome, Cancer)%>%mutate(ID=1:(nrow(.))), id.vars="ID")%>% 
  mutate(value=factor(.$value, ordered = T,  levels=c("Not at all", "A little", "Moderately", "Quite a bit", "Extremely")))
names(melted)<-c("ID", "Reason", "Compassion")

ggplot(melted) +
  geom_bar(aes(x = Reason, fill = Compassion), position = "dodge")+ggsave(file="RawNumViz.pdf")
  
  
me=dat[24,]
me

meltedAge= melt(select(dat, Age, Elective, Rape, DownSyndrome, Cancer), id="Age")%>%
  mutate(Rating  = factor(value, ordered=T,
         levels=c("Not at all", "A little", "Moderately", "Quite a bit", "Extremely")))

ggplot(meltedAge, aes(x=Age, y=jitter(as.numeric(Rating))))+geom_point()+geom_smooth()+facet_wrap(~variable)+
  ylab("Numeric Rating")#+ggsave("AgePlot.pdf")

meltedIncome= melt(select(dat, Income, Elective, Rape, DownSyndrome, Cancer), id="Income")%>%
  mutate(Rating  = factor(value, ordered=T,
                          levels=c("Not at all", "A little", "Moderately", "Quite a bit", "Extremely")))

ggplot(meltedIncome, aes(x=as.numeric(Income), y=jitter(as.numeric(Rating))))+geom_point()+geom_smooth()+facet_wrap(~variable)+
  ylab("Numeric Rating")
