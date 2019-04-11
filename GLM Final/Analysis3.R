library(mvord)
source("DataCleaning3.R")
#load("BaseMod.Rdata")
library(tidyr)
library(reshape2)


coefs=data.frame(Names=names(coef(base.mod)),Betahat = coef(base.mod), Confint=confint(base.mod))


names(coefs)[3:4]<-c("Lower Bound", "Upper Bound")
coefs_for_plot=coefs%>%separate(Names, into = c("Covariate", "Situation"), sep=" ") %>%
  mutate(Situation = factor(Situation, ordered=T))%>%
  mutate(Situation = recode(Situation, `1`= "Elective",
                            `2`="Rape",
                            `3`="Down Syndrome",
                            `4`="Cancer"),
         Covariate=plyr::revalue(.$Covariate, replace=c(`as.numeric(Income)`="Income"
                                                                    ))
        
         
         )%>% filter(Covariate!="(Intercept)")#%>%arrange(Situation)


pd <- position_dodge(width=-.5)




ggplot(coefs_for_plot, aes(Covariate, y= Betahat, color=Situation))+
  geom_point(position= pd, size=3, aes(shape=Situation))+
  geom_errorbar(position = pd,aes(ymin=`Lower Bound`, ymax=`Upper Bound`), size=1.5)+
  scale_shape_manual(values=16:19)+geom_hline(yintercept = 0, size=1)+
  scale_y_continuous(name="Effect on compassion\n Less | More", breaks=c(-1,-.5,0,.5,1))+
  coord_flip()+ scale_color_brewer(palette="Spectral")#+ggsave("CoefficientPlot.pdf")



gates=melt(rownames_to_column(data.frame(base.mod$theta), "Break"))
names(gates)[2]<-c("Situation")
cutoffs=gates%>%mutate(ID= rep(1:4, 4) )%>% group_by(Situation) %>%
  separate(Break, c("Name", "Trash"), "\\|")%>%
  rename(EndPoint= value)%>%mutate(StartPoint=lag(EndPoint, default=-2),
    Name= factor(Name, ordered=T,
                 levels=c("Not at all", "A little", "Moderately", "Quite a bit", "Extremely")))%>%
  mutate(MidPoint=(StartPoint+EndPoint)/2)%>%
  select(-c(ID, Trash))%>%ungroup()%>%
  add_row(Name="Extremely", Situation=unique(.$Situation), StartPoint=filter(., Name=="Quite a bit")$EndPoint,
          EndPoint=StartPoint+1, MidPoint=NA)%>%mutate(MidPoint=(StartPoint+EndPoint)/2)
          

source("Prediction.R")
predictions=predict(base.mod)

preds=melt(predictions%>%mutate(ID=1:207), id="ID")%>% rename(Prediction=value, Situation=variable)
truth=melt(dat%>%select(Elective, Rape, DownSyndrome, Cancer)%>%mutate(ID=1:207), id="ID")%>%
  rename(Truth = value, Situation=variable)

predDat<-full_join(truth, preds, by=c("ID", "Situation"))%>%
  mutate(Truth=factor(Truth, ordered=T,
            levels=c("Not at all", "A little", "Moderately", "Quite a bit", "Extremely")))%>%
  mutate(y=(as.numeric(Truth)-.5)
         /5+runif(nrow(.),-.1,.1))


ggplot(cutoffs)+facet_wrap(~Situation)+geom_rect(aes(xmin=StartPoint, xmax=EndPoint, fill=Name,
                                                     ymin=-.1, ymax=1.1), alpha=.3)+
  scale_y_continuous(name="True Response", breaks=1:5/5, 
                     labels=c("Not at all", "A little", "Moderately", "Quite a bit", "Extremely"))+
  scale_x_continuous(name="Latent Variable Score", 
                     limits=c(min(predDat$Prediction)-1, .4+max(predDat$Prediction)))+
  geom_point(data=predDat,aes(x=Prediction, y=y, color=Truth))+
  guides(fill=guide_legend("Latent Cutoffs")) #+ggsave("CutoffPlot.pdf")


error_eps<-error_structure(base.mod)[[2]]
cor_eps<-cov2cor(error_eps)
library(corrplot)

corrplot(cor_eps, method = "square",#, main="Correlation of Emotional Reactions",
         mar=c(0,0,.1,0),addCoef.col = "black", number.cex = .6)
