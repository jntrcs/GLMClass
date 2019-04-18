##Check normally distributed errors
load("BaseMod.RData")
source("DataCleaning3.R")
source("Prediction.R")
preds<-predict(base.mod)%>%mutate(ID=1:207)
head(preds)
actual = select(dat, Elective, Rape, DownSyndrome, Cancer)%>%mutate(ID=1:207)

library(reshape2)
gates=melt(rownames_to_column(data.frame(base.mod$theta), "Break"))
names(gates)[2]<-c("Situation")
cutoffs=gates%>%mutate(ID= rep(1:4, 4) )%>% group_by(Situation) %>%
  separate(Break, c("Name", "Trash"), "\\|")%>%
  rename(EndPoint= value)%>%mutate(StartPoint=lag(EndPoint, default=-1),
                                   Name= factor(Name, ordered=T,
                                                levels=c("Not at all", "A little", "Moderately", "Quite a bit", "Extremely")))%>%
  mutate(MidPoint=(StartPoint+EndPoint)/2)%>%
  select(-c(ID, Trash))%>%ungroup()%>%
  add_row(Name="Extremely", Situation=unique(.$Situation), StartPoint=filter(., Name=="Quite a bit")$EndPoint,
          EndPoint=StartPoint+mean(.$EndPoint-.$StartPoint), MidPoint=NA)%>%mutate(MidPoint=(StartPoint+EndPoint)/2)

resids=inner_join(melt( preds, id.vars = "ID")%>%rename(Scenario=variable, Prediction=value),
melt(actual, id.vars = "ID")%>%rename(Scenario=variable, Truth=value))

head(cutoffs)
residuals=left_join(resids, 
          select(cutoffs, Name, Situation, StartPoint, EndPoint)%>%
            mutate(Truth=Name, Scenario=Situation, Ytilde=runif(nrow(.), StartPoint, EndPoint)))%>%
    select(-one_of(c(StartPoint, EndPoint)))%>%
  mutate(Error=Ytilde-Prediction)

ggplot(residuals, aes(x=Error))+geom_histogram(bins=25)+facet_wrap(~Scenario)+ggsave("ResidualHists.pdf")


ggplot(residuals,aes(sample=Error))+stat_qq()+facet_wrap(~Scenario)

plot(residuals$Error[residuals$Scenario=="Rape"]~residuals$Error[residuals$Scenario=="Cancer"])

residuals%>%group_by(Scenario)%>%summarize(sd(Error, na.rm=T))
