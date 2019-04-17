#Model Fitting.R
library(mvord)
source("DataCleaning3.R")
#load("BaseMod.Rdata")


scaled_dat<-dat%>%mutate(Income=as.numeric(Income))%>%
  mutate(LogAge=log(Age), Above30 = as.numeric(Age>30))%>%
  mutate_at(.vars=c("Age", "ReligionImportance", "PoliticallyActive",
                             "AbortionActive", "Income", "LogAge", "Above30"), .funs=scale) %>%
  mutate_at(c("Children", "Sex", "Married"), funs(scale(as.numeric(factor(.)))))

base.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
              Age+Sex+ReligionImportance+PoliticallyActive+as.numeric(Income)+AbortionActive+Married+Children, 
            data=data.frame(scaled_dat), 
            error.structure = cov_general())



summary(base.mod)
AIC(base.mod)

#save(base.mod, file="BaseMod.Rdata")



logit.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                   Age+Sex+ReligionImportance+PoliticallyActive+as.numeric(Income)+AbortionActive+Married+Children, 
                 data=data.frame(dat), link=mvlogit(), error.structure = cor_general())
AIC(logit.mod)
#Logistic mod has a larger AIC by 2, so we will consider the easier to interpret probit model


cov.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                   Age+Sex+ReligionImportance+PoliticallyActive+as.numeric(Income)+AbortionActive+Married+Children, 
                 data=data.frame(scaled_dat), 
                 error.structure = cov_general()) #Switching between cov and cor doesn't change AIC, but cov is more interpretable to me



coef.constrains=matrix(NA, byrow = F, nrow=4, ncol=9)
coef.constrains[,1]<-1:4
coef.constrains[3,2]<-1
coef.constrains[1,4]<-1
coef.constrains[1,5]<-1
coef.constrains[2,7]<-1
coef.constrains[1,9]<-1
coef.constrains[3,9]<-2


cons.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                  Age+Sex+ReligionImportance+PoliticallyActive+as.numeric(Income)+AbortionActive+Married+Children, 
                data=data.frame(scaled_dat), 
                error.structure = cov_general(), coef.constraints = coef.constrains)

#Add a quadratic term so that
base.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                   Age+Sex+ReligionImportance+PoliticallyActive+as.numeric(Income)+AbortionActive+Married+Children, 
                 data=data.frame(scaled_dat), 
                 error.structure = cov_general())

#Model with just intercepts
int.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                   1, 
                   data=data.frame(scaled_dat), 
                   error.structure = cov_general())
AIC(int.mod)


##Just religion
religion.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                     ReligionImportance, 
                   data=data.frame(scaled_dat), 
                   error.structure = cov_general())

AIC(religion.mod) #Better than base and justintercept

##Just religion and age
relage.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                       ReligionImportance+Age, 
                     data=data.frame(scaled_dat), 
                     error.structure = cov_general())

AIC(relage.mod) #Better than just religion

##Just religion, children and age
relagekid.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                     ReligionImportance+Age+Children, 
                   data=data.frame(scaled_dat), 
                   error.structure = cov_general())

AIC(relagekid.mod) #Better than just religion and age


##What if we log age?
rellagekid.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                        ReligionImportance+LogAge+Children, 
                      data=data.frame(scaled_dat), 
                      error.structure = cov_general())
AIC(rellagekid.mod) ##Better than unlogged

##What if instead of logging age, we create an indicator for age>30 and add the interaction?
above30.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                         ReligionImportance+Age+Age:Above30+Children, 
                       data=data.frame(scaled_dat), 
                       error.structure = cov_general())
summary(above30.mod)
AIC(above30.mod) ##Better than unlogged

##What if add income and income^2?
rellagekidI.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                         ReligionImportance+LogAge+Children+as.numeric(Income)+
                         I(as.numeric(Income)^2), 
                       data=data.frame(scaled_dat), 
                       error.structure = cov_general())
AIC(rellagekidI.mod) ##Worse

##What if we add an interaction between religion and children
rellagekidInter.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                         ReligionImportance*Children+LogAge, 
                       data=data.frame(scaled_dat), 
                       error.structure = cov_general())
AIC(rellagekidInter.mod) ##worse than no interaction

##What if we add an interaction between religion and age
rellagekidInter2.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                              ReligionImportance*LogAge+Children, 
                            data=data.frame(scaled_dat), 
                            error.structure = cov_general())
AIC(rellagekidInter2.mod) ##worse than no interaction

##add abortion activity
abact.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                        ReligionImportance+Age+Children+AbortionActive, 
                      data=data.frame(scaled_dat), 
                      error.structure = cov_general())

AIC(abact.mod) #Worse than last

#Add an abortion activity * religion interaction to test my theory
abactI.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                    ReligionImportance*AbortionActive+Age+Children, 
                  data=data.frame(scaled_dat), 
                  error.structure = cov_general())

AIC(abactI.mod) #Worse than last

##Try political activity instead
polact.mod<- mvord(formula=MMO2(Elective, Rape, DownSyndrome, Cancer)~
                    ReligionImportance+Age+Children+PoliticallyActive, 
                  data=data.frame(scaled_dat), 
                  error.structure = cov_general())

AIC(polact.mod) #Worse than last


