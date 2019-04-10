#Model Fitting.R
library(mvord)
source("DataCleaning3.R")
#load("BaseMod.Rdata")


scaled_dat<-dat%>%mutate(Income=as.numeric(Income))%>%
  mutate_at(.vars=c("Age", "ReligionImportance", "PoliticallyActive",
                             "AbortionActive", "Income"), .funs=scale) %>%
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


