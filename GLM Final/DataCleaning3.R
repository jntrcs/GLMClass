library(dplyr)
library(tidyr)
library(tibble)
library(readr)

data= read_csv("AbortionSurveyEdited.csv")

text<-data[1,]
data<-data[-(1:2),]

#hist(as.numeric(data$Duration..in.seconds.))


dat<-transmute(data, Time=log(as.numeric(`Duration (in seconds)`)), Age=as.numeric(Q1),
               Sex=Q2, ReligionImportance = as.numeric(Q3_1), PoliticallyActive = as.numeric(Q20_1),
               Income=factor(Q4,ordered=T, levels=c("$0 - 25,000", "$26,000 - 50,000", 
                                                    "$51,000 - 75,000", "$76,000 - 100,000", "$100,000+")),
               AbortionActive = apply(mutate_all(mutate_all(
                 select(data, starts_with("Q8_")),factor, ordered=T,
                 levels=c("Never", "Rarely", "Occasionally", "Frequently", "Very Frequently")), as.numeric),
                 1, sum),
               Married=Q5, Children=Q18, Elective= Q10_2, Rape=Q11_2, DownSyndrome=Q13_2, Cancer=Q15_2) %>%
  replace_na(., replace=list(Age=mean(.$Age, na.rm=T),
                            Elective = "Not at all", DownSyndrome="Not at all"))%>%
  #Elective = "Extremely", DownSyndrome="Extremely"))%>%
  # I will use the modes for now, but when I am done I will switch to the opposite end of 
  #the spectrum and make sure none of my major conclusions switch.
  #Grieved = "Not at all", Compassionate = "Extremely", Alarmed="Not at all"))%>%
  
  mutate_at(10:13, factor, ordered=T,levels=c("Not at all", "A little", "Moderately", "Quite a bit", "Extremely"))




