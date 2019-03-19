rawData<-read.csv("SLC.csv")

library(dplyr)

data<-transmute(rawData, Cain= as.numeric(curscain%in% 1:2),
                 Priesthood = as.numeric(psthneg%in% 1:2),
                BOMScore = as.numeric(morofthr==3)+as.numeric(helaman==4)+as.numeric(captmoro==2),
                BibScore = as.numeric(dnyjesus==5)+as.numeric(bkofacts==2)+as.numeric(lietoapl==4),
                Age= dplyr::recode(age, `1`= 18, `2` = 22.5, `3` = 28, `4`=33, `5` =38,
                            `6`=43,`7`=48, `8`=53, `9`=58, `10` = 67, .default=NA_real_),
                Darwin = as.numeric(darwin %in% 1:2),
                College= relevel(factor(case_when(
                  yeareduc%in% 1:3 ~'NotHSGrad',
                  yeareduc%in% 4:6 ~ 'HS+',
                  yeareduc%in% 7:8~'College+',
                  yeareduc%in% 9:10 ~'Advanced'
                )), ref="NotHSGrad"),

                Gender=dplyr::recode(i.sex, `1`="Male", `2`="Female"),
                Orthodoxy=ortho-2,
                Convert = as.numeric(status%in%3:4),
                Progressive = as.numeric(consrvat%in% 1:2),
                Concerns = relevel(factor(case_when(
                  policy==1 ~'Accept',
                  policy==3 ~'Voice',
                  policy %in%c(2,4,5)~'Protest'
                )), ref="Accept")
                )

head(data)
str(data)

#Concerns has a lot of NAs and I don't want to throw out that data on all the others, so
#just randomly assign the variable based on the frequency
data$Concerns[is.na(data$Concerns)]<-sample(c("Accept", "Protest", "Voice"), replace=T, 
                                      sum(is.na(data$Concerns)), prob = table(data$Concerns))
missing=data[apply(data, 1, anyNA),]
apply(data, 2, function(x)sum(is.na(x)))
