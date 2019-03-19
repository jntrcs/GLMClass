# Model building
rm(list=ls())
require(MASS)
require(car)
set.seed(637)
source("DataCleaning.R")

# Let's just get rid of nulls
null_idx <- apply(data, 1, function(x) !any(is.na(x)))
df <- data[null_idx ,]

# As a reminder, the response is defined as follows:

# Cain: 
# Because of the wickedness of Cain and other forefathers of the Negroes, these
# people carry the mark of a black skin and the curse of perpetual inferiority

min_formula <- formula(Cain ~ BibScore)
full_formula <- formula(Cain
                        ~ BOMScore + BibScore + Age + Darwin + College +
                          Gender + Orthodoxy + Convert + Progressive +
                          Concerns)

# Let's look at link functions first
logit_mod <-   glm(full_formula, family=binomial("logit"), data=df)
probit_mod <-  glm(full_formula, family=binomial("probit"), data=df)
cloglog_mod <- glm(full_formula, family=binomial("cloglog"), data=df)
loglog_mod <-  glm(I(1-Cain) ~ BOMScore + BibScore + Age + Darwin + College +
                   Gender + Orthodoxy + Convert + Progressive + Concerns,
                   family=binomial("cloglog"), data=df)

for (mod in list(logit_mod, probit_mod, cloglog_mod, loglog_mod)) {
  print(logLik(mod)) 
}
# Hooray! Logit is the best!

# Let's get started on model selection
min_mod <- glm(min_formula, family=binomial(), data=df)
full_mod <- glm(full_formula, family=binomial(), data=df)
summary(min_mod)
summary(full_mod)

# At first glance, it looks like Bibscore, Age, CollegeAdvanced,
# CollegeCollege+, GenderMale, and Orthodoxy are all related

# Let's try a few selection methods
backward_mod <- stepAIC(full_mod, list(lower=min_formula), direction="backward")
both_mod     <- stepAIC(full_mod, list(lower=min_formula), direction="both")
forward_mod  <- stepAIC(min_mod, list(upper=full_formula), direction="forward")

# All three models are identical
backward_mod
both_mod
forward_mod

# Let's consider at least one interaction
Bible_Gender_mod <- glm(Cain ~ BibScore * Gender + Age + Darwin + College + Orthodoxy,
                      family=binomial(), data=df)
summary(Bible_Gender_mod)

# AIC is slightly better
AIC(backward_mod, Bible_Gender_mod)
# But neither is significant, so let's keep 'em out
anova(backward_mod, Bible_Gender_mod)
qchisq(0.95, 1)

# HS+ is almost 0; let's try removing it
df$CollegeBachelors <- as.numeric(df$College == "College+")
df$CollegeAdvanced  <- as.numeric(df$College == "Advanced")
no_HS_mod <- glm(Cain ~ BibScore + Age + Gender + Darwin +
                        CollegeBachelors + CollegeAdvanced + Orthodoxy,
                        family=binomial(), data=df)
# Essentially no difference, so let's remove it
AIC(no_HS_mod, backward_mod)
anova(no_HS_mod, backward_mod)

# Quadratic terms for BOM and Bible?
Bib2_mod <- glm(Cain ~ BibScore + I(BibScore^2) + Age + Gender + Darwin +
                       CollegeBachelors + CollegeAdvanced + Orthodoxy,
                family=binomial(), data=df)

anova(no_HS_mod, Bib2_mod)

# So the final model is...
data$CollegeBachelors <- as.numeric(data$College == "College+")
data$CollegeAdvanced  <- as.numeric(data$College == "Advanced")
data$BibScore2 <- data$BibScore^2
mod <- glm(Cain ~ BibScore + Age + Gender + Darwin +
                  CollegeBachelors + CollegeAdvanced + Orthodoxy,
           family=binomial(), data=data)


example1<-predict(mod, newdata = data.frame(BibScore=0, Age=mean(data$Age, na.rm = T),
                                  Gender="Male", Darwin=0, CollegeBachelors=0,
                                  CollegeAdvanced=0, Orthodoxy=1), se.fit=T)
require(boot)
logit_ci<-example1$fit+qnorm(c(.025,.975))*example1$se.fit
inv.logit(logit_ci)
inv.logit(example1$fit)

example2<-predict(mod, newdata = data.frame(BibScore=3, Age=mean(data$Age, na.rm = T),
                                            Gender="Male", Darwin=0, CollegeBachelors=0,
                                            CollegeAdvanced=0, Orthodoxy=1), se.fit=T)
logit_ci<-example2$fit+qnorm(c(.025,.975))*example2$se.fit
inv.logit(logit_ci)
inv.logit(example2$fit)

fit=data.frame(Fitted=fitted(mod), y=mod$y)
b=fit%>%arrange(desc(Fitted)) %>%mutate(Groups=rep(1:50, each=50)[1:913])%>%group_by(Groups)%>%
  summarise(Average=mean(y), midPoint=mean(Fitted))

ggplot(b, aes(y=Average, x=midPoint))+geom_point()+geom_abline()+
  xlab("Expected proportion believing theory")+
  ylab("Proportion believing Cain theory")+
  theme(text = element_text(size=15))
# Secondary response variable: Priesthood
# It is the will of God at present that the priesthood be withheld from Negroes
min_formula_priesthood <- formula(Priesthood ~ BibScore)
full_formula_priesthood <- formula(Priesthood
                                   ~ BOMScore + BibScore + Age + Darwin + 
                                     College + Gender + Orthodoxy + Convert +
                                     Progressive + Concerns)
full_mod_priesthood <- glm(full_formula_priesthood, family=binomial(), data=df)
both_mod_priesthood <- stepAIC(full_mod_priesthood, list(lower=min_formula_priesthood), direction="both")
summary(both_mod_priesthood)


