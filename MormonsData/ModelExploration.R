mod=glm(Cain~BOMScore+BibScore+Age+College+Gender+Convert+ BOMScore:BibScore,
        family = binomial, data=data)
summary(mod)

mod=glm(Cain~BOMScore+BibScore+Age+College+Gender+Darwin +Orthodoxy+Convert+Progressive+Concerns,
        family = binomial, data=data)
summary(mod)
