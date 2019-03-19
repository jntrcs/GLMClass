

ggplot(data, aes(y=Cain, x=BibScore))+geom_smooth()+geom_point()
ggplot(data, aes(y=Cain, x=BOMScore))+geom_smooth()+geom_point()
ggplot(data, aes(y=Cain, x=BibScore*BOMScore))+geom_smooth()+geom_point()
ggplot(data, aes(y=Cain, x=BibScore*Age))+geom_smooth()+geom_point()
ggplot(data, aes(y=Cain, x=Age*BOMScore))+geom_smooth()+geom_point()

ggplot(data, aes(y=Cain, x=Age))+geom_smooth()+geom_point()
ggplot(data, aes(y=Cain, x=Orthodoxy))+geom_smooth()+geom_point()
ggplot(data, aes(y=Cain, x=Priesthood))+geom_smooth()+geom_point()


table(data$Cain, data$College)
table(data$Cain, data$Gender)
table(as.logical(data$Cain), data$Priesthood)
table(data$Cain,data$Progressive)
table(data$Cain, data$Concerns)
table(data$Cain, data$Darwin)

plot(aggregate(Cain~College, data=data, FUN=mean))
plot(aggregate(Cain~Orthodoxy, data=data, FUN=mean))
plot(aggregate(Cain~BOMScore, data=data, FUN=mean))
plot(aggregate(Cain~BibScore, data=data, FUN=mean))
table(data$BibScore)
plot(aggregate(Cain~as.numeric(Gender=="Male"), data=data, FUN=mean))
plot(aggregate(Cain~Darwin, data=data, FUN=mean))

plot(aggregate(Cain~Convert, data=data, FUN=mean))

aggregate(Cain~Concerns, data=data, FUN=mean)
aggregate(Cain~Progressive, data=data, FUN=mean)

table(data$Orthodoxy, data$BOMScore)

table(data$College)/sum(table(data$College))

require(reshape2)
bom=data%>%group_by(BOMScore)%>%summarise(Prop=mean(Cain))
bible=data%>%group_by(BibScore)%>%summarise(Prop=mean(Cain))
ggplot()+geom_line(data=bom, size=2, aes(x=BOMScore, y=Prop, color="BoM"))+
  geom_line(data=bible,size=2, aes(x=BibScore, y=Prop, color="Bible"))+ylab("% Supporting Cain Theory")+
  xlab("Knowledge Score")+ggtitle("Scriptural knowledge vs. Cain Theory") +
  theme(legend.title=element_blank(),text = element_text(size=15))

names(data)
table(data$BOMS)


