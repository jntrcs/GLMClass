#Prediction.R
#The prediction function in the mvord function makes no sens whatsoever to me, so I have written one
#to calculate the predicted latent variable, but it's not robust and probably not applicable to other scenarios


predict.mvord<-function(mod){
  a=rownames_to_column(data.frame(coef(mod)), "Break")%>%separate(Break, c("Name", "Variable"), " ")%>%
    rename(Bhat=`coef.mod.`)%>%plyr::dlply("Variable", function(x)x[,3])
  b=model.matrix(mod)
  d=lapply(1:4, FUN =function(i) b[[i]]%*%a[[i]])
  names(d)<-names(mod$theta)
  data.frame(d)
}

