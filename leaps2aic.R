# taken from https://www.zoology.ubc.ca/~schluter/R/fit-model/ on Dec 31, 2013
# removed the "print" portion near end of code because it wasn't enabling it to complete. It appears to still produce the relevant output though.

leaps2aic<-function(x,y,z){
  # Calculate AIC and cAIC for every model stored in "z", the
  # output of "leaps". This version sorts the output by AICc.
  # Requires the same x and y objects inputted to "leaps"
  rows<-1:length(z$Cp)
  for(i in 1:nrow(z$which)){
    vars<-which(z$which[i,])
    newdat<-cbind.data.frame(y,x[,vars])
    znew<-lm(y~.,data=newdat)
    L<-logLik(znew)
    aL<-attributes(L)
    z$model[i]<-paste(z$label[-1][vars],collapse=" + ")
    z$p[i]<-z$size[i]  # terms in model, including intercept
    z$k[i]<-aL$df      # parameters (including sigma^2)
    z$AIC[i]<- -2*L[1] + 2*aL$df
    z$AICc[i]<- -2*L[1] + 2*aL$df +
      (2*aL$df*(aL$df+1))/(aL$nobs-aL$df-1)
    z$BIC[i]<- -2*L[1] + log(aL$nobs)*aL$df
  }
  z$AIC<-z$AIC-min(z$AIC)
  z$AICc<-z$AICc-min(z$AICc)
  z$BIC<-z$BIC-min(z$BIC)
  result<-do.call("cbind.data.frame",
                  list(z[c("model","p","k","Cp","AIC","AICc","BIC")], 
                       stringsAsFactors=FALSE))
  result<-result[order(result$AICc),]
  rownames(result)<-1:nrow(result)
  newdat<-cbind.data.frame(y,x)
  #print(summary( lm( as.formula(paste("y~",result$model[1])), 
                    # data=cbind.data.frame(y,x) )))
  return(result)
}