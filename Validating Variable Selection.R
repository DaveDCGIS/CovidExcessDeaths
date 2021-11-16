#Validating Variable Selection

#The fourth and final step prior to analysis involves implementing regsubsets, 
#using backward stepwise regression to determine if each of the models and the variable selection makes sense

#Implmenting regsubsets library

library(ISLR)
library(leaps)

#Testing against Poisson model dataset first
selectdata_P<-Poisson_Model_Ref
regfit.full<-regsubsets(Excess_D~., selectdata_P, nvmax=19)
reg.summary<-summary(regfit.full)
names(reg.summary)
plot(reg.summary$rsq, xlab="Number of Variables", ylab="RSquare", type="l" )
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l" )
points(11, reg.summary$adjr2[10], col="red", cex=4, pch=20)
title(main= "Number of Variables by RSS", sub="Poisson Model")

which.min(reg.summary$cp)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l" )
points(5, reg.summary$cp[5], col="red", cex=4, pch=20)
title(main="Number of Variables by Cp", sub= "Poisson Model")

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l" )
points(5, reg.summary$bic[5], col="red", cex=4, pch=20)
title(main="Number of Variables by BIC", sub="Poisson Model")

#Look at selected variables
plot(regfit.full, scale="r2")
plot(regfit.full, scale="bic")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
coef(regfit.full)
title(main="Poisson Model")

#Forward Stepwise Regression
regfit.fwd<-regsubsets(Excess_D~., selectdata_P, nvmax=19, method="forward")
summary(regfit.fwd)
plot(regfit.fwd, scale="Cp")
coef(regfit.fwd,which.min(summary(regfit.fwd)$cp))
title(main="Forward Regression- Variables- Poisson Model")

#BackwardStepwise Regression
regfit.bwd<-regsubsets(Excess_D~., selectdata_P, nvmax=19, method="backward")
summary(regfit.bwd)
plot(regfit.bwd, scale="Cp")
coef(regfit.bwd,which.min(summary(regfit.bwd)$cp))
title(main="Backward Regression- Variables- Poisson Model")

#Testing against Linear model dataset 
selectdata_L<-Linear_Model_Ref
regfit.full_l<-regsubsets(Excess_D~., selectdata_L, nvmax=12)
reg.summary_l<-summary(regfit.full_l)
names(reg.summary_l)
plot(reg.summary_l$rsq, xlab="Number of Variables", ylab="RSquare", type="l" )
plot(reg.summary_l$rss, xlab="Number of Variables", ylab="RSS", type="l" )
points(11, reg.summary_l$adjr2[10], col="red", cex=4, pch=20)
title(main= "Number of Variables by R2", sub="Linear Model")

which.min(reg.summary$cp)
plot(reg.summary_l$cp, xlab="Number of Variables", ylab="Cp", type="l" )
points(6, reg.summary_l$cp[6], col="red", cex=4, pch=20)
title(main="Number of Variables by Cp", sub="Linear Model")

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l" )
points(5, reg.summary$bic[5], col="red", cex=4, pch=20)
title(main="Number of Variables by BIC", sub="Linear Model")

#Look at selected variables
plot(regfit.full_l, scale="r2")
plot(regfit.full_l, scale="bic")
plot(regfit.full_l, scale="adjr2")
plot(regfit.full_l, scale="Cp")
coef(regfit.full_l)


#Forward Stepwise Regression
regfit.fwd_l<-regsubsets_l(Excess_D~., selectdata_L, nvmax=19, method="forward")
summary(regfit.fwd_l)
plot(regfit.fwd_l, scale="Cp")
coef(regfit.fwd_l,which.min(summary(regfit.fwd_l)$cp))
title(main="Forward Regression- Variables- Linear Model")


#BackwardStepwise Regression
regfit.bwd_l<-regsubsets(Excess_D~., selectdata_L, nvmax=19, method="backward")
summary(regfit.bwd_l)
plot(regfit.bwd_l, scale="Cp")
coef(regfit.bwd_l,which.min(summary(regfit.bwd_l)$cp))
title(main="Backward Regression- Variables")

#The end result is that we see alignment with the chosen variables.