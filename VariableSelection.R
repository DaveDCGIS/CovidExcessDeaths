#Variable Section for Excess Deaths

#Two types of models will be used to refine the variables, testing each set of hypotheses for signifcance
#First, a Possion model followed by a general linear model

#Add library

library(glmnet)
require(glmnet)
library(car)
require(car)

#Model One- Testing Variables

Poisson_Model_1<-glm(Excess_D ~., family="poisson", data=Model_One)
summary(Poisson_Model_1)

#Check the Variance Inflation Factor for Multicollinearity
vif(Poisson_Model_1)
vif_P1<- vif(Poisson_Model_1)

#Graph VIF 
barplot(vif_P1, main = "VIF Values",col = 'blue',ylim = c(0.0,8.0), cex.axis=1.5, cex.names=0.4, adj=0.4)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="HTG1 Final Test Variables")

#From the graph we can conclude we need to drop ren_30, a_35_64 and min_pop

#Resulting dataset
PModel_1_fin<-subset(Model_One[,c("Excess_D","prop_multi", "prop_wires", "prop_cross", "prop_singl", "lng_com", "vac_h", "pct_obe", "prop_green", "prop_dilap", "pct_pv", "Unemployme")])


#Model Two- Testing Variables

Poisson_Model_2<-glm(Excess_D ~., family="poisson", data=Model_Two)
summary(Poisson_Model_2)

#Vaccine_strong hesitancy drops out due to lack of significance

#Check the Variance Inflation Factor for Multicollinearity
vif(Poisson_Model_2)
vif_P2<- vif(Poisson_Model_2)

#Graph VIF 
barplot(vif_P2, main = "VIF Values",col = 'blue',ylim = c(0.0,8.0), cex.axis=1.5, cex.names=0.5, adj=0.175)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="HTG2")

#From the graph we can conclude we need to drop vac_hou, a_o_65, b_pop, hisp_pop and Unemployed
PModel_2_fin<-subset(Model_Two[,c("Excess_D", "med_ren", "dr_sing","pct_smo","vc_rate", "deep_pov")])

#Model Three- Testing Variables

Poisson_Model_3<-glm(Excess_D ~., family="poisson", data=Model_Three)
summary(Poisson_Model_3)

#Check the Variance Inflation Factor for Multicollinearity
vif(Poisson_Model_3)
vif_P3<- vif(Poisson_Model_3)

#Graph VIF 
barplot(vif_P3, main = "VIF Values",col = 'blue',ylim = c(0.0,8.0), cex.axis=1.5, cex.names=0.5, adj=0.0)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="HTG3")

#From the graph we can conclude we need to drop dr_de, a_35_64, w_pop, num_pov
PModel_3_fin<-subset(Model_Three[,c("Excess_D","mhh_inc","vac_u", "pct_obe")])

#Final set of variables
Final_PData<-cbind(PModel_1_fin, PModel_2_fin,PModel_3_fin)

#Test the combined variables in the same Poisson model for significance and multicollinearity
Poisson_Model_F<-glm(Excess_D ~., family="poisson", data=Final_PData)
summary(Poisson_Model_F)

#Check the Variance Inflation Factor for Multicollinearity
vif(Poisson_Model_F)
vif_P_F<- vif(Poisson_Model_F)

#Graph VIF 
barplot(vif_P_F, main = "VIF Values",col = 'blue',ylim = c(0.0,8.0), cex.axis=1.5, cex.names=0.5, adj=0.01)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="Final Poisson Model")

#We can see from the results that the follwing variables drop
#pct_pov, mhh_inc, vac_u, vac_h

Poisson_Final<-subset(Final_PData[,c("Excess_D","prop_multi", "prop_wires","lng_com","pct_obe", "prop_green", "prop_dilap","Unemployme","mhh_inc", "dr_sing","pct_smo","vc_rate","deep_pov")])

#Testing the Final Model
Poisson_Model_Ref<-glm(Excess_D ~., family="poisson", data=Poisson_Final)
summary(Poisson_Model_Ref)

vif(Poisson_Model_Ref)
vif_P_Ref<- vif(Poisson_Model_Ref)

#Graph VIF 
barplot(vif_P_Ref, main = "VIF Values- Final Variable Selection",col = 'blue',ylim = c(0.0,8.0), cex.axis=1.5, cex.names=0.4, adj=0.2)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="HTG-Test- Poisson Model")

