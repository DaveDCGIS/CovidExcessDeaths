#Variable Section for Excess Deaths

#Two types of models will be used to refine the variables, testing each set of hypotheses for signifcance
#Second, a linear model will be used to estimate the proper set of variables


#Test the first Linear Model- using the same data as in the Poisson model
Hyp_lm1<- lm(Excess_D~.,Model_One)
summary(Hyp_lm1)

#Remove some of the less signifcant variables
Hyp_lm1_red<-lm(Excess_D ~ prop_cross + prop_singl + lng_com + pct_obe + min_pop + pct_pv, Model_One)
summary(Hyp_lm1_red)

vif(Hyp_lm1_red)
lm_vif_values <- vif(Hyp_lm1_red)

barplot(lm_vif_values, main = "VIF Values",col = 'blue',ylim = c(0.0,8.0),  cex.axis=1.5, cex.names=0.5, adj=0.4)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="HTG1 Final Test Variables")

crPlots((Hyp_lm1_red), ylim=c(0,2000))

#From this we have to drop prop_cross, prop_singl, a_35_64 and min_pop

#Resulting dataset
LM_Model_1_fin<-subset(Model_One[,c("Excess_D", "lng_com", "pct_obe", "pct_pv")])

#Test the second Linear Model- using the same data as in the Poisson model
Hyp_2m1<- lm(Excess_D~.,Model_Two)
summary(Hyp_2m1)

#Remove some of the less signifcant variables
Hyp_lm2_red<-lm(Excess_D ~ med_ren + dr_sing + pct_smo + b_pop + vc_rate + deep_pov + Unemployed, Model_Two)
summary(Hyp_lm2_red)

vif(Hyp_lm2_red)
lm_vif_values2 <- vif(Hyp_lm2_red)

barplot(lm_vif_values2, main = "VIF Values",col = 'blue',ylim = c(0.0,8.0),  cex.axis=1.5, cex.names=0.5, adj=0.175)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="HTG2")

crPlots((Hyp_lm2_red), ylim=c(0,2000))

#From this we have to drop a_o_65, vac_hou and Unemployed

#Resulting dataset
LM_Model_2_fin<-subset(Model_Two[,c("Excess_D", "med_ren", "dr_sing", "pct_smo", "b_pop", "vc_rate", "deep_pov")])

#Test the third Linear Model- using the same data as in the Poisson model
Hyp_3m1<- lm(Excess_D~.,Model_Three)
summary(Hyp_3m1)

#Remove some of the less signifcant variables
Hyp_lm3_red<-lm(Excess_D ~mhh_inc + dr_de + w_pop + anum_vc + num_pov, Model_Three)
summary(Hyp_lm3_red)

vif(Hyp_lm3_red)
lm_vif_values3 <- vif(Hyp_lm3_red)

barplot(lm_vif_values3, main = "VIF Values",col = 'blue',ylim = c(0.0,8.0),  cex.axis=1.5, cex.names=0.5, adj=0.01)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="HTG3")

crPlots((Hyp_lm2_red), ylim=c(0,2000))

#From this we have to drop a_o_65, vac_hou and Unemployed

#Resulting dataset
LM_Model_3_fin<-subset(Model_Three[,c("Excess_D", "vac_u")])

#Combine all three linear model datasets

Final_LMData<-cbind(LM_Model_1_fin, LM_Model_2_fin,LM_Model_3_fin)

#Test the combined variables in the same Poisson model for significance and multicollinearity
Linear_Model_F<-glm(Excess_D ~., data=Final_LMData)
summary(Linear_Model_F)

#Check the Variance Inflation Factor for Multicollinearity
vif(Linear_Model_F)
vif_LM_F<- vif(Linear_Model_F)

#Graph VIF 
barplot(vif_LM_F, main = "VIF Values" ,col = 'blue',ylim = c(0.0,8.0), cex.axis=1.5, cex.names=0.5, adj=0.2)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="Linear Model Three")

#We can see from the results that the follwing variables drop
#pct_smo, deep_pov, vac_u and pct_pv because of multi-collinearity problems

LinearM_Final<-subset(Final_LMData[,c("Excess_D","lng_com", "pct_obe", "med_ren", "dr_sing","b_pop","vc_rate")])

#Testing the Final Model
Linear_Model_Ref<-glm(Excess_D ~., data=LinearM_Final)
summary(Linear_Model_Ref)

vif(Linear_Model_Ref)
vif_L_Ref<- vif(Linear_Model_Ref)

#Graph VIF 
barplot(vif_L_Ref, main = "VIF Values- Final Variable Selection",col = 'blue',ylim = c(0.0,8.0), cex.axis=1.5, cex.names=0.4, adj=0.2)
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')
title(sub="HTG- Test-Linear Model")
