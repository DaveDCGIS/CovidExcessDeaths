#Exploratory Data Analysis for Excess Deaths

#This section will illustrate broad methods used to understand the variables selected

#Part of the methodology includes constructing various hypotheses, using the SDOH taxonomy, around Excess Deaths

# Here we build three different hypotheses to try different combinations of variables based upon the SDOH taxonomy.
# See the data dictionary for descriptions of the varibles.
# 
# Rule 1: No more than 3 predictors from any category, and every category has at least one variable
# Rule 2: No repetition in the variables 
# Rule 3: Combination of socio-demographic and built env. characteristics, not exclusive
# 
# Step 1- Build the Hypotheses and check correlations plots. 

#Install libraries
library(corrplot)
require(corrplot)
library(RColorBrewer)
require(RColorBrewer)

#Create Data Sets
#There are three hypotheses to be tested applying the SDOH Significant Seven
#Broad Hypothesis Equation shown below
#Linear Model
#Y= ??0 + ??1 X1 + ??2 X2 + ??3X3+ ??4 X4+ ??5 X5+ ??6 X6 + ??? 
# Where,
# ??0 = intercept
# ??1 X1 = coefficient + explanatory variable
# ??? = error term
#Y = ??0 (Excess Deaths) + ??1 (Coeff.) X1 (Exp. Variable 1) + ??2 (Coeff. )X2 (Exp. Variable 2) + ??3 (Coeff.) X3 (Exp. Variable 3) + ??4 (Coeff.)X4 (Exp. Variable 4)+ ??5 (Coeff.) X5 (Exp. Variable 5) + ??? (Random Error/Residuals)

#Poisson Model
# Yi ~ Poisson [N exp(??0+ ???K ??K??K)]
# Where,
# ??0 = intercept
# ??K = coefficient of independent variable Xk
# N = offset variable

#First dataset
#Create the Hypothesis Test Data Layer
Hyp_Test1<-subset(ExcessD_df_comp[,c("Excess_D","prop_multi", "prop_wires", "ren30", "prop_cross", "prop_singl", "lng_com", "vac_h", "ei_35to64", "pct_obe", "prop_green", "a_35_64", "min_pop", "prop_dilap", "pct_pv", "Unemployme")])

#Run the corrplot
Hyp1_Corr<-corrplot(cor(Hyp_Test1[c("Excess_D","prop_multi", "prop_wires", "ren30", "prop_cross", "prop_singl", "lng_com", "vac_h", "ei_35to64", "pct_obe", "prop_green", "a_35_64", "min_pop", "prop_dilap","pct_pv", "Unemployme")]), method = "number", addCoef.col="grey", number.cex=0.5, tl.col = "black", col = colorRampPalette(c("midnightblue", "white","darkred"))(100), type="upper", order="hclust")
title(sub="First Hypothesis Group", adj=0.1)

#Remove variables with high correlations, particularly ei_35to64
Model_One<-subset(Hyp_Test1[,c("Excess_D","prop_multi", "prop_wires", "ren30", "prop_cross", "prop_singl", "lng_com", "vac_h", "pct_obe", "prop_green", "min_pop", "prop_dilap", "pct_pv", "Unemployme")])
Model_One_Corr<-corrplot(cor(Model_One[c("Excess_D","prop_multi", "prop_wires", "ren30", "prop_cross", "prop_singl", "lng_com", "vac_h", "pct_obe", "prop_green", "min_pop", "prop_dilap", "pct_pv", "Unemployme")]), method = "number", addCoef.col="grey", number.cex=0.5, tl.col = "black", col = colorRampPalette(c("midnightblue", "white","darkred"))(100), type="upper", order="hclust")
title(sub ="HTG1- Final Test Variables", adj=0.1, line=-0.5)

#Second dataset
#Create the Hypothesis Test Data Layer

#Run the second set of hypothesis tests on different variables
Hyp_Test2<-subset(ExcessD_df_comp[,c("Excess_D","vac_hou", "ooc_hou", "med_ren", "dr_sing", "s_coll", "vac_sh", "medc_65", "pct_smo", "a_o_65", "b_pop", "hisp_pop", "vc_rate", "deep_pov", "Unemployed")])

#Run the corrplot
Hyp2_Corr<-corrplot(cor(Hyp_Test2[c("Excess_D","vac_hou", "med_ren", "dr_sing", "s_coll", "vac_sh", "medc_65", "pct_smo", "b_pop", "hisp_pop", "vc_rate", "deep_pov", "Unemployed")]), method = "number", addCoef.col="grey", number.cex=0.5, tl.col = "black", col = colorRampPalette(c("midnightblue", "white","darkred"))(100), type="upper", order="hclust")
title(sub="Second Hypothesis Group", adj=0.1)

#Refine the Correlation Plots and Reduce the Number of Variables (this was subjective)
Model_Two<-subset(Hyp_Test2[,c("Excess_D","vac_hou", "med_ren", "dr_sing", "vac_sh", "pct_smo", "b_pop", "vc_rate", "deep_pov", "Unemployed")])
Model_Two_Corr<-corrplot(cor(Model_Two[,c("Excess_D","vac_hou", "med_ren", "dr_sing", "vac_sh", "pct_smo", "b_pop","vc_rate", "deep_pov", "Unemployed")]), method = "number", addCoef.col="grey", number.cex=0.5, tl.col = "black", col = colorRampPalette(c("midnightblue", "white","darkred"))(100), type="upper", order="hclust")
title(sub ="HTG2 Final Test Variables", adj=0.1, line=-0.5)

#Third Dataset
#Create the Hypothesis Test Data Layer
Hyp_Test3<-subset(ExcessD_df_comp[,c("Excess_D","mhh_inc", "ren_occ", "al_dr", "dr_de", "vac_u", "c_high", "medc_65", "pct_obe", "a_25_34", "a_35_64", "w_pop", "anum_vc", "num_pov")])

#Run the correlation plot
Hyp3_Corr<-corrplot(cor(Hyp_Test3[c("Excess_D","mhh_inc", "ren_occ", "al_dr", "dr_de", "vac_u", "c_high", "pct_obe", "a_25_34", "a_35_64", "w_pop", "anum_vc", "num_pov")]), method = "number", addCoef.col="grey", number.cex=0.5, tl.col = "black", col = colorRampPalette(c("midnightblue", "white","darkred"))(100), type="upper", order="hclust")
title(sub="Third Hypothesis Group", adj=0.1)

#Refine the Correlation Plots and Reduce the Number of Variables- removed both of the two age group variables
Model_Three<-subset(Hyp_Test3[,c("Excess_D","mhh_inc", "dr_de", "vac_u", "pct_obe", "w_pop", "anum_vc", "num_pov")])
Hyp_Corr3_ref<-corrplot(cor(Model_Three[,c("Excess_D","mhh_inc", "dr_de", "vac_u", "pct_obe", "w_pop", "anum_vc", "num_pov")]), method = "number", addCoef.col="grey", number.cex=0.5, tl.col = "black", col = colorRampPalette(c("midnightblue", "white","darkred"))(100), type="upper", order="hclust")
title(sub ="HTG3 Final Test Variables", adj=0.1, line=-0.5)

#End result- three datasets to test using Linear and Poisson models to find variable significance.

