#Building and running the Geographic Weighted Regression and the Poisson GWR

# Both the Linear and Poisson Regression Models are in prior scripts. The next step is to build
# the GWR and Poisson GWR to test further. While the results have already been developed in the
# ESRI ArcMAP version, and are used to validate the results here, the below provides code for running the
# GWR and Poisson MGWR from R packages

# Necessary Libraries
# Necessary libraries (sp) and (spgwr) have already been loaded
#Adding GWModel
library(GWmodel)
require(GWmodel)

# Data load
# New shapefile loaded for analysis
EX_Dgeo<-readOGR(dsn=path.expand("C:/Users/dave/Downloads/GEOG797_report/data/shapes"), layer="EX_D_geo")

#Geographically weighted regression

# Geographically weighted regression (GWR) is an exploratory technique intended to indicate 
# where non-stationarity is taking place on the map, that is where locally weighted regression
# coefficients are not equal to their global values. 
# 
# GWR is based on the conern that the fitted coefficient values of a global model,
# fitted to all the data, may not represent detailed local variations in the data adequately
# ând in this it follows other local regression implementations. 
# 
# Where GWR differs is in not looking for the dynamics of every location- in not looking for local variation 
# in a data space, but by moving a weighted window over the data, estimating one set of coefficient values at every chosen a point. The fit points are very often the points at which observations were made, but do not have to be. If the local coefficients vary in space, it can be taken as an indication of non-stationarity.

# First Step- Setting the Bandwidth for Linear Model
# 
# Because we have the predictors for both the linear model and the Poisson model defined, we'll
# set the bandwidths for each model prior to running and mapping them.

GWRbandwidth <- gwr.sel(Excess_D ~ lng_com + pct_obe + med_ren + dr_sing + b_pop + vc_rate, data=EX_Dgeo, adapt=T)


# bw.gwr <- bw.ggwr(Excess_D ~ prop_multi + prop_wires + prop_cross + prop_singl + lng_com + pct_obe + prop_dilap + Unemployme + deep_pov,  
#                   data = ED_geo,
#                   family = "poisson",
#                   approach = "AICc",
#                   kernel = "bisquare", 
#                   adaptive = TRUE,
#                   dMat = DM)
# 
# P_GWRbandwidth <- gwr.sel(Excess_D ~ prop_multi + prop_wires + prop_cross + prop_singl + lng_com + pct_obe + prop_dilap + Unemployme, data=ED_geo, adapt=T)
# P_GWRbandwidth
# #Run the Poisson GWR
# 
# Poisson_gwr.model <- gwr(Excess_D ~ prop_multi + prop_wires + prop_cross + prop_singl + lng_com + pct_obe + prop_dilap + Unemployme + vc_rate + deep_pov, data=ED_geo, adapt = P_GWRbandwidth, hatmatrix=TRUE)