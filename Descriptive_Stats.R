#Descriptive Stats for Covid Excess Deaths

#Install Libraries to Load Data and Shapefile

library(spgwr)
library(rgdal)
require(rgdal)
library(sp)
require(sp)
library(ggplot2)
require(ggplot2)

#Read in Data
ExcessD<-read.csv("C:/Users/dave/Downloads/GEOG797_report/data/CapTotal_Data.csv", header=T)
ExcessD_df<-as.data.frame(ExcessD)

#Check Data for Completeness and remove any NA observations
## Check for missing values
is.na(ExcessD_df)
## Remove data without complete cases
ExcessD_df_comp <- ExcessD_df[complete.cases(ExcessD_df), ]

#Read in Shapefile
ED_geo<-readOGR(dsn=path.expand("C:/Users/dave/Downloads/GEOG797_report/data/shapes"), layer="ed_analysis")

#Check Shapefile via Plot
plot(ED_geo)

#Histogram of Excess Deaths values and Excess Deaths per 100K
qplot(ExcessD_df_comp$Excess_D, geom="histogram", binwidth = 100,  
      main = "Histogram of Excess Death Counts", 
      xlab = "Counties",  
      ylab = "Total Amount of Excess Deaths",
      fill=I("blue"), 
      col=I("red"), 
      xlim=c(0,7500),
      ylim=c(0,500))

#Histogram of Excess Deaths values and Excess Deaths per 100K
qplot(ExcessD_df_comp$Cov_100K, geom="histogram", binwidth = 10,  
      main = "Histogram of Excess Death Rates per 100K", 
      xlab = "Counties", 
      ylab = "Rates of Excess Deaths",
      fill=I("blue"), 
      col=I("red"), 
      xlim=c(0,3000),
      ylim=c(0,200))
