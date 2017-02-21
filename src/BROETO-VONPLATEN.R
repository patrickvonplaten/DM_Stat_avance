#suppose that the data file "Centrale-DM.data" lies in the same 
#as this file

#for patrick:
setwd("/Users/patrickvonplaten/DM_Stat_avance/src") 
#for erik:
#setwd("...") 
rm(list=ls(all=TRUE))

# 1)
#read in table
df = read.table("Centrale-DM.data", header = TRUE)
#check observations and variables
dim(df)
#rename colname of extra1 to "ABS" and extra2 to "OpenRoof"
colnames(df)[5] = "ABS"
colnames(df)[6] = "OpenRoof"

# 2)
#make scatterplot
library(lattice)
pairs(df[,1:4])
#make two boxplots
par(mfrow=c(1,2))
boxplot(price~ABS,data=df, main = paste("Price ~ f(ABS)"))
boxplot(price~OpenRoof,data=df, main = paste("Price ~ f(OpenRoof)"))
#comment: 
#-------------------------scatterplot---------------------------------
#scatterplot shows a rather strong correlation between price and age 
# and some correlation between age and km and price and km
# TIA seems to have no influence on any other variable in this model

#---------------------------price ~ ABS--------------------------------
# boxplots show that for cars having an ABS the medium is significantly 
# lower than for cars not having an ABS. Also, the variance of the price
# of cars having an ABS is much lower than for those that have one, 
# since it can be seen that 50% of all "ABS" cars have a price that ranges
# between 2.6 and 4 with pretty much no "ABS" car having a price higher than 6
# , wheres 50% of all "Non-ABS" cars have a price that ranges between 2.5 
# and 4.5 and a maximum of 7. It can be concluded that the price is somewhat 
# correlated to (yes/no ABS), but not very strongly.
#---------------------------price ~ OpenRoof----------------------------
# it can be seen that the medium is exactly the same for "Non-OpenRoof" and
# "OpenRoof" cars. The 50% range around the medium is smaller for "OpenRoof"
# cars, but apart from that they are no significant differences. To sum this 
# up, the correlation between price and OpenRoof seems to be rather small.

# 3)

# a)
# creating new data frame with "ABS" as a qualitative variable
logDf = df
logDf["ABS"] = as.logical(as.integer(df$ABS))
# making the two different models
modQuant = lm(price ~., data = df)
modQual = lm(price ~., data = logDf)
# compare them 
summary(modQuant)
summary(modQual)
# no difference can be seen between those two models, so there doesn't 
# seem to be a difference
# !!! not a 100% save on this one !!!

# b)
modPriABS = lm(price ~ ABS, data = df)
summary(modPriABS)
#It clearly can be seen that the variable "ABS" does not account for any of 
# the variance of the price, since its R^2 is very low (0.00097). That means 
# in general that the model does not do a very good job predicting the price
# Also, it can be seen that our "Intercept" value nearly completely influences
# the model (look at the parameter values or "Pr(>|t|)"). Therefore, our prediction
# would just create a constant line (when plotting our pred), showing that the biais 
# is too high


