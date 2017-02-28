#suppose that the data file "Centrale-DM.data" lies in the same folder
#as this file

#for patrick:
#setwd("/Users/patrickvonplaten/DM_Stat_avance/src") 
#for erik:
setwd("/Users/ebrote/Desktop/DM_Stat_avance/src") 
rm(list=ls(all=TRUE))

# 1)
#read in table format
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
bxpABS <-boxplot(price~ABS,data=df, main = paste("Price ~ f(ABS)"))
bxpOpenRoof <- boxplot(price~OpenRoof,data=df, main = paste("Price ~ f(OpenRoof)"))
#Values of each quartile of the boxplots
bxpABS$stats
bxpOpenRoof$stats

#COMMENTS:
#-------------------------scatterplot---------------------------------
# Scatterplot shows a relavitely strong correlation between price and age 
# and some correlation between age and km as well as price and km.
# TIA seems to have no influence on other variables in this model.

#---------------------------price ~ ABS--------------------------------
# The boxplot show that for cars having an ABS the median is significantly 
# high than for cars not having an ABS. Also, the variance of the price
# of cars having an ABS is much lower than for those that have one. 
# Since it can be seen that 50% of all "ABS" cars have a price that ranges
# between 2.6 and 4 with pretty much no "ABS" car having a price higher than 6
# , where as 50% of all "Non-ABS" cars have a price that ranges between 2.5 
# and 4.5 and a maximum of 7. It can be concluded that the price is somewhat 
# correlated to the dummy variable ABS, but not very strongly.

#---------------------------price ~ OpenRoof----------------------------
# It can be seen that the medians are quite similar for "Non-OpenRoof" and
# "OpenRoof" cars. The "box range" around the is smaller for "OpenRoof"
# cars, but apart from that they are no significant differences. In summary, 
# the correlation between price and OpenRoof seems to be rather small, but 
# further analysis is needed.

# 3)

# a)
# creating new data frame with "ABS" as a qualitative variable
loglDf = df
loglDf["ABS"] = as.logical(as.integer(df$ABS))
# making the two different models
modQuant = lm(price ~., data = df)
modQual = lm(price ~., data = loglDf)
# compare them 
summary(modQuant)
summary(modQual)

# COMMENTS: No difference can be seen between those two models, so there doesn't 
# seem to be a difference. It may be pertinant remeber that the logical variable
# cannot be calculated upon, e.g. we cannot calculate the average of loglDf$ABS.
# Though this should not make a difference in the final model as regression requires
# the logical variable to implemented as a dummy variable (i.e. qualitavitely) anyways.

# b)
modPriABS = lm(price ~ ABS, data = df)
summary(modPriABS)
# COMMENTS: It clearly can be seen that the variable "ABS" does not account for any of 
# the variance of the price, since it's R^2 is very low (0.00097). That means 
# in general that the model does not do a very good job predicting the price
# Also, it can be seen that our "Intercept" value nearly completely explains
# the price (look at the parameter values or "Pr(>|t|)"). Therefore, our prediction
# would just create a constant line (when plotting our pred), showing that the biais 
# is too high.

# FOR REPORT: Note that of 172 cars 118 or about 69% have ABS technology. Does this 
# influence the results?

