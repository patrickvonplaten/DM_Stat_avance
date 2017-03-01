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
colnames(df)[6] = "SunRoof"

# 2)
#make scatterplot
library(lattice)
pairs(df[,1:4])
#make two boxplots
par(mfrow=c(1,2))
bxpABS <-boxplot(price~ABS,data=df, main = paste("Car Price with and without ABS"))
bxpSunRoof <- boxplot(price~SunRoof,data=df, main = paste("Car Price with and without Sunroofs"))
#Values of each quartile of the boxplots
bxpABS$stats
bxpSunRoof$stats

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
# modPriAll = lm(price ~.,data = df)
summary(modPriABS)
# summary(modPriAll)

# COMMENTS: It clearly can be seen that the variable "ABS" does not account for any of 
# the variance of the price, since it's R^2 is very low (0.00097). That means 
# in general that the model does not do a very good job predicting the price
# Also, it can be seen that our "Intercept" value nearly completely explains
# the price (look at the parameter values or "Pr(>|t|)"). Therefore, our prediction
# would just create a constant line (when plotting our pred), showing that the biais 
# is too high.

# COMMENT: WE SHOULD EXPLAIN THE MEANING OF EACH OF THE NUMBERS WE GET WHEN 
# DOING LINEAR REGRESSION! THEN IT IS CLEARER AND EASIER TO UNDERSTAND THE CONCLUSION WE 
# ARE ARRIVING AT TAKING INTO ACCOUNT A CERTIAN LINEAR REGRESSION

# 4)
# a)
# create modPriKM = linear regression as f(km)
modPriKM = lm(price ~ km, data = df)
summary(modPriKM)

# km is obviously a better parameter to describe the price of 
# a car since 1) the t Pr(>|t|) is much lower than the one 
# obtained above and the R-squared value is much higher meaning
# that the sum of squares of residuals of this linear regression 
# much lower than the one obtained before hinting that this linear linear
# regression fits the actual price much better

# b)
# we look for cars that have a km usage of around 50,000km, so we create 
# a variable called data50TKm that just contains the value 50 (in Thousand)
# that is gonna be used for the predict function that creates the confid intervall 
# we use the default sig. level of 95 % which is the standart for both sided confid intervall

data50TKm = data.frame(km=50)
predict(modPriKM, data50TKm, interval="confidence")

# fit      lwr      upr
# 1 4.756639 4.426392 5.086886

data135TKm = data.frame(km=135)
predict(modPriKM, data135TKm, interval="confidence")

# fit      lwr      upr
# 1 3.392284 3.238505 3.546063

# We see that the confidence intervall in the case the car has 135km 
# is much smaller at the same sig. level. That implies that the variance 
# of the price for cars having 135.000 km is lower that can be justified 
# by the fact that the price of cars already having a high usage (in terms of 
# km) is naturally lower and doesn't vary too much. Whereas for cars having
# lower usage (50.000 km) the price can vary much more since other factors 
# play a more important role (car type, car performance,...)

# c)
kmCentered = (df$km - mean(df$km))
kmCentered/df$kop1 
1/(sd(df$km))
# = 0.02242945 slightly different from 1/(sd(df$km))
# this shows that kop1 is the variable km centered and reduced

modPriKop1 = lm(price ~ kop1, data = df)
summary(modPriKop1)
 
#it can be shown that the models are logically the same since R^2
# and other important functions are equal
# need a good explanation for this one!!!
# --> explain mathematically why T VALUE, Pr(>|t|) and F-statistic and R^2
# are the same. It is obvious that centering and reducing shouldn't change 
# anything. The regression looks at how changes in the X value affect the Y vaule
# maybe the last sentence nicely formulated is already enough

# d)
# linear model is as follows: given a random sample(Y,X1,...,XN) the relation
# between the observations Y and the independant variables X1,...XN is formulated 
# as Y1 = b0 + b1*f1(X1) + ... + bN*fN(XN), where fi may be non-linear funcions.

M3 = lm(price~km + I(km^2) + I(km^3), data = df)

# Thus M3 is a linear model

summary(M3)

# comment as above

# e)
M3b = lm(price~ I(km^3) + I(km^2)+ km, data = df)

anova(M3)
anova(M3b)
# results are obviously not the same --> comment!!!
# need more information about anova ...

# f)
library(car)
?vif
# check out the vif function and calculate the variance inflation factor
# wikepedia explains how to calculate the output of the function
# https://en.wikipedia.org/wiki/Variance_inflation_factor

# g)

# TODO

# 5)

# a) look at former R tp where variables where terminated. Get different methods
# from TP and amphi

# b) validate the model using test (i guess x^2 tests)


# FOR REPORT: Note that of 172 cars 118 or about 69% have ABS technology. Does this 
# influence the results?

