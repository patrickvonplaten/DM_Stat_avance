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
#par(mfrow=c(1,2))
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
# Also what is very important is that 135.000 km is the mean. Since the sample 
# follows a t distribution, it is normal that there is more data around 135.000km (the distribution is 
# denser). Therefore the 95% confidence intervall has a shorter lenght than the one at 50.000km

# c)
kmCentered = (df$km - mean(df$km))
kmCentered/df$kop1 
1/(sd(df$km))
# = 0.02242945 slightly different from 1/(sd(df$km))
# this shows that kop1 is the variable km centered and reduced

modPriKop1 = lm(price ~ kop1, data = df)
summary(modPriKop1)

 
#it can be shown that the models are logically the same, since R^2
# and other important functions are equal

# Theoretically, it is obvious that two linear regressions are if their regression lines (y) are parallel. A modified 
# linear regression line stays parallel as long as all the values of the input data are multiplied by some value and/or 
# some value is added to them. Since in our case all the input data is substracted by the same value the average and in the following
# divided (=multiplied by its inverse) by the same value = the standard deviation, it is theoretically evident that the linear regression 
# model has the same outcome



# d)
# linear model is as follows: given a random sample(Y,X1,...,XN) the relation
# between the observations Y and the independant variables X1,...XN is formulated 
# as Y1 = b0 + b1*f1(X1) + ... + bN*fN(XN), where fi may be non-linear funcions.

M3 = lm(price~km + I(km^2) + I(km^3), data = df)

# Thus M3 is a linear model

summary(M3)

# comment as above by evaluating R^2 and the rest

# e)
M3b = lm(price~ I(km^3) + I(km^2)+ km, data = df)

anova(M3)
anova(M3b)

# When applying the anova method to the two models, we can see different results even though the three input variables are the same. 
# This is due to the order the input varibles are taken in as an input. In our case, anova() determines how much variance is explained by the 
# first entry (km e.g.) and tests its significance, then what portion of the remaining variance is explained by the next variable (km^2) 
# and tests its significance and so forth. Thus, the remaining portion will differ depending on the first variable being inserted and therefore 
# different significances (Pr(>F) ) is the result.


# f)
library(car)
# let's check out the vif function
?vif
vif(M3)

# In order to calculate the vif of a model, we have to take the three input variables 
# being km, km^2 and km^3 in our case and do a linear regression for all three of them 
# using the other remaining two variables as the input variables (e.g. for km: km ~ I(km^2) + I(km^3)). 
# Then we take the coefficient of determination of each model (R^2) and apply the formula 1 / ( 1 - R^2 )
# to them. This is going to get us the variance inflation factor (VIF) of each variable.

vifKM = 1 / (1 - summary(lm(km ~I(km^2) + I(km^3),data = df))$r.squared)
vifKM
vifKM2 = 1 / (1 - summary(lm(I(km^2) ~ km + I(km^3),data = df))$r.squared)
vifKM2
vifKM3 = 1 / (1 - summary(lm(I(km^3) ~I(km^2) + km ,data = df))$r.squared)
vifKM3

#It can clearly be seen that all the vif values are high meaning that the R-squared is close to one.
# That implies that the function predicts the actual values quite well. The best value is that of vifKM2, 
# which makes sense because km^2 can easily be modeled by summing up  (factor * kw) and (km^3 / factor) where 
# as it is harder to model km by taking its square and cube because their functions are both bigger than km.

# g)

#create function to check wether mean is approx 0 and var is approx 1
centReduc <- function(var){
  mean = all.equal(1,sd(var))
  vari = all.equal(0,mean(var))
  ls <- list(mean, vari)
  return (ls)
}

centReduc(df$kop1)
centReduc(df$kop2)
centReduc(df$kop3)

# all differences are close enough to 1 or 0 respectively, so variables are 
# centered and reduced 

all.equal(0,cov(df$kop1,df$kop2))
all.equal(0,cov(df$kop3,df$kop2))
all.equal(0,cov(df$kop3,df$kop2))

# all differences are close to zero, so the variables are orthogonal to each other

M3c = lm(price ~ kop1 + kop2 + kop3, data = df)
summary(M3c)

#comment as we did before: R squared is the same, p-value is the same 
# but the intercept is less important which makes sense considering the fact 
# that the price is much closer to the reduced and centralized km then the initial km
# --> talk about why it is good to centralize and reduce the varibles


# in order to construct kop1, kop2 and kop3 we will use linear regression models to 
# get the intercept value and the other respective value to create the kop variables being:
#   kop1 = a*km + i
#   kop2 = a*km^2 + b*km + i 
#   kop3 = a*km^3 + b*km^2 + c*km + i

kop1Coef = lm(kop1~km,data = df)$coefficients
kop2Coef = lm(kop2~I(km^2) + km,data = df)$coefficients
kop3Coef = lm(kop3~I(km^3) + I(km^2) + km,data = df)$coefficients

summary(lm(kop1~km,data = df))
summary(lm(kop2~I(km^2) + km,data = df))
summary(lm(kop3~I(km^3) + I(km^2) + km,data = df))

kop1 = kop1Coef[1] + kop1Coef[2]*df$km
kop2 = kop2Coef[1] + kop2Coef[2]*(df$km^2) + kop2Coef[3]*df$km
kop3 = kop3Coef[1] + kop3Coef[2]*df$km^3 + kop3Coef[3]*df$km^2 + kop3Coef[4]*df$km

all.equal(df$kop1,kop1)
all.equal(df$kop2,kop2)
all.equal(df$kop3,kop3)

#It can be noticed that the constructed variables are the same as the variables kop1,kop2 and kop3


# 5)

# a) look at former R tp where variables where terminated. Get different methods
# from TP and amphi

# b) validate the model using test (i guess x^2 tests)




# FOR REPORT: Note that of 172 cars 118 or about 69% have ABS technology. Does this 
# influence the results?

