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
