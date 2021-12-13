install.packages("car")
install.packages("caret")
install.packages("gvlma")
install.packages("predictmeans")
install.packages("e1071")
install.packages("lmtest")

library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")

#Do the number of powerboats registered in Florida impact the number of manatees killed there by powerboats each year?

#Testing for linearity 

scatter.smooth(x=manatees$PowerBoats, y=manatees$ManateeDeaths, main="Manatee Deaths by Powerboats")

#Creating a linear model 

lmMod = lm(ManateeDeaths~PowerBoats, data=manatees)

par(mfrow=c(2,2))
plot(lmMod)

lmtest::bptest(lmMod)

car::ncvTest(lmMod)

distBCMod1 = caret::BoxCoxTrans(manatees$ManateeDeaths)
print(distBCMod1)

manatees= cbind(manatees, dist_newM= predict(distBCMod1, manatees$ManateeDeaths))

lmMod_bc2= lm(dist_newM~PowerBoats, data=manatees)
lmtest::bptest(lmMod_bc2)

gvlma(lmMod_bc2)

CookD(lmMod, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
lev = hat(model.matrix(lmMod))
plot(lev)

manatees[lev>.2,]

car::outlierTest(lmMod)

summary(influence.measures(lmMod))

summary(lmMod_bc2)

summary(lmMod)















