#Fitting a non-parametric curve through a scatterplot

data <- read.table("jaws.txt", header = TRUE)
attach(data)
names(data)
# Presentation of graphics (mfrow) means multiple frames by row
par(mfrow = c(1,1)) #default
par(mfrow = c(1,2)) # 2 plots on a single screen
par(mfrow = c(2,2)) # 4 panel of plots on a single screen
par(mfrow = c(2,2))


## LOWESS
plot(age, bone, pch = 20, main = "Lowess")
lines(lowess(age,bone), col = "red")
# It is a reasonable fit overall, but a poor descriptor of the jaw size for the lowest five ages.

## LOESS ##
# which is a model-fitting function.We use the fitted model to predict the jaw sizes:
plot(age, bone, pch = 20, main = "Loess")
model <- loess(bone~age)
xv<- 0:50
yv<-predict(model, data.frame(age = xv))
lines(xv, yv, col = "red")


#GENERALIZED ADDITIVE MODEL
#install.packages("mgcv")
library(mgcv)
plot(age, bone, pch = 20)
model<- gam(bone ~s(age))
yv <- predict(model, list(age = xv))
lines(xv, yv, col = "red")

## POLYNOMIAL ###
plot(age, bone, pch = 20, main = "Cubic Polynomial")
model <- lm(bone ~ age + I(age^2) + I(age^3))
yv = predict(model, list(age = xv))
lines(xv, yv, col = "red")

