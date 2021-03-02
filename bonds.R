#to read data
bonds <- read.delim("bonds.txt",row.names = 1)
View(bonds)
str(bonds)
summary(bonds)
plot(bonds$CouponRate,bonds$BidPrice,main = "bid vs coupan price",xlab = "coupan Rate",ylab = "Bid Price")
bondsmod <- lm(BidPrice~CouponRate,data = bonds) # to bulid model
abline(bondsmod) # to draw regression line on the plot
summary(bondsmod)

#to handle outliers
plot(bondsmod$fitted.values,rstandard(bondsmod),
     main = "Residual Plot",
     xlab = "Predicted values for Bid price",
     ylab = "standardized Residuals")
abline(h=2,lty=2)
abline(h=-2,lty=2)

#below function is to identify the points which will be clicked by mouse pointer
identify(bondsmod$fitted.values,rstandard(bondsmod))

#we have identified the points,we have to remove one point at a time
#then again check for the outliers
#here we are creating the object without farthest point i.e. 13
bonds_new <- bonds[-c(4,13,34,35)]
#and also the new model
bondsmod1 <- lm(bonds_new$BidPrice~bonds_new$CouponRate)

#we have to identify the new outliers
identify(bondsmod1$fitted.values,rstandard(bondsmod1))
summary(bondsmod1) # R2 value is now more closed to 1
#remove others points one by one & do the above process once again
#not all the process, just from bonds_new ;)

#Now plot new regresssion line
plot(bonds$CouponRate[-c(4,13,34,35)],bonds$BidPrice[-c(4,13,34,35)],
     main = "Bid price vs coupan rate Without Outliers",
     xlab = "Coupan Rate",
     ylab = "Bid Price")
abline(bondsmod1)