install.packages("lme4")
library("lme4")
library(corrplot)
x_cor<- cor(cbind(x[,12:14]))
x_cor;
corrplot(x_cor,method = "circle")
names(x)

#Descriptive Analysis of All Data

x<-read.csv("Refrigerator_Alldata_Cleaned.csv")
hist(x$AEC,main="Distribution of AEC",xlab="AEC",col ="cyan") #AEC distribution
abline(v=mean(x$AEC),col="red",lwd=2)

plot(x$AEC~x$TotalVol,xlab="Total volume of refrigerator",ylab="AEC",col="blue") #Plot of AEC vs TotalVolume of the fridge
abline(lsfit(x$AEC,x$TotalVol),col="red")
plot(x$AEC~x$RefVol,xlab="Refrigerator Volume",ylab="AEC",col="blue") #Plot of AEC vs Refrigerator volume
abline(lsfit(x$AEC,x$TotalVol),col="red")
plot(x$AEC~x$FreezerVol,xlab="Freezer Volume",ylab="AEC",col="blue")#Plot of AEC vs Freezer volume
abline(lsfit(x$AEC,x$FreezerVol),col="red")
#Plot of AEC vs Type of refrigerator
plot(x$AEC~x$Type, xlab="Type of refrigerator",ylab="AEC",names=c("Celliers ","Compact","Refrigerator"),col="cyan")

plot(x$AEC~x$Defrost,xlab="Type of Defrost",ylab="AEC",col="cyan") #Plot of AEC vs Type of defrost

plot(x$AEC~x$FreezerLoc,xlab="Freezer location",ylab="AEC",col="cyan")#Plot of AEC vs Freezer location

plot(x$AEC~x$DoorIce,xlab="Ice Door",ylab="AEC",col="cyan")#Plot of AEC vs Ice Door

#Descriptive Analysis of Refrigerator Data

x1_cor<- cor(cbind(x2[,12:14]))
x1_cor;
corrplot(x1_cor,method = "number")

x1<-read.csv("Ref_Data_cleaned.csv")

hist(x1$AEC,main="Distribution of AEC",xlab="AEC",col="cyan")
#AEC distribution
abline(v=mean(x1$AEC),col="red",lwd=2)
plot(x1$AEC~x1$TotalVol,xlab="Total volume of refrigerator",ylab="AEC",col="blue") #Plot of AEC vs TotalVolume of the fridge
abline(lsfit(x1$AEC,x1$TotalVol),col="red")
plot(x1$AEC~x1$RefVol,xlab="Refrigerator Volume",ylab="AEC",col="blue") #Plot of AEC vs Refrigerator volume
abline(lsfit(x1$AEC,x1$RefVol),col="red")
plot(x1$AEC~x1$FreezerVol,xlab="Freezer Volume",ylab="AEC",col="blue")#Plot of AEC vs Freezer volume

#Plot of AEC vs Type of refrigerator
plot(x1$AEC~x1$Type,xlab="Type of Refrigerator",ylab="AEC",names=c("Compact","Refrigerator"),col="cyan")

plot(x1$AEC~x1$Defrost,xlab="Type of Defrost",ylab="AEC",names=c("Automatic","Manual","Partial Automatic"),col="cyan") #Plot of AEC vs Type of defrost

plot(x1$AEC~x1$FreezerLoc,xlab="Freezer location",ylab="AEC",col="cyan")#Plot of AEC vs Freezer location

plot(x1$AEC~x1$DoorIce,xlab="Ice Door",ylab="AEC",col="cyan")#Plot of AEC vs Ice Door

#Descriptive Analysis of Wine Chillers Data
names(x2)
x2<-read.csv("Wine Chiller Data_Cleaned.csv")
x2_cor<- cor(cbind(x2[,12:13]))
x2_cor;
corrplot(x2_cor,method = "number")

hist(x2$AEC,main="Distribution of AEC",xlab="AEC",col="cyan") #AEC distribution
abline(v=mean(x2$AEC),col="red",lwd=2)
#plot(x2$AEC~x2$TotalVol,xlab="Total volume of refrigerator",ylab="AEC") #Plot of AEC vs TotalVolume of the fridge

plot(x2$AEC~x2$RefVol,xlab="Refrigerator Volume",ylab="AEC",col="blue") #Plot of AEC vs Refrigerator volume
abline(lsfit(x2$RefVol,x2$AEC),col="red")
#plot(x2$AEC~x2$FreezerVol,xlab="Freezer Volume",ylab="AEC")#Plot of AEC vs Freezer volume

#Plot of AEC vs Type of refrigerator
#plot(x2$AEC~x2$Type,xlab="Type of Refrigerator",ylab="AEC")

plot(x2$AEC~x2$Defrost,xlab="Type of Defrost",ylab="AEC",names=c("Automatic","Manual"),col="cyan") #Plot of AEC vs Type of defrost

#plot(x2$AEC~x2$FreezerLoc,xlab="Freezer location",ylab="AEC")#Plot of AEC vs Freezer location

#plot(x1$AEC~x1$DoorIce,xlab="Ice Door",ylab="AEC")#Plot of AEC vs Ice Door

plot(x1$TotalVol~x1$Defrost,xlab="Type of Defrost",ylab="Total Volume of Refrigerator",col="blue")
plot(x2$TotalVol~x2$Defrost,xlab="Type of Defrost",ylab="Total Volume of Refrigerator",col="blue")

#Applying the models

ws<- read.csv("C:/Users/Dips/Desktop/ankit SDM/Wine Chiller signature.csv")

#Simple OLS for wine chiller
Sm4<-lm(sqrt(AEC) ~  as.factor(Defrost) +RefVol+as.factor(Defrost)*RefVol, data = ws)
summary(Sm4)  # R2: 32.62

# lmer(random effect of brands) for wine chillers
Sm5m <- lmer(sqrt(AEC) ~ as.factor(Brand)+ as.factor(Defrost)+as.factor(Defrost)*RefVol+
               RefVol+(1|Brand), data = ws) 
summary(Sm5m)
AIC(Sm4);AIC(Sm5m)   #1077,896


#--------------------------------------------------------------------------------------

r <- read.csv("C:/Users/Dips/Desktop/ankit SDM/Refrigerator data_cleaned signature.csv")

r$FreezerLoc <- relevel(r$FreezerLoc,"No freezer")

# simple OLS
Sm2<-lm(AEC ~ as.factor(Type) + as.factor(Defrost) + as.factor(FreezerLoc) 
        + Ratio , data = r)
summary(Sm2)   # R2: 71.53

# lmer without interaction
Sm3m <- lmer(AEC ~ as.factor(Brand)+as.factor(Type) + as.factor(Defrost) 
             + as.factor(FreezerLoc)+ Ratio +(1|Brand), data = r)
summary(Sm3m)

# lmer with interaction
im2 <- lmer(AEC ~ as.factor(Brand)+as.factor(Type) + as.factor(Defrost) 
            + as.factor(FreezerLoc)+ Ratio+ r$FreezerVol*as.factor(Defrost) +as.factor(DoorIce) +(1|Brand), data = r)
summary(im2)

AIC(Sm2);AIC(Sm3m);AIC(im2)    #14171,13127,12700

#----------------------------------------------------------------------------------------------




