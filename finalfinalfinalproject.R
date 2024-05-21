library(leaps)
library(car)
library(corrplot)
library(MASS)
library(tree)
library(rpart)
library(rattle)
library(tidyverse)
library(caret)
library(Metrics)
library(randomForest)                
library(sparkline)
library(visNetwork)

data=read.csv("C:/Users/ashut/OneDrive/Desktop/STAT 515/Final_Project/indian-auto-mpg.csv")
any(is.na(data))
data$Seats=as.factor(data$Seats)
data$Manufacturer=as.factor(data$Manufacturer)
data$Location=as.factor(data$Location)
data$Fuel_Type=as.factor(data$Fuel_Type)
data$Owner_Type=as.factor(data$Owner_Type)
data$Transmission=as.factor(data$Transmission)
data$carage=2023-data$Year
data$carage=as.factor(data$carage)

table(data$Manufacturer)
datac=data[data$Manufacturer%in%c('Honda','Hyundai'),]

#Price of diesel car with automatic transmission and second and first owner are highest price.

#EDA
ggplot(data=datac, aes(x=carage,y=Price)) + geom_boxplot(fill='cyan',color='black')+
  labs(x="Carage",
       y="Prices",
       title="Age of Car vs Price of Car") 
ggplot(data=datac, aes(x=Fuel_Type,y=Price)) + geom_boxplot(fill='cyan',color='black')+
  labs(x="Fuels",
       y="Prices",
       title="Fuels used by Car vs Price of Car") 
ggplot(data=datac, aes(x=Transmission,y=Price)) + geom_boxplot(fill='cyan',color='black')+
  labs(x="Transmission",
       y="Prices",
       title="Transmission of Car vs Price of Car") 
ggplot(data=datac, aes(x=Owner_Type,y=Price)) + geom_boxplot(fill='cyan',color='black')+
  labs(x="Owner Type",
       y="Prices",
       title="Owner Number of Car vs Price of Car") 


#hypothesis test for fuel type.
dieselprice=datac$Price[datac$Fuel_Type=='Diesel']
pricemean=mean(datac$Price)
samplesizediesel=sum(datac$Fuel_Type=='Diesel')
samplesizepetrol=sum(datac$Fuel_Type=='Petrol')
alpha=0.05
results=t.test(dieselprice,mu=pricemean)
p=results$p.value
t=results$statistic
print("H0= Price of car with Diesel<=Price of car with petrol")
print("H1= Price of car with Diesel>Price of car with petrol")
cat("Alpha:",alpha)
cat("P value:",p)
cat("T statistics:",t)
if(p<=alpha)
{
  print("Reject Null Hypothesis")
}else
  print("Accept Null Hypothesis")

#hypothesis test for transmission type.
autoprice=datac$Price[datac$Transmission=='Automatic']
results1=t.test(autoprice,mu=pricemean)
t1=results1$statistic
p1=results1$p.value
print("H0= Price of car with Automatic<=Price of car with Manual")
print("H1= Price of car with Automatic>Price of car with Manual")
cat("Alpha:",alpha)
cat("P value:",p1)
cat("T statistics:",t1)
if(p1<=alpha)
{
  print("Reject Null Hypothesis")
}else
  print("Accept Null Hypothesis")

#hypothesis test for owner type.
owntype=datac$Price[datac$Owner_Type=='First']
results2=t.test(owntype,mu=pricemean)
t2=results2$statistic
p2=results2$p.value
print("H0= Price of car with First Owner<=Price of car with rest of the Owner")
print("H1= Price of car with First Owner>Price of car with rest of the Owner")
cat("Alpha:",alpha)
cat("P value:",p2)
cat("T statistics:",t2)
if(p2<=alpha)
{
  print("Reject Null Hypothesis")
}else
  print("Accept Null Hypothesis")

trainindexprice = sample(1:nrow(datac), 0.8 * nrow(datac))
traindataprice=datac[trainindexprice,]
testindexprice= sample(1:nrow(datac),0.2*nrow(datac))
testdataprice=datac[testindexprice,]

tre=rpart(Price~carage+Fuel_Type+Transmission+Owner_Type,data=traindataprice)
summary(tre)
visTree(tre)

frame=data.frame(Transmission='Automatic',carage='4',Fuel_Type='Diesel',Owner_Type='First')
pre=predict(tre,newdata=frame)
cat("Predicted Price ",pre)

#extent is changing from manual to auto and changing petrol types.
#rf
rn=randomForest(Price~carage+Fuel_Type+Transmission+Owner_Type,data=traindataprice,importance=TRUE)
summary(rn)
importance(rn)
plot(rn)
