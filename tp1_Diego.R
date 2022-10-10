#*****TP_1:UP_1 - Probabilités Avancées
library(car)


#Pre-processing
dataTot=read.table("Data_app.txt", header=TRUE)
dataTest=read.table("Data_test.txt", header=TRUE)

#______________________________________________________________

#1. Tracer les 3 séries en question
{
#Transformar en series de tiempo 
dataTot$kwh=ts(dataTot$kwh,start=c(2006,1),freq=12)
dataTot$htdd=ts(dataTot$htdd,start=c(2006,1),freq=12)
dataTot$cldd=ts(dataTot$cldd,start=c(2006,1),freq=12)
dataTot$date=time(dataTot$kwh)

dataTest$htdd=ts(dataTest$htdd,start=c(2020,1),freq=12)
dataTest$cldd=ts(dataTest$cldd,start=c(2020,1),freq=12)
dataTest$date=time(dataTest$htdd)

#Sacar el último año para probar y el resto para entrenar
dataTotCopy=data.frame(dataTot)
dataTotTrain=head(dataTotCopy,-12)
dataTotTest=tail(dataTotCopy,n=12)


#Graficar las series

op <- par(mfrow = c(3,1), mex=0.9)
plot(dataTot$kwh,type="l", xlab="Temps",ylab="[KWh]",
     main="Consommation d'électricité",cex.main=1,col="red")
grid()

plot(dataTot$htdd,type="l", xlab="Temps",ylab="htdd",
     main="Heating Degree Days",cex.main=1,col="blue")
grid()

plot(dataTot$cldd,type="l", xlab="Temps",ylab="cldd",
     main="Cooling Degree Days",cex.main=1,col="green")
grid()

par(op)
}
#________________________________________________________________

#2. Régressions linéaires multiples sans l'aspect temporel
{
#................................................
#2.1. Regresión múltiple sencilla
{
reg21=lm(dataTotTrain$kwh~dataTotTrain$htdd+dataTotTrain$cldd)
summary(reg21)

#Sacar los coeficientes de la regresión, estimar los valores del 2019 y encontrar el RMSE

beta_reg21=reg21$coefficients
predict.reg21=beta_reg21[1]+beta_reg21[2]*dataTotTest$htdd+beta_reg21[3]*dataTotTest$cldd
RMSE.reg21=sqrt(mean((dataTotTest$kwh-predict.reg21)^2))
print(RMSE.reg21)


#Graficar datos reales vs el modelo
plot(dataTot$kwh,type="l", xlab="Temps",ylab="[KWh]",
     main="Consommation d'électricité",cex.main=1)
lines(y=reg21$fitted.values,x=dataTotTrain$date, col="red")
lines(y=predict.reg21,x=dataTotTest$date, col="blue")
grid()
}


#................................................
#2.2. Regresión múltiple con interacción
{
reg22=lm(dataTotTrain$kwh~dataTotTrain$htdd+dataTotTrain$cldd+dataTotTrain$cldd*dataTotTrain$htdd)
summary(reg22)

#Sacar los coeficientes de la regresión, estimar los valores del 2019 y encontrar el RMSE

beta_reg22=reg22$coefficients
predict.reg22=beta_reg22[1]+beta_reg22[2]*dataTotTest$htdd+beta_reg22[3]*dataTotTest$cldd+beta_reg22[4]*dataTotTest$cldd*dataTotTest$htdd
RMSE.reg22=sqrt(mean((dataTotTest$kwh-predict.reg22)^2))
print(RMSE.reg22)


#Graficar datos reales vs el modelo
plot(dataTot$kwh,type="l", xlab="Temps",ylab="[KWh]",
     main="Consommation d'électricité",cex.main=1)
lines(y=reg22$fitted.values,x=dataTotTrain$date, col="red")
lines(y=predict.reg22,x=dataTotTest$date, col="blue")
grid()
}

#................................................
#2.3. Regresión múltiple con interacción y términos cuadráticos
{
#creación de términos cuadráticos

htddCarrée=dataTotTrain$htdd*dataTotTrain$htdd
clddCarrée=dataTotTrain$cldd*dataTotTrain$cldd

reg23=lm(dataTotTrain$kwh~dataTotTrain$htdd+dataTotTrain$cldd+dataTotTrain$cldd*dataTotTrain$htdd+htddCarrée+clddCarrée)
summary(reg23)

#Sacar los coeficientes de la regresión, estimar los valores del 2019 y encontrar el RMSE

beta_reg23=reg23$coefficients
predict.reg23=beta_reg23[1]+beta_reg23[2]*dataTotTest$htdd+
  beta_reg23[3]*dataTotTest$cldd+beta_reg23[4]*dataTotTest$cldd*dataTotTest$htdd+
  beta_reg23[5]*dataTotTest$htdd*dataTotTest$htdd+
  beta_reg23[6]*dataTotTest$cldd*dataTotTest$cldd
RMSE.reg23=sqrt(mean((dataTotTest$kwh-predict.reg23)^2))
print(RMSE.reg23)


#Graficar datos reales vs el modelo
plot(dataTot$kwh,type="l", xlab="Temps",ylab="[KWh]",
     main="Consommation d'électricité",cex.main=1)
lines(y=reg23$fitted.values,x=dataTotTrain$date, col="red")
lines(y=predict.reg23,x=dataTotTest$date, col="blue")
grid()
}
#................................................
#2.4. Escogiendo la mejor regresión (en este caso particular, la regresión múltiple sencilla)
{
reg24=lm(dataTot$kwh~dataTot$htdd+dataTot$cldd)
summary(reg24)

#Graficar datos reales vs el modelo
plot(dataTot$kwh,type="l", xlab="Temps",ylab="[KWh]",
     main="Consommation d'électricité",cex.main=1)
lines(y=reg24$fitted.values,x=c(dataTot$date), col="red")
grid()
}

}

#___________________________________________________________________

#3. Analyse des résidus bruts
{
  par(mfrow=c(2,2))
  
  #3.1. Independencia de los residuales
  plot(dataTot$htdd,reg24$residuals,main="Residuals vs htdd")
  grid()
  
  plot(dataTot$cldd,reg24$residuals,main="Residuals vs cldd")
  grid()
  
  #3.2. Normalidad con pp-plot
  car::qqPlot(reg24$residuals,main="QQ Plot")
  grid()
  
  #3.3. Histograma
  hist(reg24$residuals, col = 'yellow')
}

#___________________________________________________________________

#4. Améloriations avec la dynamique temporelle
{
tempsCarrée=dataTot$date*dataTot$date
reg41=lm(dataTot$kwh~dataTot$htdd+dataTot$cldd+dataTot$date+tempsCarrée)
summary(reg41)

#Graficar datos reales vs el modelo
plot(dataTot$kwh,type="l", xlab="Temps",ylab="[KWh]",
     main="Consommation d'électricité",cex.main=1)
lines(y=reg41$fitted.values,x=c(dataTot$date), col="red")
grid()

#Predecir los datos del 2020
beta_reg41=reg41$coefficients
predict.reg41=beta_reg41[1]+beta_reg41[2]*dataTest$htdd+
  beta_reg41[3]*dataTest$cldd+beta_reg41[4]*dataTest$date+beta_reg41[5]*dataTest$date*dataTest$date


# car::qqPlot(reg24$residuals,main="QQ Plot")
# grid()
# hist(reg41$residuals, col = 'yellow')


write.table(predict.reg41,file="SIERRA.txt",sep="")
}


