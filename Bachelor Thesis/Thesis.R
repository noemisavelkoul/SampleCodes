install.packages("readr") 
install.packages("ggplot2")
install.packages("tseries")
install.packages("Synth")
install.packages("tidyverse")
install.packages("StructTS")
install.packages("nnet")
install.packages("mgcv")
install.packages("quantreg")
install.packages("systemfit")
install.packages("imputeTS")
install.packages("naniar")
install.packages("strucchange")

library(readr)
library(ggplot2)
library(tseries)
library(Synth)
library(tidyverse)
library(StructTS)
library(nnet)
library(mgcv)
library(quantreg)
library(systemfit) 
library(imputeTS)
library(naniar)
library(strucchange)


#-------------------------------------------------------EXTENSION HIV INCIDENCE-------------------------------------------------------#

Data <- data.frame((read_csv("INF-104-1.csv")))
Data[is.na(Data)] = -999
percentageMissing <- Data[1,2:(ncol(Data))]
for (i in 1:(ncol(Data)-1)){
  percentageMissing[1,i] <- 1 - mean(ifelse(Data[,i+1] != -999, 1, 0))
}
percentageMissing <- sort(percentageMissing)
percentageMissing

#----------------------------
# Interpolation
dataInterpolate <- data.frame((read_csv("INF-104-1.csv")))
for (i in 2:ncol(Data)) {
  if ((1 - percentageMissing[1,(i-1)]) >= 2/(nrow(dataInterpolate))) {
    dataInterpolate[,i] <- na_interpolation(dataInterpolate[,i], option = "spline")
  } 
}
#---------------------------- 
#Contrasts
interventionRow <- which(dataInterpolate[,1] == 2001)
Portugal <- as.matrix(dataInterpolate[, 'Portugal'])
dataInterpolate <- dataInterpolate[,colnames(Data)!="Portugal"]
Contrasts <- dataInterpolate[1:(interventionRow-1),2:ncol(dataInterpolate)]
for (i in 1:ncol(Contrasts)) {
  Contrasts[,i] <- Contrasts[,i]-Portugal[1:(interventionRow-1),1]
}
# ---------------------------- 
#Variance 
Variances <- dataInterpolate[1, 2:ncol(dataInterpolate)]
for (i in 1:(ncol(dataInterpolate)-1)) {
  Variances[,i] <- var(Contrasts[,i])
}
Variances 
#---------------------------- 
#KPSS tests for cointegration(level)
testStatisticVector <- dataInterpolate[1,2:ncol(dataInterpolate)]
for(country in 2:ncol(dataInterpolate)) {
  x = ts(Contrasts[1:(interventionRow-1),(country-1)], frequency = 1)
  #KPSS tests 
  testStatisticVector[1,(country-1)] <- kpss.test(x, null = "Level")$statistic
}
testStatisticVector <- sort(testStatisticVector)
testStatisticVector
#----------------------------  
#KPSS(2) test for I(0)
dataInterpolate <- data.frame((read_csv("INF-104-1.csv")))
for (i in 2:ncol(Data)) {
  if ((1 - percentageMissing[1,(i-1)]) >= 2/(nrow(dataInterpolate))) {
    dataInterpolate[,i] <- na_interpolation(dataInterpolate[,i], option = "spline")
  } 
}
x <- dataInterpolate[1:(interventionRow-1),2:ncol(dataInterpolate)]
testStatisticVector <- as.matrix(dataInterpolate[1,2:ncol(dataInterpolate)])
for(country in 2:ncol(dataInterpolate)){
  print(kpss.test(x[,(country-1)], null = "Trend")$statistic)
  testStatisticVector[1,(country-1)] <- kpss.test(x[,(country-1)], null = "Trend")$statistic
}
round(testStatisticVector, digits = 3)
#----------------------------  
# KPSS(2) test for I(1)
dataInterpolate <- data.frame((read_csv("INF-104-1.csv")))
for (i in 2:ncol(Data)) {
  if ((1 - percentageMissing[1,(i-1)]) >= 2/(nrow(dataInterpolate))) {
    dataInterpolate[,i] <- na_interpolation(dataInterpolate[,i], option = "spline")
  } 
}
x <- dataInterpolate[1:(interventionRow-1),2:ncol(dataInterpolate)]
testStatisticVector <- as.matrix(dataInterpolate[1,2:ncol(dataInterpolate)])
for (country in 2:ncol(dataInterpolate)){
  print(kpss.test(x%>%pull(country-1)%>%diff, null = "Level"))
  testStatisticVector[1,(country-1)] <- kpss.test(x%>%pull(country-1)%>%diff, null = "Level")$statistic
}
round(testStatisticVector, digits = 3)
#----------------------------  
#Weight RLS (Netherlands, Denmark, and Czech Republic)
Portugal <- as.matrix(Data[1:(interventionRow-1), 'Portugal'])
regressand <- as.matrix(Portugal[1:(interventionRow-1),1]-dataInterpolate[1:(interventionRow-1),'Czech.Republic'])
regressors <- matrix(2,(interventionRow-1),2)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Netherlands']-dataInterpolate[1:(interventionRow-1),'Czech.Republic'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Denmark']-dataInterpolate[1:(interventionRow-1), 'Czech.Republic'])
weightsNDC <- lm(formula = regressand~-1+regressors)
summary(weightsNDC)
weightsNDC <-weightsNDC$coefficients[1:2]
round(weightsNDC, digits=3)
weightControlNDC <- 1-sum(weightsNDC)
round(weightControlNDC, digits=3)
#----------------------------  
#Weight RLS (Netherlands, Denmark, and United Kingdom)
Portugal <- as.matrix(Data[1:(interventionRow-1), 'Portugal'])
regressand <- as.matrix(Portugal[1:(interventionRow-1),1]-dataInterpolate[1:(interventionRow-1),'United.Kingdom'])
regressors <- matrix(2,(interventionRow-1),2)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Netherlands']-dataInterpolate[1:(interventionRow-1),'United.Kingdom'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Denmark']-dataInterpolate[1:(interventionRow-1), 'United.Kingdom'])
weightsNDU <- lm(formula = regressand~-1+regressors)
summary(weightsNDU)
weightsNDU <-weightsNDU$coefficients[1:2]
round(weightsNDU, digits=3)
weightControlNDU <- 1-sum(weightsNDU)
round(weightControlNDU, digits=3)
#----------------------------  
#Weight RLS (Netherlands, Denmark, and Slovenia )
Portugal <- as.matrix(Data[1:(interventionRow-1), 'Portugal'])
regressand <- as.matrix(Portugal[1:(interventionRow-1),1]-dataInterpolate[1:(interventionRow-1),'Slovenia'])
regressors <- matrix(2,(interventionRow-1),2)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Netherlands']-dataInterpolate[1:(interventionRow-1),'Slovenia'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Denmark']-dataInterpolate[1:(interventionRow-1), 'Slovenia'])
weightsNDS <- lm(formula = regressand~-1+regressors)
summary(weightsNDS)
weightsNDS <-weightsNDS$coefficients[1:2]
round(weightsNDS, digits=3)
weightControlNDS <- 1-sum(weightsNDS)
round(weightControlNDS, digits=3)
#------------------------------
#Weight OLS (Netherlands, Denmark, and Czech Republic)
regressand <- as.matrix(Portugal[1:(interventionRow-1),1])
regressors <- matrix(2,(interventionRow-1),3)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Netherlands'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Denmark'])
regressors[,3] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Czech.Republic'])
weights_OLS_NDC <- as.matrix(lm(formula = regressand~regressors)$coefficients[2:4])
round(weights_OLS_NDC, digits=3)
#------------------------------
#Weight OLS (Netherlands, Denmark, and United Kingdom)
regressand <- as.matrix(Portugal[1:(interventionRow-1),1])
regressors <- matrix(2,(interventionRow-1),3)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Netherlands'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Denmark'])
regressors[,3] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'United.Kingdom'])
weights_OLS_NDU <- as.matrix(lm(formula = regressand~regressors)$coefficients[2:4])
round(weights_OLS_NDU, digits=3)
#------------------------------
#Weight OLS (Netherlands, Denmark, and Slovenia)
regressand <- as.matrix(Portugal[1:(interventionRow-1),1])
regressors <- matrix(2,(interventionRow-1),3)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Netherlands'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Denmark'])
regressors[,3] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Slovenia'])
weights_OLS_NDS <- as.matrix(lm(formula = regressand~regressors)$coefficients[2:4])
round(weights_OLS_NDS, digits=3)
#------------------------------
#Intervention effects assuming 6 pulse dummies and Synthetic Control Model
syntheticControl <- as.matrix(weights[1]*dataInterpolate[,'Netherlands'] +
                                weights[2]*dataInterpolate[, 'Denmark'] + 
                                weightControl*dataInterpolate[, 'Czech.Republic'])
syntheticControl
#----------
#STORING FIRST RUN!!! Using Netherlands, Denmark, Czech Republic 
syntheticControlNDC <- as.matrix(weightsNDC[1]*dataInterpolate[,'Netherlands'] +
                                   weightsNDC[2]*dataInterpolate[, 'Denmark'] + 
                                   weightControlNDC*dataInterpolate[, 'Czech.Republic'])

Data$SyntheticNDC <- syntheticControlNDC
kpss.test((Data$Portugal-Data$SyntheticNDC)[1:(interventionRow-1),], null = "Level")
#----------
#STORING SECOND RUN!!! Using Netherlands, Denmark, United Kingdom 
syntheticControlNDU <- as.matrix(weightsNDU[1]*dataInterpolate[,'Netherlands'] +
                                   weightsNDU[2]*dataInterpolate[, 'Denmark'] + 
                                   weightControlNDU*dataInterpolate[, 'United.Kingdom'])
Data$SyntheticNDU <- syntheticControlNDU 
kpss.test((Data$Portugal-Data$SyntheticNDU)[1:(interventionRow-1),], null = "Level")
#----------
#STORING THIRD RUN!!! Using Netherlands, Denmark, Slovenia
syntheticControlNDS <- as.matrix(weightsNDS[1]*dataInterpolate[,'Netherlands'] +
                                   weightsNDS[2]*dataInterpolate[, 'Denmark'] + 
                                   weightControlNDS*dataInterpolate[, 'Slovenia'])

Data$SyntheticNDS <- syntheticControlNDS
kpss.test((Data$Portugal-Data$SyntheticNDS)[1:(interventionRow-1),], null = "Level")
#------------------------------
# Synthetic Control Model 

#----------
# Netherlands, Denmark, and Czech Republic 
plot(syntheticControlNDC, type ='l', x = dataInterpolate[,'Year'])
plot(Data$Portugal, type = 'l', x=dataInterpolate[,'Year'])
sctest(syntheticControlNDC ~ as.matrix(dataInterpolate[1:nrow(dataInterpolate),1]), type = "Chow", point = which(dataInterpolate[,1] == 2004))
Data$SyntheticNDC <- syntheticControlNDC
x <- kpss.test((Data$Portugal-Data$SyntheticNDC)[1:(interventionRow-1),], null = "Level")
x
regressand <- as.matrix(Data[,'Portugal']-syntheticControlNDC)
DataDLO <- dataInterpolate
DataDLO$'Step Dummy' <- ifelse(DataDLO[,1] >= 2004,1,0)
DataDLO$'Pulse 2001' <- ifelse(DataDLO[,1] == 2001,1,0)
DataDLO$'Pulse 2002' <- ifelse(DataDLO[,1] == 2002,1,0)
DataDLO$'Pulse 2003' <- ifelse(DataDLO[,1] == 2003,1,0)
#DataDLO$'Pulse 2004' <- ifelse(DataDLO[,1] == 2004,1,0)
#DataDLO$'Pulse 2005' <- ifelse(DataDLO[,1] == 2005,1,0)
#DataDLO$'Pulse 2006' <- ifelse(DataDLO[,1] == 2006,1,0)
#DataDLO$'Pulse 2007' <- ifelse(DataDLO[,1] == 2007,1,0)
#DataDLO$'Pulse 2008' <- ifelse(DataDLO[,1] == 2008,1,0)

syntheticControlModelNDC <- lm(formula = regressand~DataDLO$'Step Dummy'+DataDLO$'Pulse 2001'+DataDLO$'Pulse 2002'+
                                 DataDLO$'Pulse 2003'#+ DataDLO$'Pulse 2004'+DataDLO$'Pulse 2005'+DataDLO$'Pulse 2006'+
                               #DataDLO$'Pulse 2007'+DataDLO$'Pulse 2008'
)
summary(syntheticControlModelNDC)
round(syntheticControlModel1$coefficients, digits=3)
durbinWatsonTest(syntheticControlModelNDC)
#----------
# Netherlands, Denmark, and United Kingdom 
plot(syntheticControlNDU, type ='l', x = dataInterpolate[,'Year'])
plot(Data$Portugal, type = 'l', x=dataInterpolate[,'Year'])
sctest(syntheticControlNDU ~ as.matrix(dataInterpolate[1:nrow(dataInterpolate),1]), type = "Chow", point = which(dataInterpolate[,1] == 2004))
Data$SyntheticNDU <- syntheticControlNDU
x <- kpss.test((Data$Portugal-Data$SyntheticNDU)[1:(interventionRow-1),], null = "Level")
x
regressand <- as.matrix(Data[,'Portugal']-syntheticControlNDU)
DataDLO <- dataInterpolate
DataDLO$'Step Dummy' <- ifelse(DataDLO[,1] >= 2004,1,0)
DataDLO$'Pulse 2001' <- ifelse(DataDLO[,1] == 2001,1,0)
DataDLO$'Pulse 2002' <- ifelse(DataDLO[,1] == 2002,1,0)
DataDLO$'Pulse 2003' <- ifelse(DataDLO[,1] == 2003,1,0)
#DataDLO$'Pulse 2004' <- ifelse(DataDLO[,1] == 2004,1,0)
#DataDLO$'Pulse 2005' <- ifelse(DataDLO[,1] == 2005,1,0)
#DataDLO$'Pulse 2006' <- ifelse(DataDLO[,1] == 2006,1,0)
#DataDLO$'Pulse 2007' <- ifelse(DataDLO[,1] == 2007,1,0)
#DataDLO$'Pulse 2008' <- ifelse(DataDLO[,1] == 2008,1,0)

syntheticControlModelNDU <- lm(formula = regressand~DataDLO$'Step Dummy'+DataDLO$'Pulse 2001'+DataDLO$'Pulse 2002'+
                                 DataDLO$'Pulse 2003'#+ DataDLO$'Pulse 2004'+DataDLO$'Pulse 2005'+DataDLO$'Pulse 2006'+
                               #DataDLO$'Pulse 2007'+DataDLO$'Pulse 2008'
)
summary(syntheticControlModelNDU)
round(syntheticControlModelNDU$coefficients, digits=3)
durbinWatsonTest(syntheticControlModelNDU)
#----------
# Netherlands, Denmark, and Slovenia
plot(syntheticControlNDS, type ='l', x = dataInterpolate[,'Year'])
plot(Data$Portugal, type = 'l', x=dataInterpolate[,'Year'])
sctest(syntheticControlNDS ~ as.matrix(dataInterpolate[1:nrow(dataInterpolate),1]), type = "Chow", point = which(dataInterpolate[,1] == 2004))
Data$SyntheticNDS <- syntheticControlNDS
x <- kpss.test((Data$Portugal-Data$SyntheticNDS)[1:(interventionRow-1),], null = "Level")
x
regressand <- as.matrix(Data[,'Portugal']-syntheticControlNDS)
DataDLO <- dataInterpolate
DataDLO$'Step Dummy' <- ifelse(DataDLO[,1] >= 2004,1,0)
DataDLO$'Pulse 2001' <- ifelse(DataDLO[,1] == 2001,1,0)
DataDLO$'Pulse 2002' <- ifelse(DataDLO[,1] == 2002,1,0)
DataDLO$'Pulse 2003' <- ifelse(DataDLO[,1] == 2003,1,0)
#DataDLO$'Pulse 2004' <- ifelse(DataDLO[,1] == 2004,1,0)
#DataDLO$'Pulse 2005' <- ifelse(DataDLO[,1] == 2005,1,0)
#DataDLO$'Pulse 2006' <- ifelse(DataDLO[,1] == 2006,1,0)
#DataDLO$'Pulse 2007' <- ifelse(DataDLO[,1] == 2007,1,0)
#DataDLO$'Pulse 2008' <- ifelse(DataDLO[,1] == 2008,1,0)

syntheticControlModelNDS <- lm(formula = regressand~DataDLO$'Step Dummy'+DataDLO$'Pulse 2001'+DataDLO$'Pulse 2002'+
                                 DataDLO$'Pulse 2003'#+ DataDLO$'Pulse 2004'+DataDLO$'Pulse 2005'+DataDLO$'Pulse 2006'+
                               #DataDLO$'Pulse 2007'+DataDLO$'Pulse 2008'
)
summary(syntheticControlModelNDS)
round(syntheticControlModelNDS$coefficients, digits=3)
durbinWatsonTest(syntheticControlModelNDS)

#--------------------------------
#Intervention effect assuming 6 pulse dummies and Univariate Model 

#----------
# Netherlands, Denmark, and Czech Republic 
regressand <- as.matrix(Data['Portugal']-dataInterpolate['Netherlands'])
regressors <- matrix(1,nrow(dataInterpolate),2)
regressors[,1] <- dataInterpolate[,'Czech.Republic']- dataInterpolate[,'Netherlands']
regressors[,2] <- dataInterpolate[,'Denmark']- dataInterpolate[,'Netherlands']
DataDLO <- dataInterpolate
DataDLO$'Step Dummy' <- ifelse(DataDLO[,1] >= 2004,1,0)
DataDLO$'Pulse 2001' <- ifelse(DataDLO[,1] == 2001,1,0)
DataDLO$'Pulse 2002' <- ifelse(DataDLO[,1] == 2002,1,0)
DataDLO$'Pulse 2003' <- ifelse(DataDLO[,1] == 2003,1,0)
#DataDLO$'Pulse 2004' <- ifelse(DataDLO[,1] == 2004,1,0)
#DataDLO$'Pulse 2005' <- ifelse(DataDLO[,1] == 2005,1,0)
#DataDLO$'Pulse 2006' <- ifelse(DataDLO[,1] == 2006,1,0)
#DataDLO$'Pulse 2007' <- ifelse(DataDLO[,1] == 2007,1,0)
#DataDLO$'Pulse 2008' <- ifelse(DataDLO[,1] == 2008,1,0)
univariateModelNDC <- lm(formula = regressand~+DataDLO$'Step Dummy'+DataDLO$'Pulse 2001'+DataDLO$'Pulse 2002'+
                           DataDLO$'Pulse 2003'#+DataDLO$'Pulse 2004'#+DataDLO$'Pulse 2005'+DataDLO$'Pulse 2006'+
                         #DataDLO$'Pulse 2007'+DataDLO$'Pulse 2008'
                         +regressors)
round(univariateModelNDC$coefficients, digits=3)
summary(univariateModelNDC)
durbinWatsonTest(univariateModelNDC)
#----------
# Netherlands, Denmark, and United Kingdom  
regressand <- as.matrix(Data['Portugal']-dataInterpolate['Netherlands'])
regressors <- matrix(1,nrow(dataInterpolate),2)
regressors[,1] <- dataInterpolate[,'United.Kingdom']- dataInterpolate[,'Netherlands']
regressors[,2] <- dataInterpolate[,'Denmark']- dataInterpolate[,'Netherlands']
DataDLO <- dataInterpolate
DataDLO$'Step Dummy' <- ifelse(DataDLO[,1] >= 2004,1,0)
DataDLO$'Pulse 2001' <- ifelse(DataDLO[,1] == 2001,1,0)
DataDLO$'Pulse 2002' <- ifelse(DataDLO[,1] == 2002,1,0)
DataDLO$'Pulse 2003' <- ifelse(DataDLO[,1] == 2003,1,0)
#DataDLO$'Pulse 2004' <- ifelse(DataDLO[,1] == 2004,1,0)
#DataDLO$'Pulse 2005' <- ifelse(DataDLO[,1] == 2005,1,0)
#DataDLO$'Pulse 2006' <- ifelse(DataDLO[,1] == 2006,1,0)
#DataDLO$'Pulse 2007' <- ifelse(DataDLO[,1] == 2007,1,0)
#DataDLO$'Pulse 2008' <- ifelse(DataDLO[,1] == 2008,1,0)
univariateModelNDU <- lm(formula = regressand~+DataDLO$'Step Dummy'+DataDLO$'Pulse 2001'+DataDLO$'Pulse 2002'+
                           DataDLO$'Pulse 2003'#+DataDLO$'Pulse 2004'#+DataDLO$'Pulse 2005'+DataDLO$'Pulse 2006'+
                         #DataDLO$'Pulse 2007'+DataDLO$'Pulse 2008'
                         +regressors)
round(univariateModelNDU$coefficients, digits=3)
summary(univariateModelNDU)
durbinWatsonTest(univariateModelNDU)
#----------
# Netherlands, Denmark, and Slovenia  
regressand <- as.matrix(Data['Portugal']-dataInterpolate['Netherlands'])
regressors <- matrix(1,nrow(dataInterpolate),2)
regressors[,1] <- dataInterpolate[,'Slovenia']- dataInterpolate[,'Netherlands']
regressors[,2] <- dataInterpolate[,'Denmark']- dataInterpolate[,'Netherlands']
DataDLO <- dataInterpolate
DataDLO$'Step Dummy' <- ifelse(DataDLO[,1] >= 2004,1,0)
DataDLO$'Pulse 2001' <- ifelse(DataDLO[,1] == 2001,1,0)
DataDLO$'Pulse 2002' <- ifelse(DataDLO[,1] == 2002,1,0)
DataDLO$'Pulse 2003' <- ifelse(DataDLO[,1] == 2003,1,0)
#DataDLO$'Pulse 2004' <- ifelse(DataDLO[,1] == 2004,1,0)
#DataDLO$'Pulse 2005' <- ifelse(DataDLO[,1] == 2005,1,0)
#DataDLO$'Pulse 2006' <- ifelse(DataDLO[,1] == 2006,1,0)
#DataDLO$'Pulse 2007' <- ifelse(DataDLO[,1] == 2007,1,0)
#DataDLO$'Pulse 2008' <- ifelse(DataDLO[,1] == 2008,1,0)
univariateModelNDS <- lm(formula = regressand~+DataDLO$'Step Dummy'+DataDLO$'Pulse 2001'+DataDLO$'Pulse 2002'+
                           DataDLO$'Pulse 2003'#+DataDLO$'Pulse 2004'#+DataDLO$'Pulse 2005'+DataDLO$'Pulse 2006'+
                         #DataDLO$'Pulse 2007'+DataDLO$'Pulse 2008'
                         +regressors)
round(univariateModelNDS$coefficients, digits=3)
summary(univariateModelNDS)
durbinWatsonTest(univariateModelNDS)
#-------------------------------------------------------PLOTS EXTENSION HIV INCIDENCE-------------------------------------------------------#
DataDLO <- data.frame((read_csv("INF-104-1.csv")))
Data$below50_0 <- ifelse(DataDLO[,1] >= 1993,1,0)
Data$Synthetic1 <- syntheticControlNDC
Data$Synthetic2 <- syntheticControlNDU
Data$Synthetic3 <- syntheticControlNDS
fortify(data.frame(Data$Portugal)) 
ggplot(Data, aes(Data$Year, Data$Portugal, color = as.factor(Data$below50_0)))+
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_vline(xintercept=2001, linetype="longdash", alpha = 0.75)+
  geom_line(data = fortify(data.frame(Data$Portugal)) , aes(x = as.numeric(Data$Year, color = '#000000'), y = Data$Portugal))+
  geom_line(data = fortify(data.frame(Data$Synthetic1)) , aes(x = as.numeric(Data$Year), y = Data$Synthetic1, color = '#800020'))+
  geom_line(data = fortify(data.frame(Data$Synthetic2)) , aes(x = as.numeric(Data$Year), y = Data$Synthetic2, color = '#2496cd'))+
  geom_line(data = fortify(data.frame(Data$Synthetic3)) , aes(x = as.numeric(Data$Year), y = Data$Synthetic3, color = '#Cedc00'))+
  xlab("Year") +
  ylab("HIV Incidence" ) +
  scale_color_manual(values=c('#Cedc00','#2496cd' ,'#800020' , '#000000'), labels=c("SC-NDU", "SC-NDC", "SC-NDS" , "Portugal"),name=(""))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

#-------------------------------------------------------EXTENSION AIDS INCIDENCE-------------------------------------------------------#
Data <- data.frame((read_csv("INF-104-2.csv")))
Data[is.na(Data)] = -999
percentageMissing <- Data[1,2:(ncol(Data))]
for (i in 1:(ncol(Data)-1)){
  percentageMissing[1,i] <- 1 - mean(ifelse(Data[,i+1] != -999, 1, 0))
}
percentageMissing <- sort(percentageMissing)
percentageMissing

#----------------------------
# Interpolation
dataInterpolate <- data.frame((read_csv("INF-104-2.csv")))
for (i in 2:ncol(Data)) {
  if ((1 - percentageMissing[1,(i-1)]) >= 2/(nrow(dataInterpolate))) {
    dataInterpolate[,i] <- na_interpolation(dataInterpolate[,i], option = "spline")
  } 
}
#---------------------------- 
#Contrasts
interventionRow <- which(dataInterpolate[,1] == 2001)
Portugal <- as.matrix(dataInterpolate[, 'Portugal'])
dataInterpolate <- dataInterpolate[,colnames(Data)!="Portugal"]
Contrasts <- dataInterpolate[1:(interventionRow-1),2:ncol(dataInterpolate)]
for (i in 1:ncol(Contrasts)) {
  Contrasts[,i] <- Contrasts[,i]-Portugal[1:(interventionRow-1),1]
}
# ---------------------------- 
#Variance 
Variances <- dataInterpolate[1, 2:ncol(dataInterpolate)]
for (i in 1:(ncol(dataInterpolate)-1)) {
  Variances[,i] <- var(Contrasts[,i])
}
Variances 
#---------------------------- 
#KPSS tests for cointegration(level)
testStatisticVector <- dataInterpolate[1,2:ncol(dataInterpolate)]
for(country in 2:ncol(dataInterpolate)) {
  x = ts(Contrasts[1:(interventionRow-1),(country-1)], frequency = 1)
  #KPSS tests 
  testStatisticVector[1,(country-1)] <- kpss.test(x, null = "Level")$statistic
}
testStatisticVector <- sort(testStatisticVector)
round(testStatisticVector, digits = 3)
#----------------------------  
#KPSS(2) test for I(0)
dataInterpolate <- data.frame((read_csv("INF-104-2.csv")))
for (i in 2:ncol(Data)) {
  if ((1 - percentageMissing[1,(i-1)]) >= 2/(nrow(dataInterpolate))) {
    dataInterpolate[,i] <- na_interpolation(dataInterpolate[,i], option = "spline")
  } 
}
x <- dataInterpolate[1:(interventionRow-1),2:ncol(dataInterpolate)]
testStatisticVector <- as.matrix(dataInterpolate[1,2:ncol(dataInterpolate)])
for(country in 2:ncol(dataInterpolate)){
  print(kpss.test(x[,(country-1)], null = "Trend")$statistic)
  testStatisticVector[1,(country-1)] <- kpss.test(x[,(country-1)], null = "Trend")$statistic
}
round(testStatisticVector, digits = 3)
#----------------------------  
# KPSS(2) test for I(1)
dataInterpolate <- data.frame((read_csv("INF-104-2.csv")))
for (i in 2:ncol(Data)) {
  if ((1 - percentageMissing[1,(i-1)]) >= 2/(nrow(dataInterpolate))) {
    dataInterpolate[,i] <- na_interpolation(dataInterpolate[,i], option = "spline")
  } 
}
x <- dataInterpolate[1:(interventionRow-1),2:ncol(dataInterpolate)]
testStatisticVector <- as.matrix(dataInterpolate[1,2:ncol(dataInterpolate)])
for (country in 2:ncol(dataInterpolate)){
  print(kpss.test(x%>%pull(country-1)%>%diff, null = "Level"))
  testStatisticVector[1,(country-1)] <- kpss.test(x%>%pull(country-1)%>%diff, null = "Level")$statistic
}
round(testStatisticVector, digits = 3)
#-----------------------------
#Weight RLS using Belgium, Poland, and Greece
Portugal <- as.matrix(Data[1:(interventionRow-1), 'Portugal'])
regressand <- as.matrix(Portugal[1:(interventionRow-1),1]-dataInterpolate[1:(interventionRow-1),'Greece'])
regressors <- matrix(2,(interventionRow-1),2)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Belgium']-dataInterpolate[1:(interventionRow-1),'Greece'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Poland']-dataInterpolate[1:(interventionRow-1), 'Greece'])
weightsBPG <- lm(formula = regressand~regressors)
summary(weightsBPG)
weightsBPG <- weightsBPG$coefficients[2:3]
round(weightsBPG, digits=3)
weightControlBPG <- 1-sum(weightsBPG)
round(weightControlBPG, digits=3)
#-----------------------------
#Weight RLS using Luxembourg, Poland, and Turkey
Portugal <- as.matrix(Data[1:(interventionRow-1), 'Portugal'])
regressand <- as.matrix(Portugal[1:(interventionRow-1),1]-dataInterpolate[1:(interventionRow-1),'Turkey'])
regressors <- matrix(2,(interventionRow-1),2)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Luxembourg']-dataInterpolate[1:(interventionRow-1),'Turkey'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Poland']-dataInterpolate[1:(interventionRow-1), 'Turkey'])
weightsLPT <- lm(formula = regressand~regressors)
summary(weightsLPT)
weightsLPT <- weightsLPT$coefficients[2:3]
round(weightsLPT, digits=3)
weightControlLPT <- 1-sum(weightsLPT)
round(weightControlLPT, digits=3)
#------------------------------
#Weight OLS Belgium, Poland, and Greece
regressand <- as.matrix(Portugal[1:(interventionRow-1),1])
regressors <- matrix(2,(interventionRow-1),3)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Belgium'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Poland'])
regressors[,3] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Greece'])
weights_OLS_BPG <- as.matrix(lm(formula = regressand~regressors)$coefficients[2:4])
round(weights_OLS_BPG, digits=3)
#------------------------------
#Weight OLS Luxembourg, Poland, and Turkey
regressand <- as.matrix(Portugal[1:(interventionRow-1),1])
regressors <- matrix(2,(interventionRow-1),3)
regressors[,1] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Luxembourg'])
regressors[,2] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Poland'])
regressors[,3] <- as.matrix(dataInterpolate[1:(interventionRow-1), 'Turkey'])
weights_OLS_LPT <- as.matrix(lm(formula = regressand~regressors)$coefficients[2:4])
round(weights_OLS_LPT, digits=3)
#-------------------------------
# Synthetic controls

syntheticControlBPG <- as.matrix(weightsBPG[1]*dataInterpolate[,'Belgium'] +
                                   weightsBPG[2]*dataInterpolate[, 'Poland'] + 
                                   weightControlBPG*dataInterpolate[, 'Greece'])

plot(syntheticControlBPG, type ='l', x = dataInterpolate[,'Year'])
plot(Data$Portugal, type = 'l', x=dataInterpolate[,'Year'])
sctest(syntheticControlBPG ~ as.matrix(dataInterpolate[1:nrow(dataInterpolate),1]), type = "Chow", point = which(dataInterpolate[,1] == 2004))
Data$SyntheticBPG <- syntheticControlBPG
x <- kpss.test((Data$Portugal-Data$SyntheticBPG)[1:(interventionRow-1),], null = "Level")
x

syntheticControlLPT <- as.matrix(weightsLPT[1]*dataInterpolate[,'Luxembourg'] +
                                   weightsLPT[2]*dataInterpolate[, 'Poland'] + 
                                   weightControlLPT*dataInterpolate[, 'Turkey'])

plot(syntheticControlLPT, type ='l', x = dataInterpolate[,'Year'])
plot(Data$Portugal, type = 'l', x=dataInterpolate[,'Year'])
sctest(syntheticControlLPT ~ as.matrix(dataInterpolate[1:nrow(dataInterpolate),1]), type = "Chow", point = which(dataInterpolate[,1] == 2004))
Data$SyntheticLPT <- syntheticControlLPT
x <- kpss.test((Data$Portugal-Data$SyntheticLPT)[1:(interventionRow-1),], null = "Level")
x

#---------------------------------
#Synthetic control model using Belgium Poland and Turkey 
regressand <- as.matrix(Data[,'Portugal']-syntheticControlLPT)
DataDLO <- dataInterpolate
DataDLO$'Step Dummy' <- ifelse(DataDLO[,1] >= 2005,1,0)
DataDLO$'Pulse 2001' <- ifelse(DataDLO[,1] == 2001,1,0)
DataDLO$'Pulse 2002' <- ifelse(DataDLO[,1] == 2002,1,0)
DataDLO$'Pulse 2003' <- ifelse(DataDLO[,1] == 2003,1,0)
DataDLO$'Pulse 2004' <- ifelse(DataDLO[,1] == 2004,1,0)
#DataDLO$'Pulse 2006' <- ifelse(DataDLO[,1] == 2006,1,0)
#DataDLO$'Pulse 2007' <- ifelse(DataDLO[,1] == 2007,1,0)
#DataDLO$'Pulse 2008' <- ifelse(DataDLO[,1] == 2008,1,0)

syntheticControlModelLPT <- lm(formula = regressand~DataDLO$'Step Dummy'+DataDLO$'Pulse 2001'+DataDLO$'Pulse 2002'+
                                 DataDLO$'Pulse 2003'+ DataDLO$'Pulse 2004'#+DataDLO$'Pulse 2005'#+DataDLO$'Pulse 2006'+
                               #DataDLO$'Pulse 2007'+DataDLO$'Pulse 2008'
)
summary(syntheticControlModelLPT)
round(syntheticControlModelLPT$coefficients, digits=3)
durbinWatsonTest(syntheticControlModelLPT)
#---------------------------------------
# Univariate model using luxembourg, Poland, and Turkey and 4 pulses 
regressand <- as.matrix(Data['Portugal']-dataInterpolate['Luxembourg'])
regressors <- matrix(1,nrow(dataInterpolate),2)
regressors[,1] <- dataInterpolate[,'Poland']- dataInterpolate[,'Luxembourg']
regressors[,2] <- dataInterpolate[,'Turkey']- dataInterpolate[,'Luxembourg']
DataDLO <- dataInterpolate
DataDLO$'Step Dummy' <- ifelse(DataDLO[,1] >= 2005,1,0)
DataDLO$'Pulse 2001' <- ifelse(DataDLO[,1] == 2001,1,0)
DataDLO$'Pulse 2002' <- ifelse(DataDLO[,1] == 2002,1,0)
DataDLO$'Pulse 2003' <- ifelse(DataDLO[,1] == 2003,1,0)
DataDLO$'Pulse 2004' <- ifelse(DataDLO[,1] == 2004,1,0)
#DataDLO$'Pulse 2005' <- ifelse(DataDLO[,1] == 2005,1,0)
#DataDLO$'Pulse 2006' <- ifelse(DataDLO[,1] == 2006,1,0)
#DataDLO$'Pulse 2007' <- ifelse(DataDLO[,1] == 2007,1,0)
#DataDLO$'Pulse 2008' <- ifelse(DataDLO[,1] == 2008,1,0)
univariateModelLPT <- lm(formula = regressand~DataDLO$'Step Dummy'+DataDLO$'Pulse 2001'+DataDLO$'Pulse 2002'+
                           DataDLO$'Pulse 2003'+DataDLO$'Pulse 2004' +regressors)

round(univariateModelLPT$coefficients, digits=3)
summary(univariateModelLPT)
durbinWatsonTest(univariateModelLPT)
#-------------------------------------------------------PLOTS EXTENSION AIDS INCIDENCE-------------------------------------------------------#

#-----------------------------------
#Synthetic Controls using Luxembourg, Poland and Turkey and Belgium Poland and Greece together with Portuguese series 

DataDLO <- data.frame((read_csv("INF-104-2.csv")))
DataDLO$below50_0 <- ifelse(Data[,1] >= 1995,1,0)

DataDLO$SyntheticBPG <- syntheticControlBPG
DataDLO$SyntheticLPT <- syntheticControlLPT
fortify(data.frame(Data$Portugal)) 
ggplot(Data, aes(DataDLO$Year, DataDLO$Portugal, color = as.factor(DataDLO$below50_0)))+
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_vline(xintercept=2001, linetype="longdash", alpha = 0.75)+
  geom_line(data = fortify(data.frame(DataDLO$Portugal)) , aes(x = as.numeric(DataDLO$Year, color = '#000000'), y = DataDLO$Portugal))+
  geom_line(data = fortify(data.frame(DataDLO$SyntheticBPG)) , aes(x = as.numeric(DataDLO$Year), y = DataDLO$SyntheticBPG, color = '#Cedc00'))+
  geom_line(data = fortify(data.frame(DataDLO$SyntheticLPT)) , aes(x = as.numeric(DataDLO$Year), y = DataDLO$SyntheticLPT, color = '#2496cd'))+
  xlab("Year") +
  ylab("AIDS Incidence" ) +
  scale_color_manual(values=c('#2496cd', '#Cedc00', '#000000'), labels=c("SC-LPT", "SC-BPG", "Portugal"),name=(""))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

#-----------------------------------
#Portuguese series next to chosen synthetic controls (Luxembourg, Poland, and Turkey)
Data <- data.frame((read_csv("INF-104-2.csv")))
Data[is.na(Data)] = -999
percentageMissing <- Data[1,2:(ncol(Data))]
for (i in 1:(ncol(Data)-1)){
  percentageMissing[1,i] <- 1 - mean(ifelse(Data[,i+1] != -999, 1, 0))
}
percentageMissing <- sort(percentageMissing)
percentageMissing
DataDLO <- data.frame((read_csv("INF-104-2.csv")))
dataInterpolate <- data.frame((read_csv("INF-104-2.csv")))
for (i in 2:ncol(Data)) {
  if ((1 - percentageMissing[1,(i-1)]) >= 2/(nrow(dataInterpolate))) {
    dataInterpolate[,i] <- na_interpolation(dataInterpolate[,i], option = "spline")
  } 
}
DataDLO <- dataInterpolate
DataDLO$below50_0 <- ifelse(Data[,1] >= 1995,1,0)
fortify(data.frame(Data$Portugal)) 
ggplot(Data, aes(DataDLO$Year, DataDLO$Portugal, color = as.factor(DataDLO$below50_0)))+
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_vline(xintercept=2001, linetype="longdash", alpha = 0.75)+
  geom_line(data = fortify(data.frame(DataDLO$Portugal)) , aes(x = as.numeric(DataDLO$Year, color = '#000000'), y = DataDLO$Portugal))+
  geom_line(data = fortify(data.frame(DataDLO$Luxembourg)) , aes(x = as.numeric(DataDLO$Year), y = DataDLO$Luxembourg, color = '#Cedc00'))+
  geom_line(data = fortify(data.frame(DataDLO$Poland)) , aes(x = as.numeric(DataDLO$Year), y = DataDLO$Poland, color = '#800020'))+
  geom_line(data = fortify(data.frame(DataDLO$Turkey)) , aes(x = as.numeric(DataDLO$Year), y = DataDLO$Turkey, color = '#2496cd'))+
  xlab("Year") +
  ylab("AIDS Incidence" ) +
  scale_color_manual(values=c('#800020','#2496cd', '#Cedc00', '#000000'), labels=c("Luxembourg", "Poland", "Turkey", "Portugal"),name=(""))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

#-------------------------------------------------------END-------------------------------------------------------#


