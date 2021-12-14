# -Setup-

dfFull <- read.csv("dataset/train.csv")
dfTest <- read.csv("dataset/test.csv")

# -Descripció dataset-
head(dfFull)
colnames(dfFull)

#Variables que mantenim - 'survived', 'pclass', 'name', 'sex', 'age', 'sibsp', 'parch', 'fare'
# La variable 'name', pot tenir algun tipus amb el títol nobiliari? 

# -Integració i seleccio dades-
relevantFields <- c("Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Fare")
df <- dfFull[relevantFields]

# -Neteja de dades-

#Check NA values
colSums(is.na(df))
colSums(df=="")

#All NA values of 'Age' var to mean age
mean(df$Age)
df$Age[is.na(df$Age)] <- mean(df$Age,na.rm=T)

colSums(is.na(df))
colSums(df=="")

#Analisi camp 'Age'
summary(df[,"Age"])
df["segment_edat"] <- cut(df$Age, breaks = c(0,10,20,30,40,50,60,70,100), labels = c("0-9", "10-19", "20-29", "30-39","40-49","50-59","60-69","70-79"))
plot(df$segment_edat)
age.bp <- boxplot(df$Age, xlab = "Age", horizontal=TRUE)
ageOutliers <- age.bp$out #boxplot outliers
ageOutliers

#Outliers verification

# Q1 - 1,5 · IQR - https://www.adictosaltrabajo.com/2019/11/28/deteccion-y-reemplazo-de-outliers-con-r/
firstQuartile <- summary(df[,"Age"])[2]
firstQuartile <- quantile(df[,"Age"],0.25)
firstQuartile
thirdQuartile <- summary(df[,"Age"])[5]
thirdQuartile
IQR <- thirdQuartile-firstQuartile
IQR

# Criterio +/-2SD - 
#https://machinelearningmastery.com/how-to-use-statistics-to-identify-outliers-in-data/
#https://www.diegocalvo.es/calculo-de-outliers-en-r-distancia-de-gauss-y-de-mahalanobis/
mean<-mean(df$Age)
sd<-sd(df$Age)
cutoff<- sd*2
upperBoundary <- mean+cutoff
lowerBoundary <- mean-cutoff

age.outlier <- abs(scale(df$Age)) > 2
outliers <- na.omit(rbind(df[age.outlier,],df$Age)) #http://r-statistics.co/Outlier-Treatment-With-R.html
dataSetWOutliers <- df[!(df$Age %in% outliers$Age),]
plot(dataSetWOutliers$Age, pch=0, xlab="Nº item", ylab="Age",ylim=c(0,90))
points(outliers$Age, pch=16, col="red")
