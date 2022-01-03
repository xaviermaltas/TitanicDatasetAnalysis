# -Setup-

df.full <- read.csv("dataset/train.csv")
df.test <- read.csv("dataset/test.csv")
df.realSurvived <- read.csv("dataset/gender_submission.csv")

# -Descripció dataset-
head(df.full)
colnames(df.full)

#Variables que mantenim - 'survived', 'pclass', 'sex', 'age', 'sibsp', 'parch', 'fare'
# La variable 'name', pot tenir algun tipus amb el títol nobiliari? Finalment no mantenim 'name'
#No es una variable que importi a l'hora de predir si un passatger sobreviu o no.

if(!require(ggplot2)){
  install.packages('ggplot2', repos='http://cran.us.r-project.org')
  library(ggplot2)
}

if(!require(grid)){
  install.packages('grid', repos='http://cran.us.r-project.org')
  library(grid)
}

if(!require(gridExtra)){
  install.packages('gridExtra', repos='http://cran.us.r-project.org')
  library(gridExtra)
}

# https://cran.r-project.org/web/packages/ggplot2/index.html
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
# https://cran.r-project.org/web/packages/dplyr/index.html
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')

if(!require('magrittr')) install.packages('magrittr'); library('magrittr')

grid.newpage()
plotbyClass<-ggplot(df.full,aes(Pclass))+geom_bar() +labs(x="Pclass", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Pclass")
plotbyClass

plotbyAge<-ggplot(df.full,aes(Age))+geom_histogram(bins=20, colour="black") +labs(x="Age", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Age")
#plotbyAge

plotbySex<-ggplot(df.full,aes(Sex))+geom_bar() +labs(x="Sex", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Sex")
#plotbySex

plotbySurvived<-ggplot(df.full,aes(Survived))+geom_bar() +labs(x="Survived", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Survived")
#plotbySurvived
#grid.arrange(plotbyClass,plotbyAge,plotbySex,plotbySurvived,ncol=2)

plotbySibSp<-ggplot(df.full,aes(SibSp))+geom_bar() + labs(x="SibSp", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("SibSp")
#plotbySibSp

plotbyParch<-ggplot(df.full,aes(Parch))+geom_bar() + labs(x="Parch", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Parch")
#plotbyParch

grid.arrange(plotbyClass,plotbyAge,plotbySex,plotbySurvived,plotbySibSp,plotbyParch,ncol=2)

###survived grid
grid.newpage()
survivedbyClass<-ggplot(df.full,aes(x=(as.factor(Pclass)), fill=(as.factor(Survived))))+geom_bar()+labs(x="Pclass", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Survived by Pclass")
#survivedbyClass

survivedbySex <- ggplot(df.full, aes(x=(as.factor(Sex)), fill=(as.factor(Survived))))+geom_bar()+labs(x="Gender", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Survived by gender")
#survivedbySex

survivedbySibSp <- ggplot(df.full, aes(x=(as.factor(SibSp)), fill=(as.factor(Survived))))+geom_bar()+labs(x="SibSp", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Survived by SibSp")
#survivedbySibSp

survivedbyParch <- ggplot(df.full, aes(x=(as.factor(Parch)), fill=(as.factor(Survived))))+geom_bar()+labs(x="Parch", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Survived by Parch")
#survivedbyParch

grid.arrange(survivedbyClass,survivedbySex,survivedbySibSp,survivedbyParch,ncol=2)

###survived age
survivedbyAge<-ggplot(df.full, aes(x=Age, fill=(as.factor(Survived))))+geom_histogram(bins=20, colour="black")+labs(x="Age", y="Passengers")+ guides(fill=guide_legend(title=""))+ggtitle("Survived by Age")
survivedbyAge

survivedbyAgeSexClass<-ggplot(df.full, aes(x=Age, fill=(as.factor(Survived))))+geom_histogram(bins=20, colour="black")+ facet_grid(Sex~Pclass, scales="free")+labs(x="Age", y="Passengers")+guides(fill=guide_legend(title=""))+ggtitle("Survived by age, sex and ticket class")
survivedbyAgeSexClass



# -Integració i seleccio dades-
relevantFields <- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")
df <- df.full[relevantFields]

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
#La seguent linia es per fer un gràfic on veu el  nombre de passatgers per grups d'edats i es detecta que la gran majoria estan entre els 20 i 29 anys??
#df["segment_edat"] <- cut(df$Age, breaks = c(0,10,20,30,40,50,60,70,100), labels = c("0-9", "10-19", "20-29", "30-39","40-49","50-59","60-69","70-79"))
#plot(df$segment_edat)

#Identificació dels outliers
#Les variables a analitzar del dataframe són 'Age' i 'Fare' ja que són les úniques pròpiament numèriques. 
#Graficar els gràfics de caixes per veure els outliers: 
summary(df[,"Age"])
age.bp <- boxplot(df$Age, xlab = "Age", horizontal=TRUE)
ageOutliersAge <- age.bp$out #boxplot outliers

#Outliers verification

#https://www.adictosaltrabajo.com/2019/11/28/deteccion-y-reemplazo-de-outliers-con-r/
#firstQuartileAge <- summary(df[,"Age"])[2] 
#thirdQuartileAge <- summary(df[,"Age"])[5]
quantile(df$Age)
iAge <- IQR(df$Age)
#Outliers per damunt del bigotis:
#Q3 + 1,5 x IQR 
out_upp_Age <- 35 + 1.5 * iAge
#Outliers per sota del bigotis: 
#Q1 - 1,5 x IQR
out_low_Age <- 22 - 1.5 * iAge

# Criterio +/-2SD - 
#https://machinelearningmastery.com/how-to-use-statistics-to-identify-outliers-in-data/
#https://www.diegocalvo.es/calculo-de-outliers-en-r-distancia-de-gauss-y-de-mahalanobis/
mean<-mean(df$Age)
sd<-sd(df$Age)
cutoff<- sd*2
upperBoundaryAge <- mean+cutoff
lowerBoundaryAge <- mean-cutoff
#Graficar els outliers amb gràfic de punts: 
age.outlier.Age <- abs(scale(df$Age)) > 2
outliers <- na.omit(rbind(df[age.outlier.Age,],df$Age)) #http://r-statistics.co/Outlier-Treatment-With-R.html
dataSetWOutliers <- df[!(df$Age %in% outliers$Age),]
plot(dataSetWOutliers$Age, pch=0, xlab="Nº item", ylab="Age",ylim=c(0,90))
points(outliers$Age, pch=16, col="red")

#Identificació outliers variable 'Fare':
#Analitzar estadístiques de 'Fare':
summary(df$Fare)
#Graficar els gràfics de caixes per veure els outliers: 
fare.bp <- boxplot(df$Fare, xlab = "Fare", horizontal=TRUE)
fareOutliers <- fare.bp$out #boxplot outliers

#Fare outliers:
#firstQuartileFare <- summary(df[,"Fare"])[2]
#thirdQuartileFare <- summary(df[,"Fare"])[5]

quantile(df$Fare)
iFare <- IQR(df$Fare)
#Outliers per damunt del bigotis:
#Q3 + 1,5 x IQR 
out_upp_Fare <- 31 + 1.5 * iFare

#Outliers per sota del bigotis: 
#Q1 - 1,5 x IQR
out_low_Fare <- 7.91 - 1.5 * iFare

# Criterio +/-2SD - verificació outliers 'Fare'
mean<-mean(df$Fare)
sd<-sd(df$Fare)
cutoff<- sd*2
upperBoundaryFare <- mean+cutoff
lowerBoundaryFare <- mean-cutoff

fare.outlier <- abs(scale(df$Fare)) > 2
outliers <- na.omit(rbind(df[fare.outlier,],df$Fare))
dataSetWOutliers <- df[!(df$Fare %in% outliers$Fare),]
plot(dataSetWOutliers$Fare, pch=0, xlab="Nº item", ylab="Fare", ylim = c(0,515))
points(outliers$Fare, pch=16, col="red")

#Els outliers 'Age' els eliminem del dataframe, ja que no son gaires i generen soroll i desviacions 
#quan fem anàlisis i fem models de predicció.

#Els de 'Fare'són bastant més que al treure'ls sí estariem traient molta informació del data,
#Decidim deixar-los en el dataframe per analitzar ja que son dades reals la tripulacio no pagava res
#En canvi els rics de 1ª classe pagaven moltes lliures, es una diferenciació REAL. 

#Eliminar outliers variable Age:
library('dplyr')
df.clean <- df %>% filter(Age > 2 & Age < 55)

# -Analisi de les dades-

#Sex to dummy variables
sexfactor = factor(df.clean$Sex)
dummies.sex = model.matrix(~sexfactor)
RL.df <- data.frame(df.clean, dummies.sex[,2])
RL.df$dummies.sex <- RL.df$dummies.sex...2.
RL.df$dummies.sex...2. <- NULL

#RL - Model de regressió logistica 
if (!require('caret')) install.packages('caret'); library('caret')

#Creating Sigmoide function
RL.model <- glm(Survived ~ Pclass + dummies.sex +Age + SibSp + Parch + Fare, data=RL.df, family=binomial) #https://stats.idre.ucla.edu/r/dae/logit-regression/
summary(RL.model)

#Clean Test DF
df.test <- select(df.test, Pclass, Sex, Age, SibSp, Parch, Fare)
colSums(is.na(df.test))
colSums(df.test=="")
df.test$Age[is.na(df.test$Age)] <- mean(df.test$Age,na.rm=T)
df.test$Fare[is.na(df.test$Fare)] <- 0 #If this value is a NA, we set it to 0. We can not know which was its price.

#RL df.test
RL.df.test <- df.test
sexfactor.test = factor(RL.df.test$Sex)
dummies.sex.test = model.matrix(~sexfactor.test)
RL.df.test <- data.frame(RL.df.test, dummies.sex.test[,2])
RL.df.test$dummies.sex <- RL.df.test$dummies.sex.test...2.
RL.df.test$dummies.sex.test...2. <- NULL


#Prediccions
RL.predictions <- predict(RL.model, RL.df.test)
View(RL.predictions)
plot(RL.predictions)

#Apply Sigmoide function
sigmoideFunction <- function(x){1/(1+exp(-x))}
sigmoideNormailzedValues <- sapply(RL.predictions, sigmoideFunction) #https://www.guru99.com/r-apply-sapply-tapply.html#3
plot(sigmoideNormailzedValues)
abline(h=0.5, col="red") # http://www.sthda.com/english/wiki/abline-r-function-an-easy-way-to-add-straight-lines-to-a-plot-using-r-software#add-an-horizontal-line

#Computing survived parameter
RL.survived <- ifelse(sigmoideNormailzedValues > 0.5, 1,0)
View(RL.survived)
RL.df.test$predictedValue <- sigmoideNormailzedValues
RL.df.test$PredictedSurvived <- RL.survived #https://www.marsja.se/how-to-add-a-column-to-dataframe-in-r-with-tibble-dplyr/

#Test with the real results
RL.df.test$RealSurvived <- df.realSurvived$Survived

#Confusion Matrix
PredictedSurvivedFactor <- factor(RL.df.test$PredictedSurvived)
RealSurvivedFactor <- factor(RL.df.test$RealSurvived)
RL.confusionMatrix <- confusionMatrix(PredictedSurvivedFactor,RealSurvivedFactor)
RL.confusionMatrix

#DT - Decision Tree
DT.df <- df.clean

#C50
DT.C50.df <- DT.df
DT.C50.df.test <- df.test
if(!require(C50)){
  install.packages('C50', repos='http://cran.us.r-project.org')
  library(C50)
}
library(C50)
DT.df.y <- DT.df[1]
DT.df.y.factor = as.factor(DT.df.y)
DT.df.x <- DT.df[2:7]
DT.model <- C50::C5.0(DT.df.x, DT.df.y.factor, rules=TRUE )
summary(DT.model)

#rpart
if (!require('rpart')) install.packages('rpart'); library('rpart')
if (!require('rpart.plot')) install.packages('rpart.plot'); library('rpart.plot')

DT.rpart.df <- DT.df
DT.rpart.df.test <- df.test

#Model creation
DT.rpart.model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, data=DT.rpart.df)
summary(DT.rpart.model)
rpart.plot(DT.rpart.model)

#Predictions
DT.rpart.predictions <- predict(DT.rpart.model, DT.rpart.df.test)
View(DT.rpart.predictions)

DT.rpart.survived <- ifelse(DT.rpart.predictions > 0.5, 1,0)
View(DT.rpart.survived)

#Adding columns for the comparison
DT.rpart.predictions <- factor(DT.rpart.predictions)
DT.rpart.df.test$predictedValue <- DT.rpart.predictions
DT.rpart.df.test$PredictedSurvived <- DT.rpart.survived
DT.rpart.df.test$RealSurvived <- df.realSurvived$Survived

#Confusion Matrix

DT.rpart.predictedSurvived.factor <- factor(DT.rpart.df.test$PredictedSurvived)
DT.rpart.realSurvived.factor <- factor(DT.rpart.df.test$RealSurvived)

DT.rpart.confusionMatrix <- confusionMatrix(DT.rpart.predictedSurvived.factor,DT.rpart.realSurvived.factor)
DT.rpart.confusionMatrix
