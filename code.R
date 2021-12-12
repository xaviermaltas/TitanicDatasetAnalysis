dfFull <- read.csv("dataset/train.csv")
dfTest <- read.csv("dataset/test.csv")

head(dfFull)
colnames(dfFull)

#Variables que mantenim - 'survived', 'pclass', 'name', 'sex', 'age', 'sibsp', 'parch', 'fare'
# La variable 'name', pot tenir algun tipus amb el títol nobiliari? 

relevantFields <- c("Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Fare")
df <- dfFull[relevantFields]

#Check NA values
colSums(is.na(df))
colSums(df=="")

#All NA values of 'Age' var to mean age
df$Age[is.na(df$Age)] <- mean(df$Age,na.rm=T)

colSums(is.na(df))
colSums(df=="")