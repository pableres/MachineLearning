library(randomForest)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(dplyr)
library(textshape)
library(stats)

df <- read.csv("C:/Users/pable/Downloads/healthcare-dataset-stroke-data.csv", sep=";")
df<-textshape::column_to_rownames(df, loc = 1)
df<- df[complete.cases(df), ]
sapply(df, class)
df<-subset(df, gender != "Other")
df$gender<-as.factor(df$gender)
nlevels(df$gender)
levels(df$gender)
df$bmi<-as.numeric(df$bmi)
df$smoking_status<-as.factor(df$smoking_status)
nlevels(df$smoking_status)
levels(df$smoking_status)
sapply(df, class)

df<- df[complete.cases(df), ]

#reducir la base de datos

soloinfarto<- df[df$stroke == 1,]
NOinfarto<- df[df$stroke == 0,]
brevessininfarto<- NOinfarto[sample(nrow(NOinfarto), 209), ]
df<- data.frame(rbind(brevessininfarto, soloinfarto))

#entrenamos y testeamos (subdata)

bound <- floor(nrow(df)/3)         #define % of training and test set
entrenamiento <- df[sample(nrow(df)), ]           #sample rows 
testset <- entrenamiento[1:bound, ]              #get training set
trainset <- entrenamiento[(bound+1):nrow(entrenamiento), ] 


#un solo árbol primero
first_model <- rpart(stroke~., data=trainset)
first_model
rpart.plot(first_model)

#Ahoa si el random forest
modelorf <- randomForest(as.factor(stroke)~., data=trainset, ntree=500)
modelorf

modelorf$importance

plot(modelorf)


#comprobando los modelos.

infartorf <- predict(modelorf, testset)

infartorf<-as.numeric(infartorf)
infartorf[infartorf<2] <- 0
infartorf[infartorf>1] <- 1
infartorf

actuals_preds <- data.frame(cbind(real=testset$stroke, random=infartorf))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)
actuals_preds
attach(actuals_preds)
tabla2<-table(real,random)
prop.table(tabla2)%>% 
  {. * 100} %>% 
  round(2)

