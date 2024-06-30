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


#Red neuronal.
testset <- as.data.frame(apply(testset, 2, as.integer)) 
trainset <- as.data.frame(apply(trainset, 2, as.integer))
sapply(testset, class)

str(testset)
modelrn<-neuralnet(stroke~ age + hypertension + heart_disease + avg_glucose_level + bmi , data=trainset, hidden=c(6,2),act.fct = "logistic",linear.output = F,threshold = 0.05)
modelrn
plot(modelrn, rep="best")
print(modelrn)

#comprobando los modelos.
#infartorl <- predict(modelrl, testset)
#infartorf <- predict(modelorf, testset)
infartorn <- predict.nn(modelrn, testset)

a<-infartorn$net.result
sino <- ifelse(a>0.5,1,0)

actuals_preds <- data.frame(cbind(real=testset$stroke, neuro=sino))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)
actuals_preds
attach(actuals_preds)
table(real,V2)
prop.table(tabla1)%>% 
  {. * 100} %>% 
  round(2)
