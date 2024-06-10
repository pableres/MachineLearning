library(dplyr)
library(textshape)
library(stats)

#cargar y limpiar la base de datos.
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


#regresion
modelrl <- glm(stroke ~ ., data=trainset, family = "binomial")  # build linear regression model on full data
print(modelrl)
summary(modelrl)

#comprobando los modelos.
infartorl <- predict(modelrl, testset)
infartorl[infartorl<0] <- 0
infartorl[infartorl>0] <- 1
infartorl

actuals_preds <- data.frame(cbind(real=testset$stroke, regres=infartorl))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy

attach(actuals_preds)
head(actuals_preds)
actuals_preds
tabla1<-table(real,regres)
prop.table(tabla1)%>% 
{. * 100} %>% 
  round(2)
