# * * * * * * * * * * * * * * * * * * * * * * * * * *
# ESAME UTF-01 Linguaggi di programmazione per il Machine Learning
#
#	Corso 2020-2022 AI-ML
#
# * * * * * * * * * * * * * * * * * * * * * * * * * *

library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)

#----------------------------------------------------------------------------------------------------------------------------
# Caricare il dataset heart.csv e analizzare dettagliatamente la struttura.

posizione_dataset <- "C:/Users/marco/Desktop/ITSAR-AIML/UTF-01 Linguaggi di programmazione per il Machine Learning/R/Esame/heart.csv"

dataset <- read.csv(posizione_dataset, header = TRUE, stringsAsFactors = FALSE)

#----------------------------------------------------------------------------------------------------------------------------

# Controllare se sono preseni valori NA e, nel caso, rimuoverli.

if(sum(is.na(dataset)) > 0){
  print("Nel dataset sono presenti dei valori NA")
  dataset <- na.omit(dataset)
}else{
  print("Nel dataset non sono presenti dei valori NA")
}

dataset$sex[dataset$sex == "unspecified"] <- NA
dataset$chol[dataset$chol == "undefined"] <- NA


#----------------------------------------------------------------------------------------------------------------------------

# Trasformare i dati in modo che siano tecnicamente corretti.
# tipizzare in modo corretto

dataset <- subset(dataset, age>0) 

grafico <- ggplot(data=dataset, aes(age))+geom_histogram(bins=15,colour="white",size=1)+labs(title = "Grafico età",x = "Età",y = "Frequenza")
plot(grafico)

dataset$sex <- as.factor(dataset$sex) #V
levels(dataset$sex) <- c("female", "male")
dataset$chol <- as.integer(dataset$chol)
dataset$cp <- as.factor(dataset$cp)
# (cp 1,2,3,4) i valori non corrispondono, quindi per evitare ambiguità o assegnamenti errati
#abbiamo deciso di non assegnare i valori ai livelli.
dataset$fbs <- as.factor(dataset$fbs)
levels(dataset$fbs) <- c("false", "true")
dataset$restecg <- as.factor(dataset$restecg)
levels(dataset$restecg) <- c("normal", "st-t wawe abnormality","probable or definite left ventricular hypertrophy")
dataset$slope <- as.factor(dataset$slope) 
# (slope 1,2,3,4) i valori non corrispondono, quindi per evitare ambiguità o assegnamenti errati
#abbiamo deciso di non assegnare i valori ai livelli.
dataset$exang <- as.factor(dataset$exang)
levels(dataset$exang) <- c("no", "yes")
dataset$ca <- as.factor(dataset$ca)
dataset$thal <- as.factor(dataset$thal)
# (thal) i valori non corrispondono (3,6,7), quindi per evitare ambiguità o assegnamenti errati
#abbiamo deciso di non assegnare i valori ai livelli.
dataset$target <- as.factor(dataset$target) 
dataset <- na.omit(dataset)

#----------------------------------------------------------------------------------------------------------------------------

# Rinominare le colonne in maniera appropriata

colnames(dataset)[c(1)] <- c("id")  #x
colnames(dataset)[c(4)] <- c("chestPain") #cp
colnames(dataset)[c(5)] <- c("restingBloodPressure")  #trestbps
colnames(dataset)[c(6)] <- c("cholesterol") #chol
colnames(dataset)[c(7)] <- c("fastingBloodSugar") #fbs
colnames(dataset)[c(8)] <- c("restingElectrocardiographicResults")  #restecg
colnames(dataset)[c(9)] <- c("maxHeartRate")  #thalach
colnames(dataset)[c(10)] <- c("exerciseInducedAngina")  #exang
colnames(dataset)[c(13)] <- c("nMainVesselsStainedfluorosopy")  #ca
colnames(dataset)[c(15)] <- c("vaselNarrowing") #target

#----------------------------------------------------------------------------------------------------------------------------

# Rimuovere le colonne ritenute non necessarie.

dataset <- subset(dataset, select = - id)

#----------------------------------------------------------------------------------------------------------------------------

# Trasformare i dati in modo che siano consistenti. Assumere, ad esempio, che la
# frequenza cardiaca massima non possa essere superiore a 222, sostituendo i valori
# maggiori di 222 con il valore medio della variabile.

# Prima
#hist(dataset$maxHeartRate)

# ggplot2
grafico <- ggplot(data=dataset, aes(maxHeartRate))+geom_histogram(bins=15,colour="white",size=1)+labs(title = "Frequenza dei battiti massimi",x = "maxHeartRate",y = "Frequenza")
plot(grafico)

dataset$maxHeartRate[dataset$maxHeartRate > 222] <- median(dataset$maxHeartRate)

# Dopo
#hist(dataset$maxHeartRate)  

# ggplot2
grafico <- ggplot(data=dataset, aes(maxHeartRate))+geom_histogram(bins=15,colour="white",size=1)+labs(title = "Frequenza dei battiti massimi",x = "maxHeartRate",y = "Frequenza")
plot(grafico)

#----------------------------------------------------------------------------------------------------------------------------

# Trasformare i dati in modo che siano consistenti. Assumere come outlier, ad
# esempio, i valori relativi alla pressione sanguigna a riposo che non rispettano la
# 1.5xIQR Rule. 

restbps <- dataset$restingBloodPressure 

iqr <- IQR(dataset$restingBloodPressure, na.rm = TRUE)

q1 <- quantile(dataset$restingBloodPressure, 0.25, na.rm = TRUE)
q2 <- quantile(dataset$restingBloodPressure, 0.50, na.rm = TRUE)
q3 <- quantile(dataset$restingBloodPressure, 0.75, na.rm = TRUE)
q4 <- quantile(dataset$restingBloodPressure, 1, na.rm = TRUE)

risq1 <- (q1 - (1.5 * iqr))
risq3 <- (q3 + (1.5 * iqr))

restbps <- restbps[(restbps > risq1)]
restbps <- restbps[(restbps <= risq3)]

#boxplot(dataset$restingBloodPressure,na.rm=TRUE) #Originale
#boxplot(restbps,na.rm=TRUE) #1.5xIQR rule

ggboxplot <- ggplot(dataset, aes(restingBloodPressure)) + geom_boxplot(varwidth = T) + 
  labs(
    title = "1.5xIQR rule",
    subtitle = "Prima",
    caption = "boxplot"
  )
plot(ggboxplot)

dataset <- filter(dataset, restingBloodPressure > risq1 & restingBloodPressure < risq3)

ggboxplot <- ggplot(dataset, aes(restingBloodPressure)) + geom_boxplot(varwidth = T) + 
  labs(
    title = "1.5xIQR rule",
    subtitle = "Dopo",
    caption = "boxplot"
  )
plot(ggboxplot)

#----------------------------------------------------------------------------------------------------------------------------

# Analizzare la relazione tra due variabili del dataset attraverso la regressione lineare
# semplice e determinare:
# . il grafico del modello;
# . il coefficiente angolare e l'intercetta (interpretabile) della retta di regressione;
# . il tipo di relazione tramite r e la bontà del modello tramite R^2;
# . l'analisi dei residui e la distribuzione in quantili, con i relativi grafici.
dataset <- na.omit(dataset)
dataset_reg <-lm(dataset$age ~ dataset$cholesterol,data = dataset)
summary(dataset_reg)

plot(dataset$age ~ dataset$cholesterol, xlab="Colesterolo", ylab="Età", main="Regr.lin età (age) e livello di colesterolo (cholesterol)")
abline(dataset_reg, col = "red")
segments(dataset$cholesterol, fitted(dataset_reg), dataset$cholesterol, dataset$age, col = "blue", lty = 2)

plot(dataset_reg$fitted, dataset_reg$residuals,main = "Residui")
abline(0,0)

qqnorm(dataset_reg$residuals, main = "Distribuzione in quantili") 
qqline(dataset_reg$residuals, col = "red")

r <- cor(dataset$cholesterol, dataset$age)

summary(dataset_reg)

#----------------------------------------------------------------------------------------------------------------------------

# Creare un data frame contenente 10 osservazioni (non presenti nel dataset) ed
# effettuare delle previsioni.

df_pred <- data.frame("age" = c(99,98,87,1,2,3,4,5,6,7))
predict(dataset_reg, df_pred)

#----------------------------------------------------------------------------------------------------------------------------

# Applicare un modello di Machine Learning a scelta, misurandone l’accuratezza sul test set.
dataset <- na.omit(dataset)

test_set <- dataset

seed = set.seed(2021)
control <- trainControl(method = "cv", number = 10, seed = seed)
metric <- "Accuracy"

fit_knn <- train(chestPain ~ ., data = test_set, metric = metric, trControl = control, method = "knn")

predictions <- predict(fit_knn, test_set)
confusionMatrix(predictions, test_set$chestPain)

summary(fit_knn)
#----------------------------------------------------------------------------------------------------------------------------
# Analisi descrittiva 
freq_sex <- table(dataset$sex)
freq_sex / sum(freq_sex)

stampa <- paste("female =",round((freq_sex[1]/sum(freq_sex))*100,2),"%"," male = ",round((freq_sex[2]/sum(freq_sex))*100,2),"%")

grafico_sex_barplot <- ggplot(dataset, aes(x = "",fill = sex)) + geom_bar(width = 0.5) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) + 
  labs(
    title = "Genere",
    subtitle = stampa,
    xlab="a",
    y=""
  )
grafico_sex <- grafico_sex_barplot + coord_polar("y", start=0)

plot(grafico_sex)
#Da questo grafico possiamo notare una grossa differenza di genere tra i pazienti.

grafico_col <- ggplot(data=dataset, aes(cholesterol))+geom_histogram(bins=15,colour="white",size=1)+labs(title = "Colesterolo in mg/dl",x = "Colesterolo (mg/dl)",y = "Frequenza")
plot(grafico_col)
# Possiamo notare una concentrazione maggiore di soggetti con un colesterolo compreso tra poco più di 150  e 300


grafico_restecg <- ggplot(dataset, aes(restingElectrocardiographicResults)) +  geom_bar(aes(fill = restingElectrocardiographicResults), width = 0.5) + 
  theme(axis.text.x = element_blank())+
  labs(
    title = "Rapporto",
    subtitle = "",
    x = "",
    y = "Frequenza",
    caption = ""
  )

plot(grafico_restecg)
# Mentre, da questo grafico risulta che solo una piccolissima parte di pazienti soffre ipertrofia ventricolare sinistra


grafico_fbs <- ggplot(dataset, aes(fastingBloodSugar)) +  geom_bar(aes(fill = fastingBloodSugar), width = 0.5) + 
  theme(axis.text.x = element_blank()) +  
  labs(
    title = "Rapporto",
    subtitle = "fasting blood sugar > 120 mg/dl",
    x = "",
    y = "Frequenza",
    caption = ""
  )

plot(grafico_fbs)
# Questo grafico ci mostra che la maggior parte dei pazienti non presenta quantitativi eccessivi di
# zuccheri nel sangue.

grafico_exang <- ggplot(dataset, aes(exerciseInducedAngina)) +  geom_bar(aes(fill = exerciseInducedAngina), width = 0.5) + 
  labs(
    title = "Rapporto",
    subtitle = "",
    x = "",
    y = "Frequenza",
    caption = ""
  )

plot(grafico_exang)

table(dataset$sex)
table(dataset$cholesterol)
table(dataset$restingElectrocardiographicResults)
table(dataset$exerciseInducedAngina)
table(dataset$fastingBloodSugar)



