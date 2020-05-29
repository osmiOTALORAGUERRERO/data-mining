# Read data
path <- 'https://raw.githubusercontent.com/osmiOTALORAGUERRERO/data-mining/master/ROC-Curve/osb_enftransm-covid-19-%20(2).csv'
cases_covid <- read.csv(path, sep=';')

# basic analisis #
str(cases_covid)

summary(cases_covid)

filter(cases_covid, Ubicación == "Fallecido" & Estado == "Recuperado")
### Limpieza de los datos ###
library(dplyr)
library(tidyr)

# Change format of Localidad.de.residencia, split every object by number and name from Localidad.de.residencia
cases_covid <- cases_covid %>%
  separate(Localidad.de.residencia, c('Numero.localidad.residencia', 'Nombre.localidad.residencia'), ' - ')

# Error in var Tipo.de.caso in factor "En estudio" and "En Estudio" for same reference -> unify to "En estudio",  "relacionado" and "Relacionado" -> "Relacionado" 
cases_covid$Tipo.de.caso[cases_covid$Tipo.de.caso == 'En Estudio'] <- "En estudio"
cases_covid$Tipo.de.caso[cases_covid$Tipo.de.caso == 'relacionado'] <- "Relacionado"

# Date format
cases_covid <- cases_covid %>%
  mutate( Fecha.de.diagnóstico = as.Date(str_c(Fecha.de.diagnóstico,'20'), format = '%d/%m/%Y'))

# Eliminar niveles no usado
cases_covid$Tipo.de.caso <- factor(cases_covid$Tipo.de.caso)

str(cases_covid)

summary(cases_covid)

# Distribución de las variables
library(ggplot2)

ggplot(cases_covid, aes(x = Ciudad.de.residencia)) +
  geom_bar()

ggplot(cases_covid, aes(x = Numero.localidad.residencia)) + 
  geom_bar()

ggplot(cases_covid, aes(x = Nombre.localidad.residencia)) + 
  geom_bar()

ggplot(cases_covid, aes(x = Edad)) + 
  geom_histogram()

ggplot(cases_covid, aes(x = Sexo)) + 
  geom_bar()

ggplot(cases_covid, aes(x = Tipo.de.caso)) +
  geom_bar()

ggplot(cases_covid, aes(x = Ubicación)) + 
  geom_bar()

ggplot(cases_covid, aes(x = Estado)) +
  geom_bar()

### Correlación de las variables ###

# Correlación entre el genero y el resto de los datos
ggplot(cases_covid, aes(x=Fecha.de.diagnóstico, y = Sexo))+
  geom_line() + 
  scale_x_date(date_labels = '%B %d')

ggplot(cases_covid, aes(x=Sexo, fill=Ciudad.de.residencia))+
  geom_bar(position = 'dodge')

ggplot(cases_covid, aes(x=Nombre.localidad.residencia, fill=Sexo))+
  geom_bar(position = 'dodge')

ggplot(cases_covid, aes(x=Edad, colour=Sexo))+
  geom_freqpoly(position = 'dodge')

ggplot(cases_covid, aes(x=Sexo, fill=Tipo.de.caso))+
  geom_bar(position = 'dodge')

ggplot(cases_covid, aes(x=Sexo, fill=Ubicación)) +
  geom_bar(position = 'dodge')

ggplot(cases_covid, aes(x=Sexo, fill=Estado)) +
  geom_bar(position = 'dodge')

# Indetificar y cuantificar localidades más afectadas
prueba <- within(cases_covid, Nombre.localidad.residencia <- factor(Nombre.localidad.residencia,
                                                                    levels = names(sort(table(Nombre.localidad.residencia),                                                                                       decreasing = TRUE))))
ggplot(prueba, aes(x = Nombre.localidad.residencia)) + 
  geom_bar()

ggplot(prueba, aes(x = Nombre.localidad.residencia, fill=Estado)) +
  geom_bar(position = 'dodge')

ggplot(cases_covid, aes(Fecha.de.diagnóstico, colour=Nombre.localidad.residencia))+
  geom_density(alpha = 0.2, show.legend=FALSE) +
  facet_wrap('Nombre.localidad.residencia ~ .')
  scale_x_date(date_labels = '%b %d')

# Rangos de edades y distribución
ggplot(cases_covid, aes(x = Edad, fill=Estado, colour = Estado)) + 
  geom_freqpoly(bins = 100)

ggplot(cases_covid, aes(x = Edad, fill=Estado, colour = Estado)) + 
  geom_histogram(bins = 100, position = 'dodge', alpha=0.2)

# Ubicación comparandola con la localidad
ggplot(cases_covid, aes(x=Ubicación, fill=Ubicación))+
  geom_bar(position = 'dodge', show.legend=FALSE)+
  facet_wrap(Nombre.localidad.residencia ~ .)

# Correlación del origen del caso con las demas variables
ggplot(cases_covid, aes(Fecha.de.diagnóstico, y=Tipo.de.caso))+
  geom_line() +
  scale_x_date(date_labels = '%b %d')

ggplot(cases_covid, aes(Fecha.de.diagnóstico, colour=Tipo.de.caso))+
  geom_density(alpha = 0.2) +
  scale_x_date(date_labels = '%b %d')

ggplot(cases_covid, aes(Tipo.de.caso , fill=Ciudad.de.residencia)) +
  geom_bar(position = 'dodge')

ggplot(cases_covid, aes(Tipo.de.caso, colour=Tipo.de.caso)) + 
  geom_bar(alpha=0.5, show.legend = FALSE) +
  facet_wrap(Nombre.localidad.residencia ~ .)

ggplot(cases_covid, aes(Tipo.de.caso, fill=Sexo)) +
  geom_bar(position = 'dodge')

ggplot(cases_covid, aes(x=Tipo.de.caso, y=Edad, colour=Estado))+
  geom_jitter(alpha=0.5)

ggplot(cases_covid, aes(Tipo.de.caso, fill=Ubicación)) +
  geom_bar(position = 'dodge')

ggplot(cases_covid, aes(Estado, fill=Tipo.de.caso)) +
  geom_bar(position = 'dodge')

ggplot(cases_covid, aes(Tipo.de.caso, colour=Tipo.de.caso)) +
  geom_violin(alpha = 0.5) +
  facet_grid(. ~ Estado)

### mapa
library(sf)
# library(maps)
# library(rgdal)
# library(gpclib)
# library(maptools)

Bog <- st_read(dsn='poligonos-localidades')

Bog <- rename(Bog,  Numero.localidad.residencia = Identificad)

freq.bog <- data.frame(table(cases_covid$Numero.localidad.residencia))
freq.bog <- rename(freq.bog, Numero.localidad.residencia=Var1, Cases=Freq)

Bog <- merge(Bog, freq.bog, by='Numero.localidad.residencia')

ggplot(data=Bog, aes(fill=Cases)) +
  geom_sf()+
  geom_sf_label(aes(label = Nombre_de_l))

### Modelos de arboles ###
library('rattle')
library('rpart.plot')
library('RColorBrewer')
library('lattice')
library('caret')
library('party')

set.seed(420)
data_covid_tree <- select(cases_covid, -Nombre.localidad.residencia, -Fecha.de.diagnóstico)

data_covid_tree$Etapa.crecimiento <- NULL

data_covid_tree$Etapa.crecimiento[data_covid_tree$Edad > 0 & data_covid_tree$Edad <= 5] <- 'Primera infancia'
data_covid_tree$Etapa.crecimiento[data_covid_tree$Edad > 5 & data_covid_tree$Edad <= 11] <- 'Infante'
data_covid_tree$Etapa.crecimiento[data_covid_tree$Edad > 11 & data_covid_tree$Edad <= 18] <- 'Adolescente'
data_covid_tree$Etapa.crecimiento[data_covid_tree$Edad > 18 & data_covid_tree$Edad <= 29] <- 'Joven adulto'
data_covid_tree$Etapa.crecimiento[data_covid_tree$Edad > 29 & data_covid_tree$Edad <= 59] <- 'Adulto'
data_covid_tree$Etapa.crecimiento[data_covid_tree$Edad > 59] <- 'Anciano'

ggplot(data_covid_tree, aes(Etapa.crecimiento, fill=Estado)) +
  geom_bar(position = "dodge")

data_train <- sample_frac(data_covid_tree, .8)
data_test <- setdiff(data_covid_tree, data_train)


table(data_train$Sexo, data_train$Estado)
prop.table(table(data_train$Sexo, data_train$Estado),1)
table(data_test$Sexo, data_test$Estado)
prop.table(table(data_test$Sexo, data_test$Estado),1)

table(data_train$Estado)
table(data_test$Estado)
table(data_train$Ciudad.de.residencia)
table(data_test$Ciudad.de.residencia)
table(data_train$Numero.localidad.residencia)
table(data_test$Numero.localidad.residencia)
table(data_train$Edad)
table(data_test$Edad)
table(data_train$Sexo)
table(data_test$Sexo)
table(data_train$Ubicación)
table(data_test$Ubicación)
table(data_train$Tipo.de.caso)
table(data_test$Tipo.de.caso)

# tree with Edad and without Etapa.crecimiento
my_tree_one <- rpart(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Edad + Tipo.de.caso + Ubicación, data = data_train, method = 'class')
fancyRpartPlot(my_tree_one)
my_prediction <- predict(my_tree_one,data_test,type="class")
solution_tree_one <- data.frame(Estado = my_prediction)
confusionMatrix(my_prediction, data_test$Estado)

# tree with Etapa.crecimineto and withot Edad
my_tree_two <- rpart(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Etapa.crecimiento + Tipo.de.caso + Ubicación, data = data_train, method = 'class')
fancyRpartPlot(my_tree_two)
my_prediction <- predict(my_tree_two,data_test,type="class")
solution_tree_two <- data.frame(Estado = my_prediction)
confusionMatrix(my_prediction, data_test$Estado)

# with more parameters
my_tree_three <- rpart(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Edad + Tipo.de.caso + Ubicación, data = data_train, method = 'class', control = rpart.control(minsplit = 1, cp=0))
fancyRpartPlot(my_tree_three)
my_prediction <- predict(my_tree_three,data_test,type="class")
solution_tree_three <- data.frame(Estado = my_prediction)
confusionMatrix(my_prediction, data_test$Estado)

# with more parameters
my_tree_four <- rpart(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Etapa.crecimiento + Tipo.de.caso + Ubicación, data = data_train, method = 'class', control = rpart.control(minsplit = 1, cp=0))
fancyRpartPlot(my_tree_four)
my_prediction <- predict(my_tree_three,data_test,type="class")
solution_tree_four <- data.frame(Estado = my_prediction)
confusionMatrix(my_prediction, data_test$Estado)

# with All parameters
my_tree_five <- rpart(Estado ~ ., data = data_train, method = 'class', control = rpart.control(minsplit = 1, cp=0))
fancyRpartPlot(my_tree_five)
my_prediction <- predict(my_tree_five,data_test,type="class")
solution_tree_five <- data.frame(Estado = my_prediction)
confusionMatrix(my_prediction, data_test$Estado)


### Curva ROC

## ROC 2
library(pROC)
library(randomForest)

set.seed(420)

full_covid_model <- glm(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Edad + Tipo.de.caso + Ubicación, family = binomial(link = 'logit'))
sort_fity <- sort(full_covid_model$fitted.values)
obese <- ifelse(test = (runif(n=nrow(data_covid_roc))<(rank(sort_fity)/100)), yes=1, no=0)

plot(x=sort_fity, y=obese)

glm.fit = glm(obese ~ sort_fity, family = binomial)
lines(sort_fity, glm.fit$fitted.values)

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive",
    ylab="True Positive", print.auc=TRUE)
roc.info = roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE)
roc.df = data.frame(tpp = roc.info$sensitivities*100,
                    fpp=(1-roc.info$specificities)*100,
                    thresholds = roc.info$thresholds)
head(roc.df)
tail(roc.df)

rf.model <- randomForest(factor(obese)~sort_fity)

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive",
    ylab="True Positive", print.auc=TRUE, col='#377eb8')
plot.roc(obese, rf.model$votes[,1], percent=TRUE, print.auc=TRUE, add=TRUE, print.auc.y=40, col='#4daf4a')
legend("bottomright", legend=c('logistic regresion', 'random forest'), col=c('#377eb8','#4daf4a'), lwd=4)


#----------------------------------------------------------------------------------------------------

library(randomForest)
library(pROC)

set.seed(420)

#first create a simple tree
# I use the the my_tree_three 
fancyRpartPlot(my_tree_three)

# get prediction
tree_pred <- predict(my_tree_three, data_test, type='class')

#table(tree_pred, data_test$Estado)
# get confusion matrix
cm_three <- confusionMatrix(my_prediction, data_test$Estado)
cm_three$table

# get prediction prob
tree_pred_prob <- predict(my_tree_three, data_test, type='prob')

##Model with random forest
model_rf <- randomForest(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Edad + Tipo.de.caso + Ubicación,
                         data = data_train)
# get prediction
pred_rf <- predict(model_rf, data_test)
# get confusion matrix
cm_rf <- confusionMatrix(pred_rf, data_test$Estado)
# get prediction prob
prob_rf <- predict(model_rf, data_test, type = 'prob')


## model with random forest2
model_rf_2 <- randomForest(Estado ~ Ciudad.de.residencia + Edad + Tipo.de.caso + Ubicación,
                         data = data_train, ntree = 500, type='regression')
# prediction
pred_rf_2 <-predict(model_rf_2, data_test)
#confusion matrix
cm_rf_2 <- confusionMatrix(pred_rf_2, data_test$Estado)
# get prediction prob
prob_rf_2 <- predict(model_rf_2, data_test, type = 'prob')


# tree with Edad and without Etapa.crecimiento
model_a <- rpart(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Edad + Tipo.de.caso + Ubicación, data = data_train, method = 'anova', control = rpart.control(minsplit = 1, cp=0))
pred_a <- predict(model_a,data_test)
confusionMatrix(pred_a, data_test$Estado)
prob_a <- predict(model_a,data_test, type='prob')

# glm
model_glm <- glm(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Edad + Tipo.de.caso + Ubicación, data = data_train, family=binomial)
pred_glm <- predict(model_glm,type=c("response"))

## cross folding
library(cvAUC)
data_covid_roc <- data_covid_tree
k <- 5
N <- dim(data_covid_roc)[[1]]
min_index <- 0
folds<-list()

for (i in 1:k) {
  if (1==k) {
    max_index<-(floor(N/k))*i+(N%%K)
  } else {
    max_index<-(floor(N/k))*i
  }
  folds[i]<-list(data_covid_roc[(min_index+1):max_index,])
}

predictions.vc <- list()
for (j in 1:k) {
  test.kf <-data.frame(folds[j])

  train.kf <- list()
  for (i in 1:k) {
    if (i != j) {
      train.kf <- rbind(train.kf, data.frame(folds[i]))
    }
  }
  model_rf.vc <- randomForest(Estado ~ Ciudad.de.residencia + Edad + Tipo.de.caso + Ubicación,
                            data = train.kf, ntree = 500)
  predict_kf <- predict(model_rf.vc, test.kf, type="prob")
  predictions.vc <- rbind(predictions.vc, data.frame(predict_kf))
  
  par(pty='s')
  cv_ROC <- roc(response = test.kf$Estado, 
                  predictor = predict_kf[,1],
                  plot=TRUE, legacy.axes=TRUE, print.auc=TRUE,
                  xlab="False Positive", ylab="True Positive", col='#377eb8')

}

cvauc_ROC <- roc(response = data_covid_roc$Estado, 
              predictor = predictions.vc[,1],
              plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, add=TRUE,
              xlab="False Positive", ylab="True Positive", col='#4daf4a')

## ---- print model in roc curve ------

library(ROCR)
pred <- prediction(pred_glm, data_train$Estado)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")  
# ROC curve
library(pROC)

par(pty='s')
tree_ROC <- roc(response = data_test$Estado, 
                predictor = tree_pred_prob[,1],
                plot=TRUE, legacy.axes=TRUE, print.auc=TRUE,
                xlab="False Positive", ylab="True Positive", col='#377eb8')
rf_ROC <- roc(response = data_test$Estado,
              predictor = prob_rf[,1],
              plot=TRUE, legacy.axes=TRUE, print.auc=TRUE, print.auc.y=0.4,
              xlab="False Positive", ylab="True Positive", col='#4daf4a')

legend("bottomright", legend=c('tree desicion', 'random forest'), col=c('#377eb8','#4daf4a'), lwd=2)

