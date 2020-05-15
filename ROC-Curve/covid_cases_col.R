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

# Modelos de arboles
library('rattle')
library('rpart.plot')
library('RColorBrewer')
library('lattice')
library('caret')


set.seed(2345)
data_covid_tree <- select(cases_covid, -Nombre.localidad.residencia, -Fecha.de.diagnóstico)

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


my_tree_one <- rpart(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Edad + Tipo.de.caso + Ubicación, data = data_train, method = 'class')
fancyRpartPlot(my_tree_one)
my_prediction <- predict(my_tree_one,data_test,type="class")
solution_tree_one <- data.frame(Estado = my_prediction)
confusionMatrix(my_prediction, data_test$Estado)

my_tree_two <- rpart(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Edad + Tipo.de.caso + Ubicación, data = data_train, method = 'class', control = rpart.control(minsplit = 5, cp=0))
fancyRpartPlot(my_tree_two)
my_prediction <- predict(my_tree_two,data_test,type="class")
solution_tree_two <- data.frame(Estado = my_prediction)
confusionMatrix(my_prediction, data_test$Estado)


# Curva ROC
library(knitr)
library(randomForest)

head(cases_covid)
summary(cases_covid)

data_covid_roc <- select(cases_covid, -Nombre.localidad.residencia, -Fecha.de.diagnóstico)
attach(data_covid_roc)

# marital = relevel(x,ref="x") ???

#fit logistic regresion model
full_covid_model <- glm(Estado ~ Ciudad.de.residencia + Numero.localidad.residencia + Edad + Tipo.de.caso + Ubicación, family = binomial(link = 'logit'))

summary(full_covid_model)

# Calculate Pseudo R-2

full_covid_model_red <- glm(Estado~1, family=binomial(link='logit'))
data_covid_roc.rf <- 1-(logLik(full_covid_model))/(logLik(full_covid_model_red))

data_covid_roc.rf <- randomForest(data_covid_roc)
importance(data_covid_roc.rf) # importance of each predictor ???

# Calculate sensitivity and false positive measures for logit model

fity_ypos <- full_covid_model$fitted.values[full_covid_model$y == 1]
fity_yneg <- full_covid_model$fitted.values[full_covid_model$y == 0]

sort_fity <- sort(full_covid_model$fitted.values)

sens <- 0
spec_c <- 0

for (i in length(sort_fity):1){
  sens <- c(sens, mean(fity_ypos >= sort_fity[i]))
  spec_c <- c(spec_c, mean(fity_yneg >= sort_fity[i]))
} 

# Calculate sensitivity and false positive measure for random forest model

fity_ypos2 <- as.numeric(data_covid_roc.rf$pred[data_covid_roc.rf$y == 1]) - 1
fity_yneg2 <- as.numeric(data_covid_roc.rf$pred[data_covid_roc.rf$y == 0]) - 1

sort_fity2 <- as.numeric(sort(data_covid_roc.rf$pred)) - 1

sens2 <- 0
spec_c2 <- 0

for (i in length(sort_fity2):1){
  sens2 <- (c(sens2, mean(fity_ypos2 >= sort_fity2[i])))
  spec_c2 <- (c(spec_c2, mean(fity_yneg2 >= sort_fity2[i])))
} 

# plot ROC curves

plot(spec_c, sens, xlim = c(0, 1), ylim = c(0, 1), type = "l", 
     xlab = "false positive rate", ylab = "true positive rate", col = 'blue')
abline(0, 1, col= "black")

lines(spec_c2, sens2, col='green')
legend("topleft", legend = c("logit","random forest") , pch = 15, bty = 'n', col = c("blue","green"))

npoints2 <- length(sens2)

# Discrete approximation area under the curve, using Trapezoidal Rule

area2 <- sum(0.5 * (sens2[-1] + sens2[-npoints2]) * (spec_c2[-1] - spec_c2[-npoints2]))

# Alternative definition of lift: sensitivity rate - false positive rate

lift2 <- (sens2 - spec_c2)[-1]

cutoff2 <- sort_fity2[lift2 == max(lift2)][1]

sensopt2 <- sens2[-1][lift2 == max(lift2)][1]

spec_copt2 <- spec_c2[-1][lift2 == max(lift2)][1]

list(area2 = area2, cutoff2 = cutoff2, sensopt = sensopt2, spec_copt2 = spec_copt2)

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
