# get data
path <- 'https://raw.githubusercontent.com/osmiOTALORAGUERRERO/data-mining/master/TreeCredit/credit%20(1)%20(2).csv'
credits <- read.csv(path)


# data compression
str(credits)

summary(credits)

# data grphics 
library(ggplot2)


## Distribución de cada variable ##

ggplot(credits, aes(x=checking_balance)) +
  geom_bar()

ggplot(credits, aes(x=months_loan_duration)) + 
  geom_histogram(binwidth = 2)

#Distribución de la historia crediticia
ggplot(credits, aes(x=factor(credit_history))) +
  geom_bar()

ggplot(credits, aes(x=purpose)) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=amount)) + 
  geom_freqpoly(binwidth = 100)

ggplot(credits, aes(x=factor(savings_balance))) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=factor(employment_duration ))) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=factor(percent_of_income))) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=factor(years_at_residence))) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=age)) +
  geom_histogram(binwidth = 1)

ggplot(credits, aes(x=factor(other_credit))) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=factor(housing))) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=factor(existing_loans_count))) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=factor(job))) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=factor(dependents))) +
  geom_bar(position = 'dodge')

ggplot(credits, aes(x=factor(phone))) +
  geom_bar(position = 'dodge')

#Distribucion de aprobados y no aprobados  
ggplot(credits, aes(x=default)) + 
  geom_bar()


## Correlacion entre variables ##

##Distribución de checking_balance y default
ggplot(credits, aes(x=checking_balance, fill=default))+
  geom_bar(position = 'dodge')

##Distribución de months_loan_duration y default
ggplot(credits, aes(months_loan_duration, colour=default))+
  geom_freqpoly(binwidth = 1)

##Distribución de credit_history y default
ggplot(credits, aes(x=credit_history, fill=default))+
  geom_bar(position = 'dodge')

##Distribución de purpose y default
ggplot(credits, aes(x=purpose, fill=default))+
  geom_bar(position = 'dodge')

##Distribución de amount y default
ggplot(credits, aes(x=amount, colour=default))+
  geom_freqpoly(position = 'dodge')

##Distribución de savings_balance y default
ggplot(credits, aes(x=savings_balance, fill=default))+
  geom_bar(position = 'dodge')

##Distribución de employment_duration y default
ggplot(credits, aes(x=employment_duration, fill=default))+
  geom_bar(position = 'dodge')

##Distribución de percent_of_income y default
ggplot(credits, aes(x=percent_of_income, fill=default))+
  geom_bar(position = 'dodge')

##Distribución de years_at_residence y default
ggplot(credits, aes(x=years_at_residence, fill=default))+
  geom_bar(position = 'dodge')

##Distribución de age y default
ggplot(credits, aes(age, fill=default))+
  geom_histogram(binwidth = 1)

#Distribución de other_credit y default
ggplot(credits, aes(x=other_credit, fill=default))+
  geom_bar(position = 'dodge')

#Distribución de housing y default
ggplot(credits, aes(x=housing, fill=default))+
  geom_bar(position = 'dodge')

#Distribución de existing_loans_count y default
ggplot(credits, aes(x=existing_loans_count, fill=default))+
  geom_bar(position = 'dodge')

#Distribución de job y default
ggplot(credits, aes(x=job, fill=default))+
  geom_bar(position = 'dodge')

#Distribución de dependents y default
ggplot(credits, aes(x=dependents, fill=default))+
  geom_bar(position = 'dodge')

#Distribución de phone y default
ggplot(credits, aes(x=phone, fill=default))+
  geom_bar(position = 'dodge')

#Distribución de la historia crediticia por monto y aprobacións
ggplot(credits, aes(x=factor(credit_history), y=amount)) +
  geom_jitter(size=3,alpha=0.5) +
  facet_grid(". ~ default")
#Distribución del historial crediticio, monto diferenciando duración de empleo
ggplot(credits, aes(x=factor(credit_history), y=amount, colour=employment_duration)) +
  geom_jitter(size=3,alpha=0.5) +
  facet_grid(". ~ default")
#Distribución del historial crediticio, monto diferenciando el saldo
ggplot(credits, aes(x=factor(credit_history), y=amount, colour=checking_balance)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(". ~ default")
#distribucion del historial crediticio, respecto al monto, dividido entre default, y savings_balance
ggplot(credits, aes(x=factor(credit_history), y=amount, colour=employment_duration)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(vars(default), vars(savings_balance))
#distribucion del historial crediticio, respecto al monto, dividido entre default, y checking_balance
ggplot(credits, aes(x=factor(credit_history), y=amount, colour=employment_duration)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(vars(default), vars(checking_balance))

ggplot(credits, aes(x=factor(credit_history), y=amount, colour=employment_duration)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(default + savings_balance ~ age)
#Distribución por edades, e historia crediticia, dividita con savings_balance y default con employment_duration
ggplot(credits, aes(x=factor(age), y=credit_history, colour=employment_duration)) +
  geom_point() +
  facet_grid(savings_balance+default ~ ., scales = "free", space = "free") + 
  theme(strip.text.y = element_text(angle = 0))
ggplot(credits, aes(x=factor(age), y=credit_history, colour=employment_duration)) +
  geom_point() +
  facet_grid(checking_balance+default ~ ., scales = "free", space = "free") + 
  theme(strip.text.y = element_text(angle = 0))
#Distribución por edades, e historia crediticia, dividita con checking_balance y default con purpose
ggplot(credits, aes(x=factor(age), y=credit_history, colour=purpose)) +
  geom_point() +
  facet_grid(checking_balance+default ~ ., scales = "free", space = "free") + 
  theme(strip.text.y = element_text(angle = 0))
#Distribución por edades, e historia crediticia, dividita con checking_balance y default con savings_balance
ggplot(credits, aes(x=factor(age), y=credit_history, colour=savings_balance)) +
  geom_point() +
  facet_grid(checking_balance+default ~ ., scales = "free", space = "free") + 
  theme(strip.text.y = element_text(angle = 0))

#Distribución de la historia crediticia por edad y aprobación
ggplot(credits, aes(x=factor(credit_history), y=age)) +
  geom_jitter(size=3,alpha=0.5) +
  facet_grid(". ~ default")

#Distribucion de checking_balance por historial crediticio con default
ggplot(credits, aes(x=checking_balance, fill=factor(credit_history))) +
  geom_bar(position = 'dodge') +
  facet_grid(". ~ default")
#Distribucion de historial crediticio por proposito con default
ggplot(credits, aes(x=factor(credit_history), fill=factor(purpose))) +
  geom_bar(position = 'dodge') +
  facet_grid(". ~ default")
#Distribucion de historial crediticio por edad con default y proposito
ggplot(credits, aes(x=factor(credit_history), y=age, colour=purpose)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(". ~ default")
#Distribucion de historial crediticio por edad con default y otro credito
ggplot(credits, aes(x=factor(credit_history), y=age, colour=other_credit)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(". ~ default")
#Distribucion de historial crediticio por edad con default y vivienda
ggplot(credits, aes(x=factor(credit_history), y=age, colour=housing)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(". ~ default")
#Distribucion de historial crediticio por edad con default y trabajo
ggplot(credits, aes(x=factor(credit_history), y=age, colour=job)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(". ~ default")

#distribucion con el historial creditiocio por monto, comparado con el saldo, proposito, y default
ggplot(credits, aes(x=factor(credit_history), y=amount, colour=purpose)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(vars(default), vars(checking_balance))
#distribución 
ggplot(credits, aes(x=factor(credit_history), y=amount, colour=purpose)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(default + savings_balance ~ age)
#distribución de la edad por historial crediticio, dividido por saving_balance+percent_of_income y default
ggplot(credits, aes(x=factor(age), y=credit_history, colour=purpose)) +
  geom_point() +
  facet_grid(savings_balance+percent_of_income ~ default, scales = "free", space = "free") + 
  theme(strip.text.y = element_text(angle = 0))


ggplot(credits, aes(x=factor(credit_history), y=amount, colour=months_loan_duration)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(vars(default), vars(job))
ggplot(credits, aes(x=factor(credit_history), y=amount, colour=months_loan_duration)) +
  geom_jitter(size=3,alpha=0.5) +
  facet_grid(default + checking_balance ~ age)
ggplot(credits, aes(x=factor(age), y=credit_history, colour=months_loan_duration)) +
  geom_point() +
  facet_grid(savings_balance+checking_balance ~ default, scales = "free", space = "free") + 
  theme(strip.text.y = element_text(angle = 0))
ggplot(credits, aes(x=factor(amount), y=credit_history, colour=months_loan_duration)) +
  geom_point() +
  facet_grid(savings_balance+checking_balance ~ default, scales = "free", space = "free") + 
  theme(strip.text.y = element_text(angle = 0))

ggplot(credits, aes(x=factor(credit_history), y=amount, colour=months_loan_duration)) +
  geom_jitter(size=3,alpha=0.8) +
  facet_grid(vars(default), vars(job))
ggplot(credits, aes(x=factor(credit_history), y=amount, colour=months_loan_duration)) +
  geom_jitter(size=3,alpha=0.5) +
  facet_grid(default + checking_balance ~ age)

#distribución edad por historia crediticia (checkings_balance+savings_balance / default)
ggplot(credits, aes(x=factor(age), y=credit_history, colour=months_loan_duration)) +
  geom_point() +
  facet_grid(savings_balance+checking_balance ~ default, scales = "free", space = "free") + 
  theme(strip.text.y = element_text(angle = 0))
#distribución edad por historia crediticia (checkings_balance+savings_balance / default) - months_loan_duration
ggplot(credits, aes(x=factor(age), y=credit_history, colour=amount)) +
  geom_point() +
  facet_grid(savings_balance+checking_balance ~ default, scales = "free", space = "free") + 
  theme(strip.text.y = element_text(angle = 0))

ggplot(credits, aes(percent_of_income, fill=factor(job)))+
  geom_bar(position = 'dodge')
ggplot(credits, aes(job, fill=factor(percent_of_income)))+
  geom_bar(position = 'dodge')


# Construcción del arbol de decisión

library('rattle')
library('rpart.plot')
library('RColorBrewer')
library('dplyr')
library('lattice')
library('caret')

## Arbol1
set.seed(500)
data_train <- sample_frac(credits, .7)
data_test <- setdiff(credits, data_train)

table(data_train$default)
table(data_train$credit_history, data_train$default)
prop.table(table(data_train$credit_history, data_train$default),1)

my_tree <- rpart(default ~ credit_history + age + savings_balance + employment_duration + checking_balance + percent_of_income + months_loan_duration + other_credit + existing_loans_count + purpose + amount , data=data_train, method='class')

fancyRpartPlot(my_tree)

my_prediction <- predict(my_tree,data_test,type="class")

my_solution <- data.frame(approved = my_prediction)

confusionMatrix(my_prediction, data_test[["default"]])

set.seed(1000)
data_train <- sample_frac(credits, .8)
data_test <- setdiff(credits, data_train)
table(data_train$default)
my_tree <-  rpart(default ~ credit_history + age + savings_balance + employment_duration + checking_balance + percent_of_income + months_loan_duration + other_credit + existing_loans_count + purpose + amount, data = data_train, method = "class", control = rpart.control(minsplit = 28, cp =0.001))
fancyRpartPlot(my_tree)
my_prediction <- predict(my_tree,data_test,type="class")
confusionMatrix(my_prediction, data_test[["default"]])

set.seed(1000)
data_train <- sample_frac(credits, .8)
data_test <- setdiff(credits, data_train)
table(data_train$default)
my_tree <-  rpart(default ~ ., data = data_train, method = "class", control = rpart.control(minsplit = 28, cp =0.001))
fancyRpartPlot(my_tree)
my_prediction <- predict(my_tree,data_test,type="class")
confusionMatrix(my_prediction, data_test[["default"]])


set.seed(1015)
data_train <- sample_frac(credits, .7)
data_test <- setdiff(credits, data_train)
table(data_train$default)
my_tree <-  rpart(default ~ credit_history + age + savings_balance + employment_duration + checking_balance + percent_of_income + months_loan_duration + other_credit + existing_loans_count + purpose + amount, data = data_train, method = "class", control = rpart.control(minsplit = 25, cp =.001))
fancyRpartPlot(my_tree)
my_prediction <- predict(my_tree,data_test,type="class")
confusionMatrix(my_prediction, data_test[["default"]])

set.seed(150)
data_train <- sample_frac(credits, .8)
data_test <- setdiff(credits, data_train)
table(data_train$default)
my_tree <-  rpart(default ~ credit_history + age + savings_balance + employment_duration + checking_balance + percent_of_income + months_loan_duration + other_credit + existing_loans_count + purpose + amount, data = data_train, method = "class", control = rpart.control(minsplit = 28, cp =.001))
fancyRpartPlot(my_tree)
my_prediction <- predict(my_tree,data_test,type="class")
confusionMatrix(my_prediction, data_test[["default"]])
