#install.packages('dplyr')
library(dplyr)

#Media y desviacion estandar
iris %>%
  group_by(Species)  %>%
  summarise(media.sepal.length = mean(Sepal.Length),
            sd.sepal.length = sd(Sepal.Length),
            sd.sepal.width = sd(Sepal.Width),
            sd.sepal.Size =sd(Sepal.Length*Sepal.Width))

#histograma longitud sepalo
hist(iris$Sepal.Length, 
     main = 'Histogram Sepal length', 
     xlab = 'length',
     col = 'grey')

#boxplot longitud sepalo por especies
boxplot(Sepal.Length~Species, 
        data=iris, 
        main='Sepal length iris data', 
        col='green')

#histograma longitud Petalo 
hist(iris$Petal.Length, 
     main = 'Histogram Petal length', 
     xlab = 'length',
     col = 'grey')

#Boxpplot ancho petalo por especies
boxplot(Petal.Width~Species, 
        data=iris, 
        main='Petal width iris data', 
        col='green')

#---------------------------------------------
#Procedimiento para allar el top 5 de los mas extensos
#crear nuevo data frame
iris_top <- iris %>%
  transmute(Petal.Size = Petal.Length*Petal.Width,
            Sepal.Size = Sepal.Length*Sepal.Width,
            Species = Species)

#Hallo los 5 primeros ordenadolos por petalo sepalo
setosa <- head(iris_top %>% 
                 filter(Species %in% c("setosa")) %>%
                 arrange(-Petal.Size, -Sepal.Size),5)


virginica <- head(iris_top %>% 
                    filter(Species %in% c("virginica")) %>% 
                    arrange(-Petal.Size, -Sepal.Size),5)


versicolor <- head(iris_top %>% 
                     filter(Species %in% c("versicolor")) %>% 
                     arrange(-Petal.Size, -Sepal.Size), 5)

#Creo el nuevo dataframe con los valores anteriores
iris_top_5 <- data.frame(Setosa = setosa, 
                         Viriginica = virginica, 
                         Versicolor = versicolor)
