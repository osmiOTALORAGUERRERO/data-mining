#install.packages('dplyr')
library(dplyr)

iris %>%
  group_by(Species)  %>%
  summarise(media.sepal.length = mean(Sepal.Length), 
            sd.sepal.length=sd(Sepal.Length))

hist(iris$Sepal.Length, 
     main = 'Histogram Sepal length', 
     xlab = 'length',
     col = 'grey')

boxplot(Sepal.Length~Species, 
        data=iris, 
        main='Sepal length iris data', 
        col='green')
