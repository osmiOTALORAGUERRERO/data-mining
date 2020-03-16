# Breast cancer winsconsin diagnostic
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)

#install_github("kassambara/easyGgplot2")
library(dplyr)
library(devtools)
library(easyGgplot2)
library(MASS)
library(corrplot)
library(GGally)
library(caret)


#Datas summary
str(wdbc)
summary(wdbc)
colnames(wdbc)
head(wdbc)

#
#Data clean
#

wdbc <- dplyr::select(wdbc, -X)

# missing values
nas <- wdbc %>%
  is.na.data.frame() %>%
  table()
nas
wdbc <- wdbc %>%
  distinct()
nas

# data duplicated
duplicates <- wdbc %>%
  duplicated() %>%
  table()
duplicates
wdbc <- wdbc %>%
  distinct()
duplicates

# imbalance
diagnosis_y <- prop.table(table(wdbc$diagnosis))
cat("% Benign", round(diagnosis_y[[1]]*100, 2), '%\n',
'% Malignant', round(diagnosis_y[[2]] * 100, 2), '%\n',
'Diferencia de datos con Benign',
    nrow(wdbc[wdbc$diagnosis == "B",])-nrow(wdbc[wdbc$diagnosis == "M",]),
    "registros de mas, con un porcentaje de desbalance de",
    round(diagnosis_y[[1]]*100, 2)-round(diagnosis_y[[2]] * 100, 2),"%")

# B - M
ggplot2.barplot(data = wdbc, mainTitle="cases benign and malignant",  xName = "diagnosis", ytitle = "cancer cases", ylim=c(0,400))

#correlacion de pearson
pearson_cor <- cor(dplyr::select(wdbc, -id, -diagnosis), method = "pearson")
View(pearson_cor)
corrplot.mixed(pearson_cor)

#Grafico correlacion lineal
ggpairs(dplyr::select(wdbc, -id, -diagnosis), title="correlogram lineal", lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))
ggpairs(dplyr::select(wdbc, -id), title="correlogram lineal", ggplot2::aes(colour=diagnosis),lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))

#Graficos distribucion de las variables
newNames = c(
  "fractal_dimension_mean",  "fractal_dimension_se", "fractal_dimension_worst",
  "symmetry_mean", "symmetry_se", "symmetry_worst",
  "concave.points_mean", "concave.points_se", "concave.points_worst",
  "concavity_mean","concavity_se", "concavity_worst",
  "compactness_mean", "compactness_se", "compactness_worst",
  "smoothness_mean", "smoothness_se", "smoothness_worst",
  "area_mean", "area_se", "area_worst",
  "perimeter_mean",  "perimeter_se", "perimeter_worst",
  "texture_mean" , "texture_se", "texture_worst",
  "radius_mean", "radius_se", "radius_worst"
)

bc.data = (wdbc[,newNames])
bc.diag = wdbc[,2]

scales <- list(x=list(relation="free"),y=list(relation="free"), cex=0.6)
featurePlot(x=bc.data, y=bc.diag, plot="box",scales=scales,
            layout = c(6,5), auto.key = list(columns = 2))

featurePlot(x=bc.data, y=bc.diag, plot="density",scales=scales,
            layout = c(6,5), auto.key = list(columns = 2),dark.theme = T)

featurePlot(x=bc.data, y=bc.diag, plot="strip",scales=scales,
            layout = c(6,5), auto.key = list(columns = 2),dark.theme = T)

featurePlot(x=bc.data, y=bc.diag, plot="pairs",scales=scales,
            layout = c(6,5), auto.key = list(columns = 2),dark.theme = T)

featurePlot(x=bc.data, y=bc.diag, plot="ellipse",scales=scales,
            layout = c(6,5), auto.key = list(columns = 2),dark.theme = T)

featurePlot(x=bc.data, y=bc.diag, plot="scatter",scales=scales,
            layout = c(6,5), auto.key = list(columns = 2),dark.theme = T)
