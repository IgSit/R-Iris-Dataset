library(readr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(ggthemes)
library(GGally)
library(corrplot)
library(purrr)
library(hrbrthemes)
library(raster)
library(tidyverse)
library(viridis)
library(reshape2)
library(moments) 

iris <- read_csv("C:/Users/sigor/Downloads/iris.csv")

# czyszczenie danych
rowSums(is.na(iris))

#podstawowe histogramy (rozk³ady cech wg gatunków)
ggplot(iris, aes(petal.length, fill=variety)) + geom_histogram(binwidth = 0.1)
ggplot(iris, aes(petal.width, fill=variety)) + geom_histogram(binwidth = 0.06)
ggplot(iris, aes(sepal.length, fill=variety)) + geom_histogram(binwidth = 0.07)
ggplot(iris, aes(sepal.width, fill=variety)) + geom_histogram(binwidth = 0.05)

#podsumowanie podstawowych info dla ca³ego zbioru
summary(iris)

setosa <- iris[1:50,]
versicolor <- iris[51:100,]
virginica <- iris[101:150,]

# a tu z podzia³em na gatunki

Name <- c("All", "Setosa", "Versicolor", "Virginica")
Min <- c(min(iris$sepal.length), min(setosa$sepal.length), min(versicolor$sepal.length), min(virginica$sepal.length))
Max <- c(max(iris$sepal.length), max(setosa$sepal.length), max(versicolor$sepal.length), max(virginica$sepal.length))
Mean <- c(mean(iris$sepal.length), mean(setosa$sepal.length), mean(versicolor$sepal.length), mean(virginica$sepal.length))
First_Quantile <- c(quantile(iris$sepal.length, 0.25), quantile(setosa$sepal.length, 0.25), quantile(versicolor$sepal.length, 0.25), quantile(virginica$sepal.length, 0.25))
Median <- c(quantile(iris$sepal.length, 0.5), quantile(setosa$sepal.length, 0.5), quantile(versicolor$sepal.length, 0.5), quantile(virginica$sepal.length, 0.5))
Third_Quantile <- c(quantile(iris$sepal.length, 0.75), quantile(setosa$sepal.length, 0.75), quantile(versicolor$sepal.length, 0.75), quantile(virginica$sepal.length, 0.75))
Variance <- c(var(iris$sepal.length), var(setosa$sepal.length), var(versicolor$sepal.length), var(virginica$sepal.length))
Std_Deviation <- c(sd(iris$sepal.length), sd(setosa$sepal.length), sd(versicolor$sepal.length), sd(virginica$sepal.length))
Skewness <- c(skewness(iris$sepal.length), skewness(setosa$sepal.length), skewness(versicolor$sepal.length), skewness(virginica$sepal.length))
Kurtosis <- c(kurtosis(iris$sepal.length), kurtosis(setosa$sepal.length), kurtosis(versicolor$sepal.length), kurtosis(virginica$sepal.length))
basicStatsSL <- data.frame(Name, Min, Max, Mean, First_Quantile, Median, Third_Quantile, Variance, Std_Deviation, Skewness, Kurtosis)

Name <- c("All", "Setosa", "Versicolor", "Virginica")
Min <- c(min(iris$sepal.width), min(setosa$sepal.width), min(versicolor$sepal.width), min(virginica$sepal.width))
Max <- c(max(iris$sepal.width), max(setosa$sepal.width), max(versicolor$sepal.width), max(virginica$sepal.width))
Mean <- c(mean(iris$sepal.width), mean(setosa$sepal.width), mean(versicolor$sepal.width), mean(virginica$sepal.width))
First_Quantile <- c(quantile(iris$sepal.width, 0.25), quantile(setosa$sepal.width, 0.25), quantile(versicolor$sepal.width, 0.25), quantile(virginica$sepal.width, 0.25))
Median <- c(quantile(iris$sepal.width, 0.5), quantile(setosa$sepal.width, 0.5), quantile(versicolor$sepal.width, 0.5), quantile(virginica$sepal.width, 0.5))
Third_Quantile <- c(quantile(iris$sepal.width, 0.75), quantile(setosa$sepal.width, 0.75), quantile(versicolor$sepal.width, 0.75), quantile(virginica$sepal.width, 0.75))
Variance <- c(var(iris$sepal.width), var(setosa$sepal.width), var(versicolor$sepal.width), var(virginica$sepal.width))
Std_Deviation <- c(sd(iris$sepal.width), sd(setosa$sepal.width), sd(versicolor$sepal.width), sd(virginica$sepal.width))
Skewness <- c(skewness(iris$sepal.width), skewness(setosa$sepal.width), skewness(versicolor$sepal.width), skewness(virginica$sepal.width))
Kurtosis <- c(kurtosis(iris$sepal.width), kurtosis(setosa$sepal.width), kurtosis(versicolor$sepal.width), kurtosis(virginica$sepal.width))
basicStatsSW <- data.frame(Name, Min, Max, Mean, First_Quantile, Median, Third_Quantile, Variance, Std_Deviation, Skewness, Kurtosis)

Name <- c("All", "Setosa", "Versicolor", "Virginica")
Min <- c(min(iris$petal.length), min(setosa$petal.length), min(versicolor$petal.length), min(virginica$petal.length))
Max <- c(max(iris$petal.length), max(setosa$petal.length), max(versicolor$petal.length), max(virginica$petal.length))
Mean <- c(mean(iris$petal.length), mean(setosa$petal.length), mean(versicolor$petal.length), mean(virginica$petal.length))
First_Quantile <- c(quantile(iris$petal.length, 0.25), quantile(setosa$petal.length, 0.25), quantile(versicolor$petal.length, 0.25), quantile(virginica$petal.length, 0.25))
Median <- c(quantile(iris$petal.length, 0.5), quantile(setosa$petal.length, 0.5), quantile(versicolor$petal.length, 0.5), quantile(virginica$petal.length, 0.5))
Third_Quantile <- c(quantile(iris$petal.length, 0.75), quantile(setosa$petal.length, 0.75), quantile(versicolor$petal.length, 0.75), quantile(virginica$petal.length, 0.75))
Variance <- c(var(iris$petal.length), var(setosa$petal.length), var(versicolor$petal.length), var(virginica$petal.length))
Std_Deviation <- c(sd(iris$petal.length), sd(setosa$petal.length), sd(versicolor$petal.length), sd(virginica$petal.length))
Skewness <- c(skewness(iris$petal.length), skewness(setosa$petal.length), skewness(versicolor$petal.length), skewness(virginica$petal.length))
Kurtosis <- c(kurtosis(iris$petal.length), kurtosis(setosa$petal.length), kurtosis(versicolor$petal.length), kurtosis(virginica$petal.length))
basicStatsPL <- data.frame(Name, Min, Max, Mean, First_Quantile, Median, Third_Quantile, Variance, Std_Deviation, Skewness, Kurtosis)

Name <- c("All", "Setosa", "Versicolor", "Virginica")
Min <- c(min(iris$petal.width), min(setosa$petal.width), min(versicolor$petal.width), min(virginica$petal.width))
Max <- c(max(iris$petal.width), max(setosa$petal.width), max(versicolor$petal.width), max(virginica$petal.width))
Mean <- c(mean(iris$petal.width), mean(setosa$petal.width), mean(versicolor$petal.width), mean(virginica$petal.width))
First_Quantile <- c(quantile(iris$petal.width, 0.25), quantile(setosa$petal.width, 0.25), quantile(versicolor$petal.width, 0.25), quantile(virginica$petal.width, 0.25))
Median <- c(quantile(iris$petal.width, 0.5), quantile(setosa$petal.width, 0.5), quantile(versicolor$petal.width, 0.5), quantile(virginica$petal.width, 0.5))
Third_Quantile <- c(quantile(iris$petal.width, 0.75), quantile(setosa$petal.width, 0.75), quantile(versicolor$petal.width, 0.75), quantile(virginica$petal.width, 0.75))
Variance <- c(var(iris$petal.width), var(setosa$petal.width), var(versicolor$petal.width), var(virginica$petal.width))
Std_Deviation <- c(sd(iris$petal.width), sd(setosa$petal.width), sd(versicolor$petal.width), sd(virginica$petal.width))
Skewness <- c(skewness(iris$petal.width), skewness(setosa$petal.width), skewness(versicolor$petal.width), skewness(virginica$petal.width))
Kurtosis <- c(kurtosis(iris$petal.width), kurtosis(setosa$petal.width), kurtosis(versicolor$petal.width), kurtosis(virginica$petal.width))
basicStatsPW <- data.frame(Name, Min, Max, Mean, First_Quantile, Median, Third_Quantile, Variance, Std_Deviation, Skewness, Kurtosis)

iris_long <- melt(iris)

#dwa boxploty, jeden dla ca³ego zbioru dla wszystkich cech, drugi grupuj¹cy wg gatunków
ggplot(iris_long, aes(x = variable, y = value)) +
  geom_boxplot(color="orange", fill="yellow", alpha=0.4) + 
  geom_jitter(color="orange", position = position_jitter(0.2), alpha=0.3)
  theme(legend.position = "none")

ggplot(iris_long, aes(x = variable, y = value, color = variety)) +
  geom_boxplot()

#gestoœci
ggplot(iris, aes(x=petal.length, group=variety, fill=variety)) +
  geom_density(adjust=1.5, alpha=.4)
ggplot(iris, aes(x=petal.width, group=variety, fill=variety)) +
  geom_density(adjust=1.5, alpha=.4)
ggplot(iris, aes(x=sepal.length, group=variety, fill=variety)) +
  geom_density(adjust=1.5, alpha=.4)
ggplot(iris, aes(x=sepal.width, group=variety, fill=variety)) +
  geom_density(adjust=1.5, alpha=.4)

# wszystko, co nam potrzebne
ggpairs(iris, aes(colour = variety, alpha = 0.4)) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#macierz korelacji
corrplot(cor(iris[,1:4]), method = 'number')

# wykres PL ~ PW
ggplot(iris, aes(x=petal.width, y=petal.length)) + 
  geom_point(color="orange") + 
  geom_smooth(method = 'lm', formula = y ~ x)

#wykres PL ~ SL
ggplot(iris, aes(x=sepal.length, y=petal.length)) + 
  geom_point(color="orange") + 
  geom_smooth(method = 'lm', formula = y ~ x)

#œrednia i wariancja again
Name <- c("All", "Setosa", "Versicolor", "Virginica")
Petal.length_Mean <- c(mean(iris$petal.length), mean(setosa$petal.length), 
                       mean(versicolor$petal.length), 
                       mean(virginica$petal.length))
Petal.width_Mean <- c(mean(iris$petal.width), mean(setosa$petal.width), 
                      mean(versicolor$petal.width), 
                      mean(virginica$petal.width))
Sepal.length_Mean <- c(mean(iris$sepal.length), mean(setosa$sepal.length), 
                       mean(versicolor$sepal.length), 
                       mean(virginica$sepal.length))
Sepal.width_Mean <- c(mean(iris$sepal.width), mean(setosa$sepal.width), 
                      mean(versicolor$sepal.width), 
                      mean(virginica$sepal.width))
Petal.length_Var <- c(var(iris$petal.length), var(setosa$petal.length), 
                       var(versicolor$petal.length), 
                       var(virginica$petal.length))
Petal.width_Var <- c(var(iris$petal.width), var(setosa$petal.width), 
                      var(versicolor$petal.width), 
                      var(virginica$petal.width))
Sepal.length_Var <- c(var(iris$sepal.length), var(setosa$sepal.length), 
                       var(versicolor$sepal.length), 
                       var(virginica$sepal.length))
Sepal.width_Var <- c(var(iris$sepal.width), var(setosa$sepal.width), 
                      var(versicolor$sepal.width), 
                      var(virginica$sepal.width))

MeanVar <- data.frame(Name, Petal.length_Mean, Petal.width_Mean, 
                      Sepal.length_Mean, Sepal.width_Mean,
                      Petal.length_Var, Petal.width_Var, 
                      Sepal.length_Var, Sepal.width_Var)

#QQ-ploty
ggplot(data.frame(d = rnorm(1000)), aes(sample=d)) + stat_qq()
ggplot(data.frame(d = iris$sepal.length), aes(sample=d)) + stat_qq()
ggplot(data.frame(d = setosa$sepal.length), aes(sample=d)) + stat_qq()
ggplot(data.frame(d = versicolor$sepal.length), aes(sample=d)) + stat_qq()
ggplot(data.frame(d = virginica$sepal.length), aes(sample=d)) + stat_qq()

#test
shapiro.test(sample(iris$Sepal.Length, 50))

#wykres gêstoœci dla Iris sepal.length
ggplot(iris, aes(x=sepal.length, color="orange", fill="orange")) +
    geom_density(adjust=1.5, alpha=.4) +
    theme(legend.position = "none")