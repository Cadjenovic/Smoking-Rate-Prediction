library(caret)
library(neuralnet)
library(dplyr)
library(corrplot)

data <- read.csv("final-data.csv")


numeric.data <- numeric.data[,-c(24:27)]
str(numeric.data)
numeric.data <- data[,-c(1,2)]
numeric.data$Population.in.thousands..2017. <- as.numeric(numeric.data$Population.in.thousands..2017.)
numeric.data$GDP..Gross.domestic.product..million.current.US.. <- as.numeric(numeric.data$GDP..Gross.domestic.product..million.current.US..)

cols.with.na <- colnames(numeric.data)[ apply(numeric.data, 2, anyNA)]
cols.with.na



median.missing <- apply(numeric.data[,colnames(numeric.data) %in% cols.with.na], 2, median, na.rm = T)


numeric.data <- numeric.data %>%
  mutate(Employment..Industry....of.employed. = ifelse(is.na(Employment..Industry....of.employed.), median.missing[1], Employment..Industry....of.employed.), 
         Employment..Services....of.employed. = ifelse(is.na(Employment..Services....of.employed.), median.missing[2], Employment..Services....of.employed.),
         Unemployment....of.labour.force. = ifelse(is.na(Unemployment....of.labour.force.), median.missing[3], Unemployment....of.labour.force.),
         Labour.force.participation..female.male.pop.... = ifelse(is.na(Labour.force.participation..female.male.pop....), median.missing[4], Labour.force.participation..female.male.pop....),
         Population.growth.rate..average.annual... = ifelse(is.na(Population.growth.rate..average.annual...), median.missing[5], Population.growth.rate..average.annual...),
         Urban.population.growth.rate..average.annual... = ifelse(is.na(Urban.population.growth.rate..average.annual...), median.missing[6], Urban.population.growth.rate..average.annual...),
         Education..Government.expenditure....of.GDP. = ifelse(is.na(Education..Government.expenditure....of.GDP.), median.missing[7], Education..Government.expenditure....of.GDP.),
         Education..Primary.gross.enrol..ratio..f.m.per.100.pop.. = ifelse(is.na(Education..Primary.gross.enrol..ratio..f.m.per.100.pop..), median.missing[8], Education..Primary.gross.enrol..ratio..f.m.per.100.pop..),
         Education..Secondary.gross.enrol..ratio..f.m.per.100.pop.. = ifelse(is.na(Education..Secondary.gross.enrol..ratio..f.m.per.100.pop..), median.missing[9], Education..Secondary.gross.enrol..ratio..f.m.per.100.pop..),
         Education..Tertiary.gross.enrol..ratio..f.m.per.100.pop.. = ifelse(is.na(Education..Tertiary.gross.enrol..ratio..f.m.per.100.pop..), median.missing[10], Education..Tertiary.gross.enrol..ratio..f.m.per.100.pop..))




numeric.data <- data.frame(
  sapply(
    numeric.data,
    function(x) ifelse(is.na(x),
                       median(x, na.rm = TRUE),
                       x)))




length(which(complete.cases(numeric.data.replace)==F))

        

str(data)
str(numeric.data)


length(which(complete.cases(numeric.data)==F))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

numeric.data <- as.data.frame(lapply(numeric.data, normalize))

str(numeric.data)

vis.data <- numeric.data[,-c(24:34)]
str(vis.data)

corr.mat <- cor(vis.data)
corrplot.mixed(corr.mat, tl.pos = 'n')


















