library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)
library(broom)
library(dslabs)
dat <- mnist_27
# Creates 100 by 10 matrix of randomly generated normal numbers
x <- matrix(rnorm(100*10), 100, 10)
dim(x)
nrow(x)
ncol(x)

# Scale each ROW of a matrix
(x - rowMeans(x)) / rowSds(x)

# ^^ Does not work for columns only rows
# For columns, transpose the matrix

t(t(x) - colMeans(x))

X_mean_0 <- sweep(x, 2, colMeans(x))




## Read Data
mnist <- read_mnist()
## Select appropriate columns
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
## draw a boxplot showing where most ink hits the paper
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")

## Binarize the data
new_y <- y
new_y[new_y < 50 & new_y > 205] <- 0
new_y[new_y > 50 & new_y < 205] <- 1

qplot(as.factor(mnist$train$labels), new_y, geom = "boxplot")
## This proportion of pixels in the grey area between 50 and 205
mean(new_y)