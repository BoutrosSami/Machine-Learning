library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)
library(broom)
library(dslabs)
library(caret)

library(genefilter)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

tt <- colttests(x, y)

pvals <- tt$p.value

ind <- which(pvals <= 0.01)
x_subset <- x[,ind]

fit <- train(x_subset, y, method = "glm")
fit$results

count <- 0
i <- 1
for (val in ind){
  if (ind[i] == TRUE)
    count <- count + 1
  i <- i + 1
}
print(count)

ind <- which(pvals <= 0.01)
x_subset <- x[,ind]

fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)
