## Bootstrap

library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)
library(broom)
library(dslabs)
library(caret)


set.seed(1)
n <- 100
B <- 10000
y <- rnorm(n, 0, 1)
Ms <- replicate(B, {
  y <- rnorm(n, 0, 1)
  y_star <- sample(y, 100, replace = TRUE)
  M_star <- quantile(y_star, 0.75)
})

mean(Ms)
sd(Ms)


B <- 10000
set.seed(1)
Q_stars <- replicate(B, {
  y_star <- sample(y, replace = T)
  Q_star <- quantile(y_star, 0.75)
})
mean(Q_stars)
sd(Q_stars)


set.seed(1)
ind10 <- createResample(y, 10000, list = FALSE)
set.seed(1)
df10 <- as.data.frame(as.table(ind10))
df10 <- df10 %>% mutate(y = y[ind10])
Q_stars <- df10 %>% group_by(Var2) %>% summarize(Q_star = quantile(y, 0.75))

mean(Q_stars$Q_star)
sd(Q_stars$Q_star)
