library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)
library(broom)
library(dslabs)



mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
fit <- loess(as.numeric(y) ~ x_2, degree = 1, span = .5, data = mnist_27$train)
mnist_27$train %>% glm(y ~ fit$fitted, family = "binomial", data = .) %>% tidy()


mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")


# Is predictive, non-linear