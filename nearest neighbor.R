## nearest neighbor

library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)
library(broom)
library(dslabs)
library(caret)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
mnist_27$train
d <- dist(tissue_gene_expression$x)
d

dist(tissue_gene_expression$x[c(1,2)])
dist(tissue_gene_expression$x[c(39,40)])
dist(tissue_gene_expression$x[c(73,74)])

dist(tissue_gene_expression$x[c(1,39)])
dist(tissue_gene_expression$x[c(1,73)])
dist(tissue_gene_expression$x[c(2,74)])

image(as.matrix(d))

fit_glm <- glm(y ~ x_1, x_2, data=mnist_27$train, family = "binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]


library(dslabs)

data(heights)

ks<-seq(1,101,3)


y<-heights$sex

x<- heights$height



test_index<- createDataPartition(y, times=1, p=0.5, list=FALSE)

train_set<- heights[test_index,]

test_set<- heights[-test_index,]

accuracy <- map_df(ks, function(k) {
  set.seed(1)
  fit<- knn3(sex~height, data=train_set, k=k)
  
  y_hat<-predict(fit, test_set, type="class")
  
  F_val<-F_meas(data=y_hat, reference=factor(test_set$sex))
  
  list(k=k, F_val=F_val)
  
})

accuracy

accuracy %>% ggplot(aes(k,F_val)) + geom_line()

ks[which.max(accuracy$F_val)]
max(accuracy$F_val)


library(dslabs)
data("tissue_gene_expression")

ks<-seq(1,11,2)

x <- tissue_gene_expression$x

y <- tissue_gene_expression$y

train_index <- createDataPartition(tissue_gene_expression$y, times = 1, list = FALSE)
train_set = x[train_index,]
test_set = x[-train_index,]
train_set_y = y[train_index]
test_set_y = y[-train_index]
set.seed(1)
accuracy <- map_df(ks, function(k){
  
  knn_fit <- knn3(train_set,train_set_y, k = k)
  y_hat <- predict(knn_fit, test_set, type = "class")
  test_error<- confusionMatrix(data = y_hat, reference = test_set_y, mode = "everything")$overall["Accuracy"]
  list(k=k,test=test_error)
})


accuracy

