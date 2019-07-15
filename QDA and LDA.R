## QDA and LDA

library(tidyverse)
library(ggplot2)
library(dslabs)
library(purrr)
library(pdftools)
library(lubridate)
library(broom)
library(caret)


set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_index <- createDataPartition(tissue_gene_expression$y, times = 1, list = FALSE)
train_set = x[train_index]
test_set = x[-train_index]

## LDA


train_lda <- train(x, y,
                   method = "lda",
                   data = train_set,
                   preProcess = "center")

means <- data.frame(t(train_lda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("LDA Means - Cerebellum vs Hippocampus") +
  geom_point()

## QDA


train_qda <- train(x, y,
                   method = "qda",
                   data = train_set)

means <- data.frame(t(train_qda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("QDA Means - Cerebellum vs Hippocampus") +
  geom_point()



## More conditionals (LDA)

set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_index <- createDataPartition(tissue_gene_expression$y, times = 1, list = FALSE)
train_set = x[train_index]
test_set = x[-train_index]

train_lda <- train(x, y,
                   method = "lda",
                   data = train_set)
