## Regression Trees and Random Forests


library(dplyer)

## Regression Tree
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ .,
             data = dat)
plot(fit)
text(fit)


dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)


## Random Forest

library(randomForest)
fit <- randomForest(y ~ x, 
                    data = dat,
                    nodesize = 50,
                    maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

plot(fit)


# Train function to minimize the estimated RMSE

library(caret)
library(randomForest)
library(Rborist)

n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)


set.seed(1)

fit <- train(y ~ .,
             method = "Rborist",
             tuneGrid = data.frame(predFixed = 1, minNode = seq(25, 100, 25)),
             data = dat)


dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

## Tissue Gene Expression -- rpart


data("tissue_gene_expression")
set.seed(1991)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

fit_rpart <- train(x, y,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0,0.1,0.01)),
                   control = rpart.control(minsplit = 0))

confusionMatrix(fit_rpart)


ggplot(fit_rpart)

fit <- rpart(tissue_gene_expression$y ~ .,
             data = tissue_gene_expression$y)

plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)


## Tissue Gene Expression -- with less than 7 genes using Random Forest

data("tissue_gene_expression")
set.seed(1991)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

grid <- data.frame(mtry = seq(50, 200, 25))

fit_rf <- train(x, y,
                method = "rf",
                tuneGrid = data.frame(mtry = seq(50,200,25)),
                control = rpart.control(minsplit = 0),
                nodesize = 1)

fit_rf$results


## Importance 
imp <- varImp(fit)
imp


tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms



## Case Study: MNIST (Modified National Institue of Standards and Technology database) digits 

library(dslabs)
names(mnist)
mnist <- read_mnist()
dim(mnist$train$images)
class(mnist$train$labels)
table(mnist$train$labels)

set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index, ]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 10000)
x_test <- mnist$train$images[index, ]
y_test <-factor(mnist$train$labels[index])

######### Preprocessing MNIST

library(matrixStats)
sds <- colSds(x)
qplots(sds, bins = 256, color = I("black"))

library(caret)
nzn <- nearZeroVar(x)


## Comprehension Check: Ensembles

models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")


fits <- lapply(models, function(model){
  train(y ~ ., method = models, data = mnist_27$train)
})

length(mnist_27$test$y)
length(models)

## Used accuracy from all of 'fits' to find mean for Q3
## Something like this but not exactly -> ""fits[1]['Accuracy']""

fits[1]



x <- with(tissue_gene_expression, sweep(x, 1, mean(x)))
x <- sweep(x, 1, rowMeans(tissue_gene_expression$x))
x <- tissue_gene_expression$x - mean(tissue_gene_expression$x)
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()
