library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
iris$Species<- droplevels(iris$Species)
y <- iris$Species
x <- iris$Petal.Width
z <- iris$Petal.Length

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]


y_hat <- ifelse(((x < 1.7) & (z < 5)), "versicolor", "verginica") %>%
  factor(levels = levels(y))

confusionMatrix(y_hat, y)

length(y_hat)


iris %>%
  group_by(Species) %>%
  summarize(mean(Petal.Length), sd(Petal.Length))