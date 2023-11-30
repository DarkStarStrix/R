# analyze the data from mtcars dataset

# load the data
data(mtcars)

# create a new variable
mtcars$mpg01 <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 0)

# create a new variable
mtcars$am01 <- ifelse(mtcars$am == 1, 1, 0)

# do an exploratory analysis of the data
summary(mtcars)

# create a logistic regression model
model <- glm(mpg01 ~ am01, data = mtcars, family = "binomial")

# show the model
summary(model)

# plot the model
plot(model)

# create a new data frame
newdata <- data.frame(am01 = 1)

# plot the number of cylinders
plot(mtcars$cyl, mtcars$mpg01)

# compare the number of gears to the number of cylinders colored by the number of am01
plot(mtcars$gear, mtcars$cyl, col = mtcars$am01)

mtcars$am01 <- as.factor(mtcars$am01)

mtcars <- mtcars[, c("mpg01", "am01", "cyl", "gear")]

mtcars %>%
  ggplot(aes(x = cyl, y = gear, color = am01)) +
  geom_point()

# create a new data frame
newdata <- data.frame(am01 = 1, cyl = 6, gear = 4)

# predict the probability of mpg01
predict(model, newdata, type = "response")

data %>%
  ggplot(aes(x = cyl, y = gear, color = am01)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# make a decision tree
library(rpart)

# create a decision tree
tree <- rpart(mpg01 ~ am01 + cyl + gear, data = mtcars, method = "class")

# plot the tree
plot(tree)

# plot the tree with the explain tree
plot(tree, uniform = TRUE, main = "Classification Tree for mpg01")
text(tree, use.n = TRUE, all = TRUE, cex = .8)

mtcars %>%
ggplot(aes(x = cyl, y = as.numeric(am01), color = am01)) +
geom_point() +
geom_smooth(method = "glm", method.args = list(family = "binomial")) +
geom_vline(xintercept = 6.5) +
geom_hline(yintercept = 0.5)

# compute the accuracy of the model
mean(mtcars$mpg01 == predict(tree, type = "class"))

# create a confusion matrix
table(mtcars$mpg01, predict(tree, type = "class"))

# create a random forest
library(randomForest)

# create a random forest
forest <- randomForest(mpg01 ~ am01 + cyl + gear, data = mtcars)

# plot the random forest
plot(forest)

# compute the accuracy of the random forest
mean(mtcars$mpg01 == predict(forest, type = "class"))

# create a confusion matrix
table(mtcars$mpg01, predict(forest, type = "class"))

# plot confusion matrix
library(caret)

confusionMatrix <- function(actual, predicted) {
  tab <- table(actual, predicted)
  acc <- sum(diag(tab)) / sum(tab)
  print(acc)
  print(tab)
}

# create a confusion matrix
confusionMatrix(mtcars$mpg01, predict(forest, type = "class"))

# plot the confusion matrix
plot(confusionMatrix(mtcars$mpg01, predict(forest, type = "class")))

mtcars %>%
  ggplot(aes(x = cyl, y = as.numeric(am01), color = am01)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_vline(xintercept = 6.5) +
  geom_hline(yintercept = 0.5)

mtcars %>%
  select(hp, cyl, mpg01) %>%
    ggplot(aes(x = hp, y = mpg01, color = cyl)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    geom_vline(xintercept = 6.5) +
    geom_hline(yintercept = 0.5)

# create a new variable
mtcars$hp01 <- ifelse(mtcars$hp > median(mtcars$hp), 1, 0)

model <- glm(hp01 ~ am01 + cyl + gear, data = mtcars, family = "binomial")
