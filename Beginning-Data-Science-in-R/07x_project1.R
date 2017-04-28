## ---- cache=TRUE, echo=FALSE---------------------------------------------
red <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", header=TRUE, sep=';')
white <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=';')

## ------------------------------------------------------------------------
wines <- rbind(data.frame(type = "red", red),
               data.frame(type = "white", white))

## ------------------------------------------------------------------------
summary(wines)

## ----wine-qualities, warning=FALSE, message=FALSE, fig.cap="Distribution of wine qualities."----
ggplot(wines) + 
  geom_bar(aes(x = factor(quality), fill = type), 
             position = 'dodge') +
  xlab('Quality') + ylab('Frequency')

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(e1071, quietly = TRUE))

## ------------------------------------------------------------------------
random_group <- function(n, probs) {
  probs <- probs / sum(probs)
  g <- findInterval(seq(0, 1, length = n), c(0, cumsum(probs)),
                    rightmost.closed = TRUE)
  names(probs)[sample(g)]
}

partition <- function(df, n, probs) {
  replicate(n, split(df, random_group(nrow(df), probs)), FALSE)
}

## ------------------------------------------------------------------------
accuracy <- function(confusion_matrix)
  sum(diag(confusion_matrix))/sum(confusion_matrix)

prediction_accuracy_wines <- function(test_and_training) {
  result <- vector(mode = "numeric", 
                   length = length(test_and_training))
  for (i in seq_along(test_and_training)) {
    training <- test_and_training[[i]]$training
    test <- test_and_training[[i]]$test
    model <- training %>% naiveBayes(type ~ ., data = .)
    predictions <- test %>% predict(model, newdata = .)
    targets <- test$type
    confusion_matrix <- table(targets, predictions)
    result[i] <- accuracy(confusion_matrix)
  }
  result
}

## ------------------------------------------------------------------------
random_wines <- wines %>% 
    partition(4, c(training = 0.5, test = 0.5))
random_wines %>% prediction_accuracy_wines

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(party, quietly = TRUE))

## ---- message=FALSE, fig.cap="Decision tree for determining the type of wine."----
tree <- ctree(type ~ ., data = wines, 
              control = ctree_control(minsplit = 4420))

## ------------------------------------------------------------------------
wines %>%
  group_by(type) %>%
  summarise(total.mean = mean(total.sulfur.dioxide),
            total.sd = sd(total.sulfur.dioxide),
            free.mean = mean(free.sulfur.dioxide),
            free.sd = sd(free.sulfur.dioxide))

## ---- fig.cap="Sulfur dioxide versis volatile acidity."------------------
qplot(total.sulfur.dioxide, volatile.acidity, data=wines,
      color = type,
      xlab = 'Total sulfur dioxide', 
      ylab = 'Volatile acidity (VA)')

## ------------------------------------------------------------------------
wines %>%
  group_by(type) %>%
  summarise(mean = mean(volatile.acidity),
            sd = sd(volatile.acidity))

## ------------------------------------------------------------------------
rmse <- function(x,t) sqrt(mean(sum((t - x)^2)))

null_prediction <- function(df) {
    rep(mean(wines$quality), each = nrow(df))
}

rmse(null_prediction(wines), wines$quality)

## ------------------------------------------------------------------------
prediction_accuracy_wines <- function(test_and_training,
                                      model_function) {
  result <- vector(mode = "numeric", 
                   length = length(test_and_training))
  for (i in seq_along(test_and_training)) {
    training <- test_and_training[[i]]$training
    test <- test_and_training[[i]]$test
    model <- training %>% model_function(quality ~ ., data = .)
    predictions <- test %>% predict(model, newdata = .)
    targets <- test$quality
    result[i] <- rmse(predictions, targets)
  }
  result
}

## ------------------------------------------------------------------------
null_model <- function(formula, data) {
    structure(list(mean = mean(data$quality)),
              class = "null_model")
}

predict.null_model <- function(model, newdata) {
    rep(model$mean, each = nrow(newdata))
}

## ------------------------------------------------------------------------
test_and_training <- wines %>% 
    partition(4, c(training = 0.5, test = 0.5))
test_and_training %>% prediction_accuracy_wines(null_model)

## ------------------------------------------------------------------------
test_and_training %>% prediction_accuracy_wines(lm)

