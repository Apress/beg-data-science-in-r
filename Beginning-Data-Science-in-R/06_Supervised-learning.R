## ------------------------------------------------------------------------
cars %>% head

## ----dist-vs-speed, fig.cap="Plot of breaking distance versus speed for cars."----
cars %>% ggplot(aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm")

## ------------------------------------------------------------------------
cars %>% lm(dist ~ speed, data = .) %>% summary

## ------------------------------------------------------------------------
cars %>% lm(dist ~ speed, data = .) %>% coefficients
cars %>% lm(dist ~ speed, data = .) %>% confint

## ----lin-regress-choices-of-thetas, fig.cap="Prediction lines for different choices of parameters."----
predict_dist <- function(speed, theta_1) 
  data.frame(speed = speed, 
             dist = theta_1 * speed, 
             theta = as.factor(theta_1))

cars %>% ggplot(aes(x = speed, y = dist, colour = theta)) +
  geom_point(colour = "black") +
  geom_line(data = predict_dist(cars$speed, 2)) +
  geom_line(data = predict_dist(cars$speed, 3)) +
  geom_line(data = predict_dist(cars$speed, 4)) +
  scale_color_discrete(name=expression(theta[1]))

## ----lin-regress-errors-for-thetas, fig.cap="Error values for different choices of parameters."----
thetas <- seq(0, 5, length.out = 50)
fitting_error <- Vectorize(function(theta) 
  sum((theta * cars$speed - cars$dist)**2)
)

data.frame(thetas = thetas, errors = fitting_error(thetas)) %>%
  ggplot(aes(x = thetas, y = errors)) +
  geom_line() +
  xlab(expression(theta[1])) + ylab("")

## ------------------------------------------------------------------------
cars %>% lm(dist ~ speed - 1, data = .) %>% coefficients

## ----lin-regress-best-theta, fig.cap="Best regression line going through (0,0)."----
cars %>% ggplot(aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x - 1)

## ------------------------------------------------------------------------
library(mlbench)
data("BreastCancer")
BreastCancer %>% head

## ----breast_cancer_data, fig.cap="Breast cancer class versus clump thickness"----
BreastCancer %>% 
  ggplot(aes(x = Cl.thickness, y = Class)) +
  geom_jitter(height = 0.05, width = 0.3, alpha=0.4)

## ----breast_cancer_logistic_regression, fig.cap="Logistic regression fit to breast cancer data."----
BreastCancer %>% 
  mutate(Cl.thickness.numeric = 
                as.numeric(as.character(Cl.thickness))) %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) %>%
  ggplot(aes(x = Cl.thickness.numeric, y = IsMalignant)) +
  geom_jitter(height = 0.05, width = 0.3, alpha=0.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"))

## ---- warnings=FALSE-----------------------------------------------------
BreastCancer %>% 
  mutate(Cl.thickness.numeric = 
                as.numeric(as.character(Cl.thickness))) %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) %>%
  glm(IsMalignant ~ Cl.thickness.numeric, 
      family = "binomial",
      data = .) 

## ------------------------------------------------------------------------
cars %>%
  model.matrix(dist ~ speed, data = .) %>%
  head(5)

## ------------------------------------------------------------------------
cars %>%
  model.matrix(dist ~ speed - 1, data = .) %>%
  head(5)

## ------------------------------------------------------------------------
BreastCancer %>%
  mutate(Cl.thickness.numeric =
           as.numeric(as.character(Cl.thickness)),
         Cell.size.numeric =
           as.numeric(as.character(Cell.size))) %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) %>%
  model.matrix(IsMalignant ~ Cl.thickness.numeric + Cell.size.numeric,
               data = .) %>%
  head(5)

## ------------------------------------------------------------------------
BreastCancer %>%
  mutate(Cl.thickness.numeric =
           as.numeric(as.character(Cl.thickness)),
         Cell.size.numeric =
           as.numeric(as.character(Cell.size))) %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) %>%
  glm(IsMalignant ~ Cl.thickness.numeric + Cell.size.numeric,
      family = "binomial", 
      data = .)

## ------------------------------------------------------------------------
BreastCancer %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) %>%
  model.matrix(IsMalignant ~ Bare.nuclei, data = .) %>%
  head(5)

## ------------------------------------------------------------------------
BreastCancer %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) %>%
  model.matrix(IsMalignant ~ Cl.thickness, data = .) %>%
  head(5)

## ------------------------------------------------------------------------
BreastCancer %>%
  mutate(Cl.thickness.numeric =
           as.numeric(as.character(Cl.thickness)),
         Cell.size.numeric =
           as.numeric(as.character(Cell.size))) %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) %>%
  model.matrix(IsMalignant ~ Cl.thickness.numeric * Cell.size.numeric,
               data = .) %>%
  head(5)

## ------------------------------------------------------------------------
BreastCancer %>%
  mutate(Cl.thickness.numeric =
           as.numeric(as.character(Cl.thickness))) %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) %>%
  model.matrix(IsMalignant ~ Cl.thickness.numeric * Bare.nuclei, data = .) %>%
  head(3)

## ------------------------------------------------------------------------
BreastCancer %>%
  mutate(Cl.thickness.numeric =
           as.numeric(as.character(Cl.thickness))) %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) %>%
  model.matrix(IsMalignant ~ Cl.thickness.numeric : Bare.nuclei, data = .) %>%
  head(3)

## ------------------------------------------------------------------------
cars %>%
  model.matrix(dist ~ speed + speed^2, data = .) %>%
  head

## ------------------------------------------------------------------------
cars %>%
  model.matrix(dist ~ speed + I(speed^2), data = .) %>%
  head

## ------------------------------------------------------------------------
cars %>% lm(dist ~ speed + I(speed^2), data = .) %>%
  summary

## ----polynomial-cars, fig.cap="The cars data fitted to a second degree polynomial."----
cars %>% ggplot(aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))

## ------------------------------------------------------------------------
line <- cars %>% lm(dist ~ speed, data = .)
poly <- cars %>% lm(dist ~ speed + I(speed^2), data = .)

predict(line, cars) %>% head
predict(poly, cars) %>% head

## ------------------------------------------------------------------------
rmse <- function(x,t) sqrt(mean(sum((t - x)^2)))

rmse(predict(line, cars), cars$dist)
rmse(predict(poly, cars), cars$dist)

## ------------------------------------------------------------------------
training_data <- cars[1:25,]
test_data <- cars[26:50,]

line <- training_data %>% lm(dist ~ speed, data = .)
poly <- training_data %>% lm(dist ~ speed + I(speed^2), data = .)

rmse(predict(line, test_data), test_data$dist)
rmse(predict(poly, test_data), test_data$dist)

## ------------------------------------------------------------------------
sampled_cars <- cars %>%
  mutate(training = sample(0:1, nrow(cars), replace = TRUE))

sampled_cars %>% head

## ------------------------------------------------------------------------
training_data <- sampled_cars %>% filter(training == 1)
test_data <- sampled_cars %>% filter(training == 0)

training_data %>% head
test_data %>% head

## ------------------------------------------------------------------------
line <- training_data %>% lm(dist ~ speed, data = .)
poly <- training_data %>% lm(dist ~ speed + I(speed^2), data = .)

rmse(predict(line, test_data), test_data$dist)
rmse(predict(poly, test_data), test_data$dist)

## ------------------------------------------------------------------------
formatted_data <- BreastCancer %>%
  mutate(Cl.thickness.numeric =
           as.numeric(as.character(Cl.thickness)),
         Cell.size.numeric =
           as.numeric(as.character(Cell.size))) %>%
  mutate(IsMalignant = ifelse(Class == "benign", 0, 1)) 

fitted_model <- formatted_data %>%
  glm(IsMalignant ~ Cl.thickness.numeric + Cell.size.numeric, 
      family = "binomial",
      data = .)

## ------------------------------------------------------------------------
predict(fitted_model, formatted_data, type = "response") %>% head

## ------------------------------------------------------------------------
classify <- function(probability) ifelse(probability < 0.5, 0, 1)
classified_malignant <- classify(predict(fitted_model, formatted_data))

## ------------------------------------------------------------------------
table(formatted_data$IsMalignant, classified_malignant)

## ------------------------------------------------------------------------
table(formatted_data$IsMalignant, classified_malignant,
      dnn=c("Data", "Predictions"))

## ------------------------------------------------------------------------
classify <- function(probability)
  ifelse(probability < 0.5, "benign", "malignant")
classified <- classify(predict(fitted_model, formatted_data))

table(formatted_data$Class, classified,
      dnn=c("Data", "Predictions"))

## ------------------------------------------------------------------------
confusion_matrix <- table(formatted_data$Class, classified,
                          dnn=c("Data", "Predictions"))

(accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))

## ------------------------------------------------------------------------
table(BreastCancer$Class)

## ------------------------------------------------------------------------
tbl <- table(BreastCancer$Class)
tbl["benign"] / sum(tbl)

## ------------------------------------------------------------------------
table(BreastCancer$Class, sample(BreastCancer$Class))

## ------------------------------------------------------------------------
accuracy <- function(confusion_matrix)
  sum(diag(confusion_matrix))/sum(confusion_matrix)

replicate(8, accuracy(table(BreastCancer$Class, 
                            sample(BreastCancer$Class))))

## ------------------------------------------------------------------------
(specificity <- confusion_matrix[1,1]/
  (confusion_matrix[1,1] + confusion_matrix[1,2]))

## ------------------------------------------------------------------------
(sensitivity <- confusion_matrix[2,2]/
  (confusion_matrix[2,1] + confusion_matrix[2,2]))

## ------------------------------------------------------------------------
specificity <- function(confusion_matrix)
  confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[1,2])

sensitivity <- function(confusion_matrix) 
  confusion_matrix[2,2]/(confusion_matrix[2,1]+confusion_matrix[2,2])

prediction_summary <- function(confusion_matrix) 
  c("accuracy" = accuracy(confusion_matrix),
    "specificity" = specificity(confusion_matrix),
    "sensitivity" = sensitivity(confusion_matrix))

random_prediction_summary <- function()
  prediction_summary(table(BreastCancer$Class,
                           sample(BreastCancer$Class)))

replicate(3, random_prediction_summary())

## ------------------------------------------------------------------------
confusion_matrix[2,1] / sum(confusion_matrix[,1])

## ------------------------------------------------------------------------
confusion_matrix[1,1] / sum(confusion_matrix[,1])

## ------------------------------------------------------------------------
confusion_matrix[2,2] / sum(confusion_matrix[,2])
confusion_matrix[1,2] / sum(confusion_matrix[,2])

## ------------------------------------------------------------------------
permuted_cars <- cars[sample(1:nrow(cars)),]
permuted_cars %>% head(3)

## ------------------------------------------------------------------------
permute_rows <- function(df) df[sample(1:nrow(df)),]

## ------------------------------------------------------------------------
permuted_cars <- cars %>% permute_rows

## ------------------------------------------------------------------------
group_data <- function(df, n) {
  groups <- rep(1:n, each = nrow(df)/n)
  split(df, groups)
}

## ------------------------------------------------------------------------
cars %>% permute_rows %>% group_data(5) %>% head(1)

## ------------------------------------------------------------------------
grouped_cars <- cars %>% permute_rows %>% group_data(5)
grouped_cars[[1]]

## ------------------------------------------------------------------------
grouped_cars[1]

## ------------------------------------------------------------------------
grouped_cars[[1]] %>%
  lm(dist ~ speed, data = .) %>%
  .$coefficients

## ------------------------------------------------------------------------
estimates <- grouped_cars[[1]] %>%
  lm(dist ~ speed, data = .) %>%
  .$coefficients

for (i in 2:length(grouped_cars)) {
  group_estimates <- grouped_cars[[i]] %>%
    lm(dist ~ speed, data = .) %>%
    .$coefficients
  estimates <- rbind(estimates, group_estimates)
}

estimates

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(purrr, quietly = TRUE))

## ------------------------------------------------------------------------
estimates <- grouped_cars %>% 
  map(. %>% lm(dist ~ speed, data = .) %>% .$coefficients)

estimates

## ------------------------------------------------------------------------
estimates <- grouped_cars %>% 
  map(. %>% lm(dist ~ speed, data = .) %>% .$coefficients) %>%
  do.call("rbind", .)

estimates

## ------------------------------------------------------------------------
cross_validation_groups <- function(grouped_df) {
  result <- vector(mode = "list", length = length(grouped_df))
  for (i in seq_along(grouped_df)) {
    result[[i]] <- grouped_df[-i] %>% do.call("rbind", .)
  }
  result
}

## ------------------------------------------------------------------------
cars %>% 
  permute_rows %>% 
  group_data(5) %>% 
  cross_validation_groups %>% 
  map(. %>% lm(dist ~ speed, data = .) %>% .$coefficients) %>%
  do.call("rbind", .)

## ------------------------------------------------------------------------
cross_validation_split <- function(grouped_df) {
  result <- vector(mode = "list", length = length(grouped_df))
  for (i in seq_along(grouped_df)) {
    training <- grouped_df[-i] %>% do.call("rbind", .)
    test <- grouped_df[[i]]
    result[[i]] <- list(training = training, test = test)
  }
  result
}

## ------------------------------------------------------------------------
prediction_accuracy_cars <- function(test_and_training) {
  result <- vector(mode = "numeric", 
                   length = length(test_and_training))
  for (i in seq_along(test_and_training)) {
    training <- test_and_training[[i]]$training
    test <- test_and_training[[i]]$test
    model <- training %>% lm(dist ~ speed, data = .)
    predictions <- test %>% predict(model, data = .)
    targets <- test$dist
    result[i] <- rmse(targets, predictions)
  }
  result
}

## ------------------------------------------------------------------------
cars %>% 
  permute_rows %>% 
  group_data(5) %>% 
  cross_validation_split %>% 
  prediction_accuracy_cars

## ------------------------------------------------------------------------
random_group <- function(n, probs) {
  probs <- probs / sum(probs)
  g <- findInterval(seq(0, 1, length = n), c(0, cumsum(probs)),
                    rightmost.closed = TRUE)
  names(probs)[sample(g)]
}

## ------------------------------------------------------------------------
random_group(8, c(training = 0.5, test = 0.5))
random_group(8, c(training = 0.5, test = 0.5))

## ------------------------------------------------------------------------
random_group(8, c(training = 0.8, test = 0.2))

## ------------------------------------------------------------------------
partition <- function(df, n, probs) {
  replicate(n, split(df, random_group(nrow(df), probs)), FALSE)
}

## ------------------------------------------------------------------------
random_cars <- cars %>% partition(4, c(training = 0.5, test = 0.5))

## ------------------------------------------------------------------------
random_cars %>% prediction_accuracy_cars

## ------------------------------------------------------------------------
library(rpart)

model <- cars %>% rpart(dist ~ speed, data = .) 
rmse(predict(model, cars), cars$dist)

## ------------------------------------------------------------------------
model <- BreastCancer %>%
  rpart(Class ~ Cl.thickness, data = .)

## ------------------------------------------------------------------------
predict(model, BreastCancer) %>% head

## ------------------------------------------------------------------------
predicted_class <- predict(model, BreastCancer) %>% 
  as.data.frame %$%
  ifelse(benign > 0.5, "benign", "malignant")

table(BreastCancer$Class, predicted_class)

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(party, quietly = TRUE))

## ------------------------------------------------------------------------
model <- cars %>% ctree(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)

## ------------------------------------------------------------------------
model <- BreastCancer %>%
  ctree(Class ~ Cl.thickness, data = .)

predict(model, BreastCancer) %>% head

table(BreastCancer$Class, predict(model, BreastCancer))

## ----cars_ctree_plot, fig.cap="Plot of the cars decision tree."----------
cars %>% ctree(dist ~ speed, data = .) %>% plot

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(randomForest, quietly = TRUE))

## ------------------------------------------------------------------------
model <- cars %>% randomForest(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)

## ------------------------------------------------------------------------
model <- BreastCancer %>%
  randomForest(Class ~ Cl.thickness, data = .)

predict(model, BreastCancer) %>% head

table(BreastCancer$Class, predict(model, BreastCancer))

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(nnet, quietly = TRUE))

## ------------------------------------------------------------------------
model <- cars %>% nnet(dist ~ speed, data = ., size = 5) 
rmse(predict(model, cars), cars$dist)

## ------------------------------------------------------------------------
model <- BreastCancer %>%
  nnet(Class ~ Cl.thickness, data = ., size = 5)

## ------------------------------------------------------------------------
predict(model, BreastCancer) %>% head

## ------------------------------------------------------------------------
predicted_class <- predict(model, BreastCancer) %>%
  { ifelse(. < 0.5, "benign", "malignant") }

table(BreastCancer$Class, predicted_class)

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(kernlab, quietly = TRUE))

## ------------------------------------------------------------------------
model <- cars %>% ksvm(dist ~ speed, data = .)
rmse(predict(model, cars), cars$dist)

## ------------------------------------------------------------------------
model <- BreastCancer %>%
  ksvm(Class ~ Cl.thickness, data = .)

predict(model, BreastCancer) %>% head

table(BreastCancer$Class, predict(model, BreastCancer))

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(e1071, quietly = TRUE))

## ------------------------------------------------------------------------
model <- BreastCancer %>%
  naiveBayes(Class ~ Cl.thickness, data = .)

predict(model, BreastCancer) %>% head

table(BreastCancer$Class, predict(model, BreastCancer))

