## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(MASS, quietly = TRUE))

## ------------------------------------------------------------------------
mvrnorm(n = 5, mu = c(0,0), Sigma = diag(1, nrow = 2))

## ---- echo=FALSE---------------------------------------------------------
make_prior <- function(alpha) {
  mean <- c(0, 0)
  S <- diag(1/alpha, nrow = 2)
  list(mean = mean, S = S)
}

sample_from_prior <- function(n, alpha) {
  prior <- make_prior(alpha)
  samples <- mvrnorm(n = n, mu = prior$mean, Sigma = prior$S)
  data.frame(w1 = samples[,1], w0 = samples[,2])  
}

## ---- fig.cap="Weight vectors sampled from the prior distribution.", echo=FALSE----
samples <- sample_from_prior(10, 1)
plot(samples$w0, samples$w1, pch=20,
     xlab=expression(w[0]), ylab=expression(w[1]))

## ---- fig.cap="Weight vectors sampled from the prior distribution represented as lines.", echo=FALSE----
plot(c(-1,1), c(-1,1), type="n", xlab="x", ylab="y")
for (i in seq(samples$w0)) {
    abline(a = samples$w0[i], b = samples$w1[i])
}

## ---- fig.cap="Randomly sampled $(x,y)$ values. The true linear model is shown in red."----
w0 <- 0.3 ; w1 <- 1.1 ; beta <- 1.3
x <- rnorm(50)
y <- rnorm(50, w1 * x + w0, 1/beta)

## ---- fig.cap="Randomly sampled $(x,y)$ values.", echo=FALSE-------------
plot(x, y, pch=20)
abline(a = w0, b = w1, col="red")

## ---- echo=FALSE---------------------------------------------------------
make_model_matrix <- function(x) cbind(1, x)

make_posterior <- function(x, y, alpha, beta) {
    phi <- make_model_matrix(x)
    S <- solve(diag(alpha, 2) + beta * t(phi) %*% phi)
    mean <- beta * S %*% t(phi) %*% y
    list(mean = mean, S = S)
}

sample_from_posterior <- function(n, x, y, alpha, beta) {
  posterior <- make_posterior(x, y, alpha, beta)
  samples <- mvrnorm(n = n, mu = posterior$mean, Sigma = posterior$S)
  data.frame(w0 = samples[,1], w1 = samples[,2])  
}

## ---- fig.cap="Samples from the posterior. The true value is shown as a red dot.", echo=FALSE----
plot_samples <- function(n) {
    x <- rnorm(n)
    y <- rnorm(n, w1 * x + w0, 1/beta)
    samples <- sample_from_posterior(10, x, y, beta, 1)
    plot(samples$w0, samples$w1, pch=20, main=paste(n, "points"),
         xlim=c(w0-1,w0+1), ylim=c(w1-1,w1+1),
         xlab=expression(w[0]), ylab=expression(w[1]))
    points(w0, w1, col="red", pch=20, cex=2)
}

layout(matrix(1:4, nrow=2, byrow=TRUE))
plot_samples(5)  ; plot_samples(10)
plot_samples(20) ; plot_samples(50)

## ---- fig.cap="Lines drawn from the posterior. The true line is shown in red.", echo=FALSE----
plot_samples <- function(n) {
    x <- rnorm(n)
    y <- rnorm(n, w1 * x + w0, 1/beta)
    samples <- sample_from_posterior(10, x, y, beta, 1)

    plot(x, y, pch=20, main=paste(n, "points"))
    for (i in seq(samples$w0)) {
        abline(a = samples$w0[i], b = samples$w1[i], col="lightgray")
    }
    points(x, y, pch=20)
    abline(a = w0, b = w1, col="red")
}

layout(matrix(1:4, nrow=2, byrow=TRUE))
plot_samples(5)  ; plot_samples(10)
plot_samples(20) ; plot_samples(50)

## ---- echo = FALSE, fig.cap="True linear model in red and predicted values in blue."----
predict_new_dist <- function(x, posterior, beta) {
  phi <- function(x) matrix(c(1, x), ncol=1)
  
  mean = t(posterior$mean) %*% phi(x)
  var = 1/beta + t(phi(x)) %*% posterior$S %*% phi(x)
  list(mean = mean, var = var)
}

predict_new_map <- function(predict_dist) predict_dist$mean

alpha <- 1; beta <- 0.2
w0 <- 0.2; w1 <- 1.2
x <- c(rnorm(20, sd=5), rnorm(10, sd=50))
y <- rnorm(30, w1 * x + w0, 1/beta)

posterior <- make_posterior(x, y, alpha, beta)
predict_x <- Vectorize(function(x) predict_new_map(predict_new_dist(x, posterior, beta)))

plot(x, y, pch=20)
abline(a = w0, b = w1, col="red")

new_x <- seq(min(x)-1, max(x)+100000, length.out = 100)
new_y <- predict_x(new_x)
lines(new_x, new_y, col="blue")


## ---- fig.cap="Prediction with 95% support interval.", echo=FALSE--------
predict_new_quantile <- function(x, q, posterior, beta) {
  predict_dist <- predict_new_dist(x, posterior, beta)
  qnorm(q, mean = predict_dist$mean, sd = sqrt(predict_dist$var))
}

new_y_q <- rbind(vapply(new_x, function(x) predict_new_quantile(x, 0.975, posterior, beta), 1),
                 vapply(new_x, function(x) predict_new_quantile(x, 0.025, posterior, beta), 1))

xx <- vapply(new_x, function(x) predict_new_quantile(x, 0.975, posterior, beta), 1)


plot(x, y, pch=20)
abline(a = w0, b = w1, col="red")
lines(new_x, new_y, col="blue")
lines(new_x, new_y_q[1,], col="blue", lty="dashed")
lines(new_x, new_y_q[2,], col="blue", lty="dashed")

## ---- fig.cap="Prediction with 95% support interval, wider range.", echo=FALSE----
predict_new_quantile <- function(x, q, posterior, beta) {
  predict_dist <- predict_new_dist(x, posterior, beta)
  qnorm(q, mean = predict_dist$mean, sd = sqrt(predict_dist$var))
}

new_y_q <- rbind(vapply(new_x, function(x) predict_new_quantile(x, 0.975, posterior, beta), 1),
                 vapply(new_x, function(x) predict_new_quantile(x, 0.025, posterior, beta), 1))

xx <- vapply(new_x, function(x) predict_new_quantile(x, 0.975, posterior, beta), 1)


plot(x, y, pch=20, xlim=range(new_x), ylim=c(min(y), 100000))
abline(a = w0, b = w1, col="red")
lines(new_x, new_y, col="blue")
lines(new_x, new_y_q[1,], col="blue", lty="dashed")
lines(new_x, new_y_q[2,], col="blue", lty="dashed")

## ------------------------------------------------------------------------

predictors <- data.frame(x = rnorm(5), z = rnorm(5))
y <- with(predictors, rnorm(5, mean = 3*x + 5*z + 2))

model <- y ~ x + z

model.frame(model, data = predictors)

## ------------------------------------------------------------------------
x <- runif(10)
model.frame(~ x + I(x^2))

## ------------------------------------------------------------------------
x <- runif(10)
y <- rnorm(10, mean=x)

model.no.intercept <- y ~ x + 0
(frame.no.intercept <- model.frame(model.no.intercept))
model.matrix(model.no.intercept, frame.no.intercept)

model.with.intercept <- y ~ x
(frame.with.intercept <- model.frame(model.with.intercept))
model.matrix(model.with.intercept, frame.with.intercept)

## ------------------------------------------------------------------------
model.response(frame.with.intercept)

## ------------------------------------------------------------------------
training.data <- data.frame(x = runif(5), y = runif(5))
frame <- model.frame(y ~ x, training.data)
model.matrix(y ~ x, frame)

predict.data <- data.frame(x = runif(5))
frame <- model.frame(y ~ x, predict.data)

## ------------------------------------------------------------------------
responseless.formula <- delete.response(terms(y ~ x))
frame <- model.frame(responseless.formula, predict.data)
model.matrix(responseless.formula, frame)

