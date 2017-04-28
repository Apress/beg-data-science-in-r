## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(microbenchmark, quietly = TRUE))

## ------------------------------------------------------------------------
mysum <- function(sequence) {
  s <- 0
  for (x in sequence) s <- s + x
  s
}

microbenchmark(
  sum(1:10),
  mysum(1:10)
)

## ------------------------------------------------------------------------
microbenchmark(
  sum(1:10),
  mysum(1:10),
  Reduce(`+`, 1:10, 0)
)

## ------------------------------------------------------------------------
x <- sample(LETTERS, 1000, replace = TRUE)
microbenchmark(
  factor(x, levels = LETTERS),
  factor(x)
)

## ------------------------------------------------------------------------
x <- rnorm(1000)
names(x) <- paste("n", 1:1000)
microbenchmark(
  unlist(Map(function(x) x**2, x), use.names = FALSE),
  unlist(Map(function(x) x**2, x))
)

