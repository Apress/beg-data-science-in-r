## ---- echo=FALSE---------------------------------------------------------
blm <- function(model, alpha = 1, beta = 1, ...) {
     frame <- model.frame(model, ...)
      phi <- model.matrix(frame)
      no_params <- ncol(phi)
      target <- model.response(frame)

      covar <- solve(diag(alpha, no_params) + 
                      beta * t(phi) %*% phi)
    mean <- beta * covar %*% t(phi) %*% target

    list(formula = model,
         frame = frame,
         mean = mean,
         covar = covar)
}

## ------------------------------------------------------------------------
# fake some data for our linear model
x <- rnorm(10)
a <- 1 ; b <- 1.3
w0 <- 0.2 ; w1 <- 3
y <- rnorm(10, mean = w0 + w1 * x, sd = sqrt(1/b))

# fit a model
model <- blm(y ~ x, alpha = a, beta = b)
model

## ------------------------------------------------------------------------
class(model)

## ------------------------------------------------------------------------
class(model) <- "blm"

## ------------------------------------------------------------------------
model

## ------------------------------------------------------------------------
print.blm <- function(x, ...) {
  print(x$formula)
}

## ------------------------------------------------------------------------
model

## ------------------------------------------------------------------------
foo <- function(x, ...) UseMethod("foo")

## ------------------------------------------------------------------------
foo.default <- function(x, ...) print("default foo")

## ------------------------------------------------------------------------
foo("a string")
foo(12)

## ------------------------------------------------------------------------
foo.blm <- function(x, ...) print("blm foo")
foo(model)

## ------------------------------------------------------------------------
foo.blm <- function(x, upper = FALSE, ...) {
  if (upper) {
    print("BLM FOO")
  } else {
    print("blm foo")
  }
}

foo("a string")
foo(model)

foo("a string", upper = TRUE)
foo(model, upper = TRUE)

## ------------------------------------------------------------------------
foo <- function(object, ...) UseMethod("foo")
foo.default <- function(object, ...) stop("foo not implemented")

bar <- function(object, ...) UseMethod("bar")
bar.default <- function(object, ...) stop("bar not implemented")

A <- function(f, b) structure(list(foo = f, bar = b), class = "A")
foo.A <- function(object, ...) paste("A::foo ->", object$foo)
bar.A <- function(object, ...) paste("A::bar ->", object$bar)

a <- A("qux", "qax")
foo(a)
bar(a)

## ------------------------------------------------------------------------
baz <- function(object, ...) UseMethod("baz")
baz.default <- function(object, ...) stop("baz not implemented")

B <- function(f, b, bb) {
  a <- A(f, b)
  a$baz <- bb
  class(a) <- "B"
  a
}

bar.B <- function(object, ...) paste("B::bar ->", object$bar)
baz.B <- function(object, ...) paste("B::baz ->", object$baz)

## ------------------------------------------------------------------------
b <- B("qux", "qax", "quux")
foo(b)

## ------------------------------------------------------------------------
B <- function(f, b, bb) {
  a <- A(f, b)
  a$baz <- bb
  class(a) <- c("B", "A")
  a
}

## ------------------------------------------------------------------------
b <- B("qux", "qax", "quux")
foo(b)
bar(b)
baz(b)

## ------------------------------------------------------------------------
C <- function(f, b, bb) {
  b <- B(f, b, bb)
  class(b) <- c("C", "B")
  b
}

c <- C("foo", "bar", "baz")
foo(c)

## ------------------------------------------------------------------------
C <- function(f, b, bb) {
  b <- B(f, b, bb)
  class(b) <- c("C", "B", "A")
  b
}

c <- C("foo", "bar", "baz")
foo(c)
bar(c)
baz(c)

## ------------------------------------------------------------------------
C <- function(f, b, bb) {
  b <- B(f, b, bb)
  class(b) <- c("C", class(b))
  b
}

