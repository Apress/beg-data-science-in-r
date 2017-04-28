## ---- echo=FALSE---------------------------------------------------------
area <- function(x) UseMethod("area")
area.circle <- function(x) pi * x$r**2
area.rectangle <- function(x) x$height * x$width

circumference <- function(x) UseMethod("circumference")
circumference.circle <- function(x) 2 * pi * x$r
circumference.rectangle <- function(x) 2 * x$height + 2 * x$width

rectangle <- function(width, height) {
    structure(list(width = width, height = height),
              class = c("rectangle", "shape"))
}

circle <- function(radius) {
    structure(list(r = radius),
              class = c("circle", "shape"))
}

## ------------------------------------------------------------------------
area <- function(x) UseMethod("area")
circumference <- function(x) UseMethod("circumference")

rectangle <- function(width, height) {
    structure(list(width = width, height = height),
              class = c("rectangle", "shape"))
}
area.rectangle <- function(x) x$height * x$width
circumference.rectangle <- function(x) 2 * x$height + 2 * x$width

r <- rectangle(width = 2, height = 4)
area(r)
circumference(r)

## ------------------------------------------------------------------------
r <- rectangle(width = 2, height = 4)
if (area(r) != 2*4) {
    stop("Area not computed correctly!")
}
if (circumference(r) != 2*2 + 2*4) {
    stop("Circumference not computed correctly!")
}

