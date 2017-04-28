## ------------------------------------------------------------------------
iris %>% sample_n(size = 5)

iris %>% sample_frac(size = 0.02)

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(pryr, quietly = TRUE))
rm(x, y)

## ------------------------------------------------------------------------
mem_change(x <- rnorm(10000))

## ------------------------------------------------------------------------
mem_change(x[1] <- 0)

## ------------------------------------------------------------------------
mem_change(y <- x)

## ------------------------------------------------------------------------
mem_change(x[1] <- 0)

## ------------------------------------------------------------------------
d <- data.frame(x = rnorm(10000), y = rnorm(10000))

## ----large-scatter-plot, fig.cap="A scatter plot with too many points."----
d %>% ggplot(aes(x = x, y = y)) +
  geom_point()

## ----large-scatter-plot-alpha, fig.cap="A scatter plot with alpha values."----
d %>% ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.2)

## ----density-2d-plot, fig.cap="A 2D density plot."-----------------------
d %>% ggplot(aes(x = x, y = y)) +
  geom_density_2d()

## ---- echo=FALSE---------------------------------------------------------
suppressPackageStartupMessages(library(hexbin, quietly = TRUE))

## ----hex-plot, fig.cap="A hex plot."-------------------------------------
d %>% ggplot(aes(x = x, y = y)) +
  geom_hex()

## ----combined-hex-density-plot, fig.cap="A plot combining hex and 2D density."----
d %>% ggplot(aes(x = x, y = y)) +
  geom_hex() +
  scale_fill_gradient(low = "lightgray", high = "red") +
  geom_density2d(color = "black")

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(biglm, quietly = TRUE))

## ------------------------------------------------------------------------
slice_size <- 10
n <- nrow(cars)
slice <- cars %>% slice(1:slice_size)
model <- biglm(dist ~ speed, data = slice)
for (i in 1:(n/slice_size-1)) {
  slice <- cars %>% slice((i*slice_size+1):((i+1)*slice_size))
  model <- update(model, moredata = slice)
}
model

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(ff, quietly = TRUE))

## ------------------------------------------------------------------------
ffcars <- as.ffdf(cars)
summary(ffcars)

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(ffbase, quietly = TRUE))

## ------------------------------------------------------------------------
model <- bigglm(dist ~ speed, data = ffcars)
summary(model)

## ------------------------------------------------------------------------
iris_db <- src_sqlite("iris_db.sqlite3", create = TRUE)

## ------------------------------------------------------------------------
iris_sqlite <- tbl(iris_db, "iris")

## ------------------------------------------------------------------------
iris_sqlite %>% group_by(Species) %>%
  summarise(mean.Petal.Length = mean(Petal.Length))

