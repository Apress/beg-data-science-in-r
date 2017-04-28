## ----cars_plot, fig.cap="Plot of the cars dataset."----------------------
data(cars)
head(cars)
cars %>% qplot(speed, dist, data = .)

## ------------------------------------------------------------------------
cars %>% head(3)

## ------------------------------------------------------------------------
cars %>% tail(3)

## ------------------------------------------------------------------------
cars %>% summary

## ------------------------------------------------------------------------
data(iris)
iris %>% summary

## ------------------------------------------------------------------------
library(mlbench)
data(BreastCancer)
BreastCancer %>% head(3)

## ---- echo=FALSE---------------------------------------------------------
data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"

## ---- cache=TRUE---------------------------------------------------------
lines <- readLines(data_url)
lines[1:5]

## ---- cache=TRUE---------------------------------------------------------
writeLines(lines, con = "data/raw-breast-cancer.csv")

## ------------------------------------------------------------------------
raw_breast_cancer <- read.csv("data/raw-breast-cancer.csv")
raw_breast_cancer %>% head(3)

## ---- cache=TRUE---------------------------------------------------------
raw_breast_cancer <- read.csv(data_url)
raw_breast_cancer %>% head(3)

## ---- echo=FALSE---------------------------------------------------------
data_url <- "data/raw-breast-cancer.csv"

## ------------------------------------------------------------------------
raw_breast_cancer <- read.csv(data_url, header = FALSE)
raw_breast_cancer %>% head(3)

## ------------------------------------------------------------------------
names(raw_breast_cancer) <- names(BreastCancer)
raw_breast_cancer %>% head(3)

## ------------------------------------------------------------------------
raw_breast_cancer <- read.csv(data_url, header = FALSE,
                              col.names = names(BreastCancer))
raw_breast_cancer %>% head(3)

## ------------------------------------------------------------------------
formatted_breast_cancer <- raw_breast_cancer

## ------------------------------------------------------------------------
map_class <- function(x) {
    ifelse(x == 2, "bening",
    ifelse(x == 4, "malignant",
           NA))
}
mapped <- formatted_breast_cancer$Class %>% map_class
mapped %>% table

## ------------------------------------------------------------------------
map_class <- function(x) {
    ifelse(x == 2, "bening", "malignant")
}
mapped <- formatted_breast_cancer$Class %>% map_class
mapped %>% table

## ------------------------------------------------------------------------
formatted_breast_cancer$Class %>% unique

## ------------------------------------------------------------------------
dict <- c("2" = "benign", "4" = "malignant")
map_class <- function(x) dict[as.character(x)]

mapped <- formatted_breast_cancer$Class %>% map_class
mapped %>% table

## ------------------------------------------------------------------------
mapped[1:5]

## ------------------------------------------------------------------------
mapped %<>% unname
mapped[1:5]

## ------------------------------------------------------------------------
read.csv(data_url, header = FALSE,
         col.names = names(BreastCancer)) ->
  formatted_breast_cancer ->
  raw_breast_cancer

dict <- c("2" = "benign", "4" = "malignant")
map_class <- function(x) dict[as.character(x)]
formatted_breast_cancer$Class <- 
  formatted_breast_cancer$Class %>%
  map_class %>%
  unname %>% 
  factor(levels = c("benign", "malignant"))

## ------------------------------------------------------------------------
raw_breast_cancer$Class %>%
  { dict <- c("2" = "benign", "4" = "malignant")
    dict[as.character(.)]
  } %>%
  unname %>% 
  factor(levels = c("benign", "malignant")) %>%
  table

## ------------------------------------------------------------------------
formatted_breast_cancer %>%
    save(file = "data/formatted-breast-cancer.rda")

## ------------------------------------------------------------------------
load("data/formatted-breast-cancer.rda")

## ------------------------------------------------------------------------
library(mlbench)
data(BostonHousing)
str(BostonHousing)

## ---- echo=FALSE---------------------------------------------------------
data_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
tiny_url <- "http://tinyurl.com/zq2u8vx"

## ------------------------------------------------------------------------
boston_housing <- read.table(data_url)
str(boston_housing)

## ------------------------------------------------------------------------
col_classes <- rep("numeric", length(BostonHousing))
col_classes[which("chas" == names(BostonHousing))] <- "factor"

## ------------------------------------------------------------------------
boston_housing <- read.table(data_url, 
                             col.names = names(BostonHousing),
                             colClasses = col_classes)
str(boston_housing)

## ------------------------------------------------------------------------
library(readr)

## ------------------------------------------------------------------------
raw_breast_cancer <- read_csv("data/raw-breast-cancer.csv")
raw_breast_cancer %>% head(3)

## ------------------------------------------------------------------------
raw_breast_cancer <- read_csv("data/raw-breast-cancer.csv",
                              col_names = names(BreastCancer))
raw_breast_cancer %>% head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df

## ------------------------------------------------------------------------
iris %>% tbl_df %>% select(Petal.Width) %>% head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% 
  select(Sepal.Width, Petal.Length) %>% head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>%
  select(Sepal.Length:Petal.Length) %>% head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% 
  select(starts_with("Petal")) %>% head(3)
iris %>% tbl_df %>% 
  select(ends_with("Width")) %>% head(3)
iris %>% tbl_df %>% 
  select(contains("etal")) %>% head(3)
iris %>% tbl_df %>% 
  select(matches(".t.")) %>% head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% 
  select(-starts_with("Petal")) %>% head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>%
  mutate(Petal.Width.plus.Length = Petal.Width + Petal.Length) %>%
  select(Species, Petal.Width.plus.Length) %>%
  head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% 
  mutate(Petal.Width.plus.Length = Petal.Width + Petal.Length,
         Sepal.Width.plus.Length = Sepal.Width + Sepal.Length) %>%
  select(Petal.Width.plus.Length, Sepal.Width.plus.Length) %>%
  head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% 
  transmute(Petal.Width.plus.Length = Petal.Width + Petal.Length) %>%
  head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% 
  arrange(Sepal.Length) %>%
  head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% 
  arrange(desc(Sepal.Length)) %>%
  head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% 
  filter(Sepal.Length > 5) %>%
  head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% 
  filter(Sepal.Length > 5 & Species == "virginica") %>%
  select(Species, Sepal.Length) %>%
  head(3)

## ------------------------------------------------------------------------
iris %>% tbl_df %>% group_by(Species) %>% head(3)

## ------------------------------------------------------------------------
iris %>%
  summarise(Mean.Petal.Length = mean(Petal.Length),
              Mean.Sepal.Length = mean(Sepal.Length))

## ------------------------------------------------------------------------
iris %>%
  group_by(Species) %>% 
  summarise(Mean.Petal.Length = mean(Petal.Length))

## ------------------------------------------------------------------------
iris %>%
  summarise(Observations = n())

## ------------------------------------------------------------------------
iris %>%
  group_by(Species) %>% 
  summarise(Number.Of.Species = n())

## ------------------------------------------------------------------------
iris %>% 
  group_by(Species) %>%
  summarise(Number.Of.Samples = n(),
              Mean.Petal.Length = mean(Petal.Length))

## ------------------------------------------------------------------------
formatted_breast_cancer <- 
  raw_breast_cancer %>% 
  mutate(Class = Class %>% {
      c("2" = "benign", "4" = "malignant")[as.character(.)] 
    } %>%
    unname %>% 
    factor(levels = c("benign", "malignant")) )

## ------------------------------------------------------------------------
format_class <- . %>% {
  dict <- c("2" = "benign", "4" = "malignant")
  dict[as.character(.)] 
} %>% unname %>% factor(levels = c("benign", "malignant"))

formatted_breast_cancer <- 
  raw_breast_cancer %>% mutate(Class = format_class(Class))

## ------------------------------------------------------------------------
formatted_breast_cancer %>%
  group_by(Class) %>%
  summarise(mean.thickness = mean(Cl.thickness))

## ------------------------------------------------------------------------
formatted_breast_cancer %>%
  group_by(Class) %>%
  summarise(mean.size = mean(Cell.size))

## ------------------------------------------------------------------------
formatted_breast_cancer %>%
  arrange(Cell.size) %>%
  group_by(Cell.size, Class) %>%
  summarise(ClassCount = n())

## ------------------------------------------------------------------------
formatted_breast_cancer %>%
  group_by(Class, as.factor(Cell.size)) %>%
  summarise(mean.thickness = mean(Cl.thickness))

## ------------------------------------------------------------------------
formatted_breast_cancer %>%
  group_by(as.factor(Cell.size), Class) %>%
  summarise(mean.thickness = mean(Cl.thickness))

## ------------------------------------------------------------------------
iris %>% select(Species, Petal.Length) %>% head(3)

## ----iris_species_vs_petal_length, fig.cap="Plot species versus petal length."----
iris %>% select(Species, Petal.Length) %>%
  qplot(Species, Petal.Length, geom = "boxplot", data = .)

## ------------------------------------------------------------------------
iris %>% 
  gather(key = Attribute, value = Measurement, 
         Sepal.Length, Sepal.Width) %>%
  select(Species, Attribute, Measurement) %>%
  head(3)

## ----iris_attributes_vs_mesurements, fig.cap="Plot measurements versus values."----
iris %>% 
  gather(key = Attribute, value = Measurement, 
         Sepal.Length, Sepal.Width) %>%
  select(Species, Attribute, Measurement) %>%
  qplot(Attribute, Measurement, 
          geom = "boxplot", 
          facets = . ~ Species, data = .)

