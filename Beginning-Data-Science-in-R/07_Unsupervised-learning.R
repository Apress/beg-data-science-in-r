## ------------------------------------------------------------------------
iris %>% head

## ----iris-pca-sepal-length-vs-width, fig.cap="Plot of iris sepal length versus sepal width."----
iris %>% ggplot() +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, colour = Species))

## ----iris-pca-petal-length-vs-width, fig.cap="Plot of iris petal length versus petal width."----
iris %>% ggplot() +
  geom_point(aes(x = Petal.Length, y = Petal.Width, colour = Species))

## ------------------------------------------------------------------------
pca <- iris %>% select(-Species) %>% prcomp
pca

## ----iris-pca-variance, fig.cap="Plot of the variance on each principal component for the iris data set."----
pca %>% plot

## ------------------------------------------------------------------------
mapped_iris <- pca %>% predict(iris)
mapped_iris %>% head

## ----iris-pca-plot, fig.cap="Plot of first two principal components for the iris data set."----
mapped_iris %>%
  as.data.frame %>%
  cbind(Species = iris$Species) %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2, colour = Species))

## ------------------------------------------------------------------------
library(mlbench)
data(HouseVotes84)
HouseVotes84 %>% head

## ------------------------------------------------------------------------
HouseVotes84 %>% select(-Class) %>% prcomp

## ------------------------------------------------------------------------
HouseVotes84 %>%
  select(-Class) %>%
  apply(c(1,2), . %>% { ifelse(as.character(.) == "n", 0, 1) }) %>%
  prcomp

## ------------------------------------------------------------------------
vote_patterns <- HouseVotes84 %>%
  select(-Class) %>%
  apply(c(1,2), . %>% { ifelse(as.character(.) == "n", 0, 1) }) %>%
  apply(c(1,2), . %>% { ifelse(is.na(.), 0.5, .) })

pca <- vote_patterns %>% prcomp

## ----votes-pca-plot, fig.cap="Plot of first two principal components for the house votes data set."----
mapped_votes <- pca %>% predict(vote_patterns)
mapped_votes %>%
  as.data.frame %>%
  cbind(Class = HouseVotes84$Class) %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2, colour = Class))

## ------------------------------------------------------------------------
iris_dist <- iris %>% select(-Species) %>% dist

## ------------------------------------------------------------------------
mds_iris <- iris_dist %>% cmdscale(k=2)
mds_iris %>% head

## ----irs-votes, fig.cap="Multidimensional scaling plot for house voting data."----
mds_iris %>%
  as.data.frame %>%
  cbind(Species = iris$Species) %>%
  ggplot() +
  geom_point(aes(x = V1, y = V2, colour = Species))

## ----mds-votes, fig.cap="Multidimensional scaling plot for house voting data."----
mds_votes <- vote_patterns %>% dist %>% cmdscale(k = 2)

mds_votes %>%
  as.data.frame %>%
  cbind(Class = HouseVotes84$Class) %>%
  ggplot() +
  geom_point(aes(x = V1, y = V2, colour = Class))

## ------------------------------------------------------------------------
random_ngram <- function(n) 
  sample(c('A','C','G','T'), size = n, replace = TRUE) %>% 
  paste0(collapse = "")

random_string <- function(m) {
  n <- max(1, m + sample(c(-1,1), size = 1) * rgeom(1, 1/2))
  random_ngram(n)
}

strings <- replicate(10, random_string(5))

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(stringdist, quietly = TRUE))

## ------------------------------------------------------------------------
string_dist <- stringdistmatrix(strings)

## ----string-mds, fig.cap="Multidimensionality reduction for random strings."----
string_dist %>% 
  cmdscale(k = 2) %>%
  as.data.frame %>%
  cbind(String = strings) %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point() +
  geom_label(aes(label = String), 
               hjust = 0, nudge_y = -0.1)

## ---- echo=FALSE---------------------------------------------------------
set.seed(1)

## ------------------------------------------------------------------------
clusters <- iris %>%
  select(-Species) %>% 
  kmeans(centers = 3)

## ------------------------------------------------------------------------
clusters$centers

## ------------------------------------------------------------------------
clusters$cluster %>% head
clusters$cluster %>% table

## ----iris-cluster-barplot, fig.cap="Cluster assignments for the three iris species."----
iris %>% 
  cbind(Cluster = clusters$cluster) %>%
  ggplot() +
  geom_bar(aes(x = Species, fill = as.factor(Cluster)),
           position = "dodge") +
  scale_fill_discrete("Cluster")

## ------------------------------------------------------------------------
pca <- iris %>% 
  select(-Species) %>% 
  prcomp

mapped_iris <- pca %>% 
  predict(iris)

mapped_centers <- pca %>% 
  predict(clusters$centers)

## ----iris-k-means-clusters, fig.cap="Clusters and species for iris."-----
mapped_iris %>%
  as.data.frame %>%
  cbind(Species = iris$Species,
        Clusters = as.factor(clusters$cluster)) %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2,
                   colour = Species, shape = Clusters)) +
  geom_point(aes(x = PC1, y = PC2), 
               size = 5, shape = "X",
             data = as.data.frame(mapped_centers))

## ---- echo=FALSE---------------------------------------------------------
set.seed(4)
bad_clusters <- iris %>%
  select(-Species) %>% 
  kmeans(centers = 3)

## ----iris-cluster-barplot-bad, fig.cap="A bad cluster assignment for the three iris species.", echo=FALSE----
iris %>% 
  cbind(Cluster = bad_clusters$cluster) %>%
  ggplot() +
  geom_bar(aes(x = Species, fill = as.factor(Cluster)),
           position = "dodge") +
  scale_fill_discrete("Cluster")

## ----iris-k-means-clusters-bad, fig.cap="Clusters and species for iris for a bad clustering.", echo=FALSE----
bad_mapped_centers <- pca %>% 
  predict(bad_clusters$centers)

mapped_iris %>%
  as.data.frame %>%
  cbind(Species = iris$Species,
        Clusters = as.factor(bad_clusters$cluster)) %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2,
                   colour = Species, shape = Clusters)) +
  geom_point(aes(x = PC1, y = PC2), 
               size = 5, shape = 4,
             data = as.data.frame(bad_mapped_centers))

## ------------------------------------------------------------------------
table(iris$Species, clusters$cluster)

## ------------------------------------------------------------------------
tbl <- table(iris$Species, clusters$cluster)
(counts <- apply(tbl, 1, which.max))

## ------------------------------------------------------------------------
map <- rep(NA, each = 3)
map[counts] <- names(counts)
table(iris$Species, map[clusters$cluster])

## ------------------------------------------------------------------------
iris_dist <- iris %>% select(-Species) %>% scale %>% dist

## ------------------------------------------------------------------------
clustering <- hclust(iris_dist)

## ----hclust-iris, fig.cap="Hierarchical clustering of iris data."--------
plot(clustering)

## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(ggdendro, quietly = TRUE))

## ----hclust-iris-dendro, fig.cap="Hierarchical clustering of iris data plotted with ggdendro."----
ggdendrogram(clustering) + theme_dendro()

## ------------------------------------------------------------------------
clusters <- clustering %>% cutree(k = 3)

## ----iris-hclust-barplot, fig.cap="Iris clustering as a bar-plot."-------
iris %>%
  cbind(Cluster = clusters) %>%
  ggplot() +
  geom_bar(aes(x = Species, fill = as.factor(Cluster)),
           position = "dodge") +
  scale_fill_discrete("Cluster")

## ----iris-hclust-points, fig.cap="Iris points plotted with species and hierarchical clustering information."----
mapped_iris %>%
  as.data.frame %>%
  cbind(Species = iris$Species,
        Clusters = as.factor(clusters)) %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2, 
                   shape = Species, colour = Clusters))

## ---- warning=FALSE, echo=FALSE------------------------------------------
suppressPackageStartupMessages(library(arules))
suppressPackageStartupMessages(library(kernlab))

## ------------------------------------------------------------------------
data(income)
income %>% head

## ------------------------------------------------------------------------
data(Income)
Income %>% head

## ------------------------------------------------------------------------
rules <- income %>% apriori

## ------------------------------------------------------------------------
rules %>% head %>% inspect(linebreak = FALSE)

## ------------------------------------------------------------------------
rules %>% sort(by = "lift") %>%
  head %>% inspect(linebreak = FALSE)

## ------------------------------------------------------------------------
rules %>% subset(support > 0.5) %>% sort(by = "lift") %>%
  head %>% inspect(linebreak = FALSE)

