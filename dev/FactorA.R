# New example for factor analysis
# first attempt
library(tidyverse)
library(psych)
library(psychTools)

# using the Wisconsin Breast Cancer dataset from the UCI Machine learning repository
wdbc <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",
                 col_names = FALSE, show_col_types = FALSE)

# The file comes without colume names, we will use features and qualifications.
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", 
              "concave_points", "symmetry", "fractal_dimension")
names(wdbc) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"),
                 paste0(features,"_worst"))

# The data is used at Mike's PCA example.
# We will try the factor analysis with psych package

# First look at the variables in a correlation matrix
wdbc %>%
  select(radius_mean:fractal_dimension_mean) %>%
  plot()

# Now for selectin only the data without the ID and group
wdbc_all <- wdbc %>% select(-1, -2)

wdbc_mean <- wdbc_all %>% select(radius_mean:fractal_dimension_mean)

# starting with latent variable exploratory factor analysis (EFA)
principal(wdbc_all, 2, rotate = "varimax", cor = "cor", method = "regression")
fa(wdbc_all, 2, rotate="varimax", fm="ml", oblique.scores=TRUE)
# fa(wdbc_all, 2, rotate = "varimax", fm= "ml")
iclust(wdbc_all, nclusters=2)

fa(wdbc_mean, 2, rotate="varimax", fm="ml", oblique.scores=TRUE)
iclust(wdbc_mean, nclusters=2)
