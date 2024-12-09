# New example for factor analysis
# first attempt
library(tidyverse)
library(psych)
library(psychTools)
library(factoextra)

# using the Wisconsin Breast Cancer dataset from the UCI Machine learning repository
wdbc <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",
                 col_names = FALSE, show_col_types = FALSE)

# The file comes without colume names, we will use features and qualifications.
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", 
              "concave_points", "symmetry", "fractal_dimension")
names(wdbc) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"),
                 paste0(features,"_worst"))

# The data is used at Mike's PCA example.
# We will try the factor analysis subroutines with psych package

# Now for selection only the data without the ID and group
wdbc_all <- wdbc %>% select(-1, -2)
wdbc_mean <- wdbc_all %>% select(radius_mean:fractal_dimension_mean)
wdbc_scale <- scale(wdbc_all)

# First look at the variables in a correlation matrix
# wdbc_mean %>%
#  select(radius_mean:fractal_dimension_mean) %>%
#  plot()
pairs.panels(wdbc_mean, pch = 21, stars = TRUE,  smoother = TRUE)

# starting with latent variable exploratory factor analysis (EFA)
# the PCS for all
# principal(wdbc_all, 30, rotate = "none", scores = TRUE, cor = "cor", method = "regression")
pca_wdbc <- principal(wdbc_scale, 30, rotate = "varimax", scores = TRUE, use ="pairwise", cor = "cor", covar = TRUE, method = "regression")
plot(pca_wdbc$values, type = "b", ylab = "Eigenvalues", xlab = "Component Number")

scores_pca_wdbc <- pca_wdbc$scores
text(scores_pca_wdbc[,1], scores_pca_wdbc[,2], labels=rownames(scores_pca_wdbc), cex=0.7)

pca_wdbc_4c <- principal(wdbc_scale, 4, rotate = "varimax", scores = TRUE, cor = "cor", method = "regression")
biplot(pca_wdbc_4c)

fa_wdbc_4c <- fa(wdbc_scale, 4, rotate="varimax", fm="ml", oblique.scores=TRUE)
fa_wdbc_4c

pca_wdbc_4c_loadings_df <- as_tibble(pca_wdbc_4c$loadings[])
pca_wdbc_4c_loadings_df$variable <- rownames(pca_wdbc_4c_loadings_df)
pca_wdbc_4c_loadings_long <- pivot_longer(pca_wdbc_4c_loadings_df, -variable, names_to = "component", values_to = "loading")
# boxplot(loading~component, data=pca_wdbc_4c_loadings_long)

ggplot(pca_wdbc_4c_loadings_long, aes(x = component, y = loading)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Principal Component Loadings", x = "Component", y = "Loading")

iclust(wdbc_scale, nclusters = 4)

# pca_wdbc_2c <- principal(wdbc_scale, 2, rotate = "varimax", scores = TRUE, cor = "cor", method = "regression")
# pca_wdbc_2c

# pca_wdbc_2c_loadings_df <- as_tibble(pca_wdbc_2c$loadings[])
# pca_wdbc_2c_loadings_df$variable <- rownames(pca_wdbc_2c_loadings_df)
# pca_wdbc_2c_loadings_long <- pivot_longer(pca_wdbc_2c_loadings_df, -variable, names_to = "component", values_to = "loading")

# ggplot(pca_wdbc_2c_loadings_long, aes(x = component, y = loading)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Principal Component Loadings", x = "Component", y = "Loading")

# fa(wdbc_scale, 2, rotate="varimax", fm="ml", oblique.scores=TRUE)
