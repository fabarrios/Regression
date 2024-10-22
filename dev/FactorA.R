# New example for factor analysis
# first attempt

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
wdbc_pca <- wdbc %>%
  select(-1, -2) %>%
  prcomp(center = TRUE, scale = TRUE)

# starting with latent variable exploratory factor analysis (EFA)
iclust(wdbc_pca, nclusters=3)