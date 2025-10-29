# --- 1. Load the dataset 
Table <- read.csv("METEO_BD.csv")

# --- 2. Data cleaning 
# Remove the first column (often an index or label)
Table <- Table[, -1]

# Remove columns 13 to 17 (irrelevant or extra variables)
Table <- Table[, -c(13:17)]

# --- 3. Convert the table into a numeric matrix 
Meteo <- as.matrix(Table)
# --- 4. Centering and scaling (standardization) 
# Each column (variable) is centered (mean = 0) and scaled (std = 1)
Meteo_Moy_Ecart <- scale(Meteo, center = TRUE, scale = TRUE)

# --- 5. Compute the covariance matrix 
Mat_Cov <- cov(Meteo_Moy_Ecart)

# --- 6. Eigen decomposition --
# Extract eigenvalues and eigenvectors from the covariance matrix
eig_result <- eigen(Mat_Cov)
Values <- eig_result$values
Vectors <- eig_result$vectors

# --- 7. Display results ---
cat("=== Covariance Matrix ===\n")
print(Mat_Cov)

cat("\n=== Eigenvalues ===\n")
print(Values)

cat("\n=== Eigenvectors ===\n")
print(Vectors)

# --- 8. (Optional) PCA using the built-in R function ---
# This step checks the results using R’s built-in PCA function
acp <- prcomp(Meteo, scale. = TRUE)

# Summary of variance explained by each principal component
summary(acp)

# --- 9. Visualization ---
# Scree plot: shows the variance explained by each component
plot(acp, main = "Variance Explained by Principal Components")

# Biplot: displays variables and observations in the PCA space
biplot(acp, main = "PCA Biplot - Meteorological Data")
