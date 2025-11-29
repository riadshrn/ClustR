cat("=== INSTALLATION ClustDeepVar WINDOWS (TensorFlow 2.10) ===\n\n")

# 1️⃣ Charger reticulate
cat("➡ Loading reticulate...\n")
library(reticulate)

# 2️⃣ Créer ou utiliser l'environnement Python
cat("➡ Using virtualenv 'r-tensorflow'...\n")
use_virtualenv("r-tensorflow", required = TRUE)

# 3️⃣ Installer TensorFlow 2.10 + numpy 1.26
cat("➡ Installing TensorFlow 2.10 and numpy<2 inside r-tensorflow...\n")
py_install(
  packages = c("numpy<2", "tensorflow==2.10.0"),
  envname  = "r-tensorflow",
  pip      = TRUE
)

# 4️⃣ Charger tensorflow dans cet env
cat("➡ Loading TensorFlow in R...\n")
library(tensorflow)

# ATTENTION : un premier appel retourne parfois une erreur fausse,
# on le rappelle une deuxième fois automatiquement.
try(tf$constant(1), silent = TRUE)
print(tf$constant(1))

cat("✔ TensorFlow operational.\n\n")

# 5️⃣ Charger keras3 et activer backend TF
cat("➡ Loading keras3...\n")
library(keras3)
keras3::use_backend("tensorflow")

# Vérifier numpy version
cat("➡ Checking numpy version...\n")
py_run_string("import numpy as np; print('NumPy:', np.__version__)")

# 6️⃣ Test complet ClustDeepVar
cat("\n➡ Testing ClustDeepVar...\n")
library(R6)
source("R/ClustDeepVar.R")

set.seed(42)
X <- matrix(rnorm(100 * 10), nrow = 100)
colnames(X) <- paste0("V", 1:10)

model <- ClustDeepVar$new(n_clusters = 3, epochs = 5)
model$fit(X, verbose = 1)

cat("\n=== INSTALLATION TERMINÉE AVEC SUCCÈS 🎉 ===\n")
cat("TensorFlow backend OK, keras3 OK, ClustDeepVar OK.\n")


# Installation sous Windows
#library(reticulate)
#use_virtualenv("r-tensorflow", required = TRUE)
#py_install(c("numpy<2", "tensorflow==2.10.0"), pip=TRUE, envname="r-tensorflow")

#library(tensorflow); tf$constant(1)
#library(keras3); keras3::use_backend("tensorflow")

#source("R/ClustDeepVar.R")
