############################################################
#       SETUP COMPLET R + TENSORFLOW + KERAS + CLUSTR
#       Compatible R 4.5.2 et Python 3.10
############################################################

cat("=== INSTALLATION DES PACKAGES R ===\n")

install.packages(c(
  "devtools","shiny","R6","ggplot2","dplyr","tidyr",
  "cluster","factoextra","plotly","DT","pheatmap",
  "reticulate","keras3","torch", "shinyWidgets"
))

devtools::install_github("riadshrn/ClustR")


############################################################
#  CONFIGURATION PYTHON 3.10 POUR TENSORFLOW
############################################################

cat("=== CHARGEMENT DE RETICULATE ===\n")
library(reticulate)

# 🔧 MODIFIER CE CHEMIN SI TON PYTHON 3.10 EST AILLEURS
python310 <- "C:/.../Python/Python310/python.exe"

cat("=== CREATION DE L'ENVIRONNEMENT VIRTUALENV r-tensorflow ===\n")

if (!virtualenv_exists("r-tensorflow")) {
  virtualenv_create(
    envname = "r-tensorflow",
    python = python310
  )
}

use_virtualenv("r-tensorflow", required = TRUE)

cat("=== CONFIGURATION ACTUELLE DE PYTHON ===\n")
print(py_config())


############################################################
# INSTALLATION DES MODULES PYTHON (TensorFlow + Keras)
############################################################

cat("=== INSTALLATION DE TENSORFLOW ET KERAS ===\n")
virtualenv_install(
  "r-tensorflow",
  packages = c("tensorflow", "keras")
)


############################################################
# TEST DE TENSORFLOW
############################################################

cat("=== TEST DE TENSORFLOW ===\n")
library(tensorflow)
library(keras3)

keras3::use_backend("tensorflow")

test <- try(tf$constant(1), silent = TRUE)
print(test)


############################################################
# LANCEMENT DU PACKAGE ClustR
############################################################

cat("=== LANCEMENT DE L'APPLICATION SHINY CLUSTR ===\n")

library(shiny)
library(ClustR)
library(shinyWidgets)

# Décommente cette ligne pour lancer automatiquement l'app #
shiny::runApp(system.file("shiny-app", package = "ClustR"))

cat("=== SETUP TERMINÉ AVEC SUCCÈS ===\n")
