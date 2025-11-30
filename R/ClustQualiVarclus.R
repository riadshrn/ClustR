#' VARCLUSQuali : Clustering de variables qualitatives basé sur l’ACM (MCA)
#'
#' Cette classe implémente un algorithme VARCLUS pour données qualitatives,
#' utilisant des analyses des correspondances multiples (ACM) cluster-par-cluster
#' et la maximisation du coefficient \eqn{\eta^2} entre chaque variable et l’axe 1
#' du cluster.
#'
#' L’algorithme itère entre :
#' \enumerate{
#'   \item l’ajustement d’un MCA dans chaque cluster,
#'   \item l’affectation des variables au cluster maximisant \eqn{\eta^2}.
#' }
#'
#' Il gère :
#' \itemize{
#'   \item la sélection automatique du nombre optimal de clusters,
#'   \item les clusters vides,
#'   \item des visualisations avancées (inerties, heatmap, tailles, etc.).
#' }
#'
#' @section Champs publics :
#' \describe{
#'   \item{n_clusters}{Nombre de clusters (NULL = sélection automatique).}
#'   \item{max_iter}{Nombre d’itérations maximum.}
#'   \item{seed}{Graine aléatoire.}
#'   \item{verbose}{Affichage des messages.}
#'   \item{fitted}{TRUE si le modèle est entraîné.}
#'   \item{clusters}{Vecteur : variable → cluster.}
#'   \item{cluster_mca}{Liste des objets \code{FactoMineR::MCA}.}
#'   \item{eta2_matrix}{Matrice des \eqn{\eta^2}.}
#' }
#'
#' @section Méthodes :
#' \describe{
#'   \item{\code{new(n_clusters = NULL, max_iter = 20, seed = 123, verbose = TRUE)}}{Constructeur.}
#'
#'   \item{\code{fit(X)}}{
#'     Ajuste le modèle sur un data.frame de variables qualitatives.
#'     Si \code{n_clusters = NULL}, sélection automatique du meilleur \code{k}.
#'   }
#'
#'   \item{\code{predict(X_new)}}{
#'     Affecte de nouvelles variables à des clusters existants,
#'     en utilisant les axes MCA du modèle ajusté.
#'   }
#'
#'   \item{\code{summary()}}{
#'     Affiche un résumé complet : affectation, tailles, matrice \eqn{\eta^2}.
#'   }
#'
#'   \item{\code{plot_k_selection_pro()}}{
#'     Graphique professionnel de la sélection de k.
#'   }
#'
#'   \item{\code{plot_cluster_sizes_pro()}}{
#'     Barplot des tailles de clusters.
#'   }
#'
#'   \item{\code{plot_eta2_heatmap_pro2(show_values = FALSE)}}{
#'     Heatmap de \eqn{\eta^2} variable × cluster.
#'   }
#'
#'   \item{\code{plot_inertia_pro()}}{
#'     Barplot de l'inertie expliquée par l’axe 1 de chaque cluster.
#'   }
#' }
#'
#' @return Un objet R6 de classe \code{VARCLUSQuali}.
#'
#' @seealso \code{\link{select_k_VARCLUSQuali}}
#'
#' @examples
#' \dontrun{
#' library(readxl)
#' data <- read_excel("cars.xlsx")
#' model <- select_k_VARCLUSQuali$new(n_clusters = NULL, max_iter = 30, seed = 42, verbose = TRUE)
#' model$fit(data)
#' model$print()
#' model$summary()
#' pred <- model$predict(data)
#' head(pred$eta2)
#' model$plot_eta2_heatmap()
#' model$plot_cluster_sizes()
#' model$plot_k_selection()
#' }

#' @export

# Dépendances
library(R6)
library(FactoMineR)    # pour MCA (ACM)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
#library(readxl)

# -------------------------------
# Sélection du nombre optimal de clusters pour VARCLUSQuali (version corrigée)
# -------------------------------
select_k_VARCLUSQuali <- function(data, k_min = 2, k_max = 8, max_iter = 20, seed = 123, verbose = TRUE) {
  if (k_min < 2) stop("k_min doit être >= 2.")
  if (k_max <= k_min) stop("k_max doit être > k_min.")

  # détecte les variables qualitatives
  quali <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  if (length(quali) == 0) stop("Aucune variable qualitative détectée dans data.")

  data <- data %>% dplyr::mutate(across(all_of(quali), ~ as.factor(.x)))

  # attention à la longueur : k_max - k_min + 1
  ks <- seq(k_min, k_max)
  scores <- numeric(length(ks))
  names(scores) <- as.character(ks)

  for (idx in seq_along(ks)) {
    k <- ks[idx]
    if (verbose) message("Test de k = ", k, " ...")

    # entraîner un modèle rapide pour ce k (verbose = FALSE pour éviter bruit)
    model_k <- ClustQualiVarclus$new(n_clusters = k, max_iter = max_iter, seed = seed, verbose = FALSE)
    model_k$fit(data)

    # construire inertias et n_vars_cluster précisément pour j in 1:k
    inertias <- numeric(k)
    n_vars_cluster <- integer(k)
    for (j in 1:k) {
      # nombre de variables assignées au cluster j
      n_vars_cluster[j] <- sum(model_k$clusters == j, na.rm = TRUE)
      m <- NULL
      if (!is.null(model_k$cluster_mca) && length(model_k$cluster_mca) >= j) {
        m <- model_k$cluster_mca[[j]]
      }
      # sécurité : si MCA invalide, inertie 0
      if (is.null(m) || is.null(m$eig) || nrow(m$eig) == 0 || is.na(m$eig[1,2])) {
        inertias[j] <- 0
      } else {
        eig <- m$eig
        # proportion (axe1) ; somme avec na.rm par sécurité
        denom <- sum(eig[, 2], na.rm = TRUE)
        if (denom <= 0 || is.na(eig[1, 2])) inertias[j] <- 0 else inertias[j] <- eig[1, 2] / denom
      }
    }

    # score pondéré (pondération par nb de variables dans le cluster)
    total_vars <- sum(n_vars_cluster)
    if (total_vars > 0) {
      score_k <- sum(inertias * n_vars_cluster) / total_vars
    } else {
      score_k <- 0
    }
    scores[idx] <- score_k
  }

  # sécuriser les NA (défensive)
  scores[is.na(scores)] <- 0

  df_scores <- data.frame(k = ks, score = scores)


  # sélection automatique : heuristique du coude (conservative)
  deltas <- diff(df_scores$score)
  # seuil relatif (on garde 0.05 du score du k_min ou min 1e-6)
  seuil <- max(1e-8, 0.05 * df_scores$score[1])
  idx_cut <- which(abs(deltas) < seuil)[1]
  if (!is.na(idx_cut)) {
    k_opt <- df_scores$k[idx_cut]
  } else {
    # si aucun "coude", choisir k qui maximise le score
    k_opt <- df_scores$k[which.max(df_scores$score)]
  }

  if (verbose) message("\n>>> Nombre optimal de clusters (approximatif) : k = ", k_opt)
  return(list(k_opt = k_opt, table_scores = df_scores))
}

# Classe ClustQualiVarclus (R6)
ClustQualiVarclus  <- R6Class("ClustQualiVarclus",
                              public = list(
                                n_clusters = NULL,
                                max_iter = NULL,
                                seed = NULL,
                                verbose = NULL,
                                fitted = FALSE,
                                scores_k_selection = NULL,

                                # données internes
                                data = NULL,               # data.frame factorisé utilisé pour fit
                                cols_quali = NULL,         # noms des variables qualitatives
                                clusters = NULL,           # vecteur de longueur p (p = nb variables) -> cluster id
                                cluster_mca = NULL,        # liste d'objets MCA (FactoMineR) par cluster (fit)
                                cluster_axis1 = NULL,      # liste des vecteurs axis1 (ind coords) par cluster (num obs)
                                eta2_matrix = NULL,        # matrice p x k des eta2 (variable x cluster)


                                #' @description
                                #' Constructeur de l'objet VARCLUSQuali
                                #' @param n_clusters Nombre de clusters (NULL = sélection automatique)
                                #' @param max_iter Nombre d'itérations max
                                #' @param seed Graine aléatoire
                                #' @param verbose Affiche les messages
                                initialize = function(n_clusters = NULL, max_iter = 20, seed = 123,
                                                      verbose = TRUE, k_max = NULL) {  # ← AJOUT
                                  self$n_clusters <- if (!is.null(n_clusters)) as.integer(n_clusters) else NULL
                                  self$max_iter <- as.integer(max_iter)
                                  self$seed <- seed
                                  self$verbose <- verbose

                                  # Stocker k_max pour utilisation dans fit()
                                  private$k_max <- if (!is.null(k_max)) as.integer(k_max) else NULL  # ← AJOUT
                                },

                                # util: calcule eta^2 entre une variable factor et un vecteur numérique y (axis scores)
                                .eta2_var_numeric = function(var_factor, y) {
                                  # var_factor : factor (length n), y : numeric (length n)
                                  # eta^2 = SSB / SST  (between sum of squares / total sum squares)
                                  if (length(var_factor) != length(y)) stop("Lengths mismatch in .eta2_var_numeric")
                                  y <- as.numeric(y)
                                  df <- data.frame(y = y, g = as.factor(var_factor))
                                  ss_total <- sum((df$y - mean(df$y))^2)
                                  if (ss_total == 0) return(0)
                                  means_by_g <- tapply(df$y, df$g, mean)
                                  n_by_g <- as.numeric(table(df$g))
                                  ss_between <- sum(n_by_g * (means_by_g - mean(df$y))^2)
                                  eta2 <- ss_between / ss_total
                                  return(as.numeric(eta2))
                                },

                                # util: safe MCA on a subset of variables (returns MCA object). Uses FactoMineR::MCA
                                # util: safe MCA on a subset of variables (returns MCA object). Uses FactoMineR::MCA
                                .run_mca = function(df_subset) {
                                  # ensure factors and at least 1 variable
                                  if (ncol(df_subset) == 0) stop("Empty subset in .run_mca")

                                  # FactoMineR::MCA requires at least 2 variables
                                  # If only 1 variable, create a mock MCA result
                                  if (ncol(df_subset) == 1) {
                                    cat("  ⚠️ Cluster avec 1 seule variable, MCA simulée\n")

                                    # Créer une indicatrice pour chaque niveau
                                    var_data <- df_subset[[1]]
                                    levs <- levels(as.factor(var_data))
                                    n <- nrow(df_subset)

                                    # Scores individus = 0 pour axe 1 (pas de structure)
                                    ind_coord <- matrix(0, nrow = n, ncol = 1)
                                    colnames(ind_coord) <- "Dim.1"
                                    rownames(ind_coord) <- rownames(df_subset)

                                    # Valeur propre = 0 (pas de variance)
                                    eig <- matrix(c(0, 0, 0), nrow = 1, ncol = 3)
                                    colnames(eig) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
                                    rownames(eig) <- "Dim.1"

                                    # Mock result
                                    mock_mca <- list(
                                      ind = list(coord = ind_coord),
                                      eig = eig
                                    )

                                    return(mock_mca)
                                  }

                                  # Cas normal : au moins 2 variables
                                  res <- FactoMineR::MCA(df_subset, graph = FALSE, ncp = 5)
                                  return(res)
                                },



                                #' @description
                                #' Ajuste le modèle sur un data.frame de variables qualitatives
                                #' @param X Data.frame avec variables qualitatives
                                #' @return self
                                fit = function(X) {
                                  set.seed(self$seed)
                                  X <- as.data.frame(X)

                                  # détecte variables qualitatives
                                  quali <- names(X)[sapply(X, function(x) is.factor(x) || is.character(x))]
                                  if (length(quali) == 0) stop("Aucune variable qualitative détectée dans X.")

                                  # convertir en factors
                                  X_quali <- X %>% mutate(across(all_of(quali), ~ as.factor(.x)))
                                  self$data <- X_quali
                                  self$cols_quali <- quali

                                  p <- length(quali)
                                  k <- self$n_clusters

                                  # -------------------------------------------------------------------
                                  # Sélection automatique du nombre optimal de clusters si NULL
                                  # -------------------------------------------------------------------
                                  if (is.null(self$n_clusters)) {
                                    if (self$verbose) message("n_clusters non spécifié → sélection automatique via méthode du coude...")

                                    k_max_value <- if (!is.null(private$k_max)) {
                                      private$k_max
                                    } else {
                                      min(10, length(quali))
                                    }

                                    res_k <- select_k_VARCLUSQuali(
                                      data = X_quali,
                                      k_min = 2,
                                      k_max = k_max_value,
                                      max_iter = 10,
                                      seed = self$seed,
                                      verbose = self$verbose
                                    )
                                    self$scores_k_selection <- res_k$table_scores

                                    self$n_clusters <- res_k$k_opt
                                    if (self$verbose) message("Nombre optimal sélectionné : k = ", self$n_clusters)
                                  }

                                  k <- self$n_clusters

                                  # -------------------------------------------------------------------
                                  # Initialisation : affectation aléatoire équilibrée
                                  # -------------------------------------------------------------------
                                  clusters_init <- rep(1:k, length.out = p)
                                  clusters_init <- sample(clusters_init, size = p, replace = FALSE)
                                  names(clusters_init) <- quali
                                  self$clusters <- clusters_init

                                  iter <- 1
                                  changed <- TRUE

                                  if (self$verbose) message("Début de l'algorithme VARCLUS-Q (réallocation)")

                                  while (iter <= self$max_iter && changed) {
                                    if (self$verbose) message("Itération ", iter)
                                    changed <- FALSE

                                    # -------------------------------------------------------------------
                                    # MCA par cluster et extraction de l’axe 1
                                    # -------------------------------------------------------------------
                                    cluster_mca <- vector("list", k)
                                    cluster_axis1 <- vector("list", k)

                                    for (j in 1:k) {
                                      vars_j <- names(self$clusters)[self$clusters == j]

                                      if (length(vars_j) == 0) {
                                        cluster_mca[[j]] <- NULL
                                        cluster_axis1[[j]] <- rep(0, nrow(self$data))
                                        next
                                      }

                                      subset_df <- self$data[, vars_j, drop = FALSE]
                                      mca_j <- self$.run_mca(subset_df)
                                      axis1_scores <- mca_j$ind$coord[, 1]

                                      cluster_mca[[j]] <- mca_j
                                      cluster_axis1[[j]] <- axis1_scores
                                    }

                                    # -------------------------------------------------------------------
                                    # Calcul η² (p × k)
                                    # -------------------------------------------------------------------
                                    eta2_mat <- matrix(
                                      NA, nrow = p, ncol = k,
                                      dimnames = list(quali, paste0("C", 1:k))
                                    )

                                    for (v in seq_along(quali)) {
                                      varname <- quali[v]
                                      varvec <- self$data[[varname]]

                                      for (j in 1:k) {
                                        axis1 <- cluster_axis1[[j]]

                                        if (is.null(axis1)) {
                                          eta2_mat[v, j] <- 0
                                        } else {
                                          eta2_mat[v, j] <- self$.eta2_var_numeric(varvec, axis1)
                                        }
                                      }
                                    }

                                    # -------------------------------------------------------------------
                                    # Réaffectation brute : variable → cluster max η²
                                    # -------------------------------------------------------------------
                                    new_clusters <- apply(eta2_mat, 1, which.max)
                                    names(new_clusters) <- quali

                                    # -------------------------------------------------------------------
                                    # Gestion des clusters vides
                                    # -------------------------------------------------------------------
                                    new_clusters_tmp <- new_clusters
                                    cluster_sizes <- table(factor(new_clusters_tmp, levels = 1:k))

                                    empty_clusters <- which(cluster_sizes == 0)

                                    if (length(empty_clusters) > 0) {
                                      for (c in empty_clusters) {

                                        # Score de la variable vis-à-vis de son cluster actuel
                                        score_var <- numeric(p)
                                        names(score_var) <- quali

                                        for (v in seq_along(quali)) {
                                          varname <- quali[v]
                                          c_old <- new_clusters_tmp[varname]
                                          score_var[varname] <- eta2_mat[varname, c_old]
                                        }

                                        # Variable la moins bien représentée
                                        var_to_move <- names(which.min(score_var))

                                        # Réassignation dans le cluster vide
                                        new_clusters_tmp[var_to_move] <- c
                                      }
                                    }

                                    # On remplace par la version corrigée
                                    new_clusters <- new_clusters_tmp

                                    # -------------------------------------------------------------------
                                    # Test convergence
                                    # -------------------------------------------------------------------
                                    if (!identical(as.integer(new_clusters), as.integer(self$clusters))) {
                                      changed <- TRUE
                                      self$clusters <- new_clusters
                                    }

                                    # Sauvegarde infos
                                    self$cluster_mca <- cluster_mca
                                    self$cluster_axis1 <- cluster_axis1
                                    self$eta2_matrix <- eta2_mat

                                    iter <- iter + 1
                                  }

                                  self$fitted <- TRUE
                                  if (self$verbose) message("Fit terminé après ", iter - 1, " itérations.")
                                  invisible(self)
                                },

                                #' @description
                                #' Affecte de nouvelles variables à des clusters existants
                                #' @param X_new Data.frame avec nouvelles variables qualitatives
                                #' @return Liste avec assignment et matrice eta2
                                predict = function(X_new) {
                                  if (!self$fitted) stop("Le modèle doit être ajusté avec $fit() avant $predict().")

                                  X_new <- as.data.frame(X_new)

                                  quali_new <- names(X_new)[sapply(X_new, function(x) is.factor(x) || is.character(x))]
                                  if (length(quali_new) == 0) stop("Aucune variable qualitative détectée dans X_new.")

                                  X_new <- X_new %>% mutate(across(all_of(quali_new), ~ as.factor(.x)))

                                  k <- self$n_clusters
                                  p_new <- length(quali_new)

                                  # UTILISE les axes1 du fit() !
                                  axis1_fitted <- self$cluster_axis1

                                  eta2_mat_new <- matrix(NA, nrow = p_new, ncol = k,
                                                         dimnames = list(quali_new, paste0("C", 1:k)))

                                  for (v in seq_along(quali_new)) {
                                    varname <- quali_new[v]
                                    varvec <- X_new[[varname]]

                                    for (j in 1:k) {
                                      axis1 <- axis1_fitted[[j]]
                                      eta2_mat_new[v,j] <- self$.eta2_var_numeric(varvec, axis1)
                                    }
                                  }

                                  assign_new <- apply(eta2_mat_new, 1, which.max)
                                  names(assign_new) <- quali_new

                                  return(list(assignment = assign_new, eta2 = eta2_mat_new))
                                },


                                #' @description
                                #' Get cluster assignments (compatibility with VariableClustering)
                                #'
                                #' @return Named vector of cluster assignments
                                get_clusters = function() {
                                  if (!self$fitted) {
                                    stop("Model not fitted. Call $fit() first.")
                                  }
                                  return(self$clusters)
                                },


                                #' @description
                                #' Print compact du modèle
                                print = function(...) {
                                  cat("=== VARCLUSQuali Model ===\n")
                                  cat("n_clusters :", self$n_clusters, "\n")
                                  cat("max_iter :", self$max_iter, "\n")
                                  cat("fitted :", self$fitted, "\n")
                                  if (self$fitted) {
                                    counts <- table(self$clusters)
                                    cat("Variables par cluster :\n")
                                    print(counts)
                                  }
                                },

                                #' @description
                                #' Résumé détaillé du clustering
                                summary = function() {
                                  if (!self$fitted) stop("Le modèle doit être ajusté avec $fit() avant $summary().")
                                  cat("=== Résumé VARCLUSQuali ===\n\n")
                                  cat("Nombre de clusters :", self$n_clusters, "\n")
                                  cat("Nombre de variables qualitatives :", length(self$cols_quali), "\n\n")

                                  cat("-> Affectation des variables (variable : cluster) :\n")
                                  print(self$clusters)

                                  cat("\n-> Matrice eta^2 (variables x clusters) : (valeurs entre 0 et 1)\n")
                                  print(round(self$eta2_matrix, 3))

                                  # tableau des variables par cluster
                                  cat("\n-> Composition des clusters :\n")
                                  for (j in 1:self$n_clusters) {
                                    vars_j <- names(self$clusters)[self$clusters == j]
                                    cat("\nCluster", j, " (", length(vars_j), " variables ) :\n", sep = "")
                                    if (length(vars_j) > 0) cat(paste0("  - ", vars_j, collapse = "\n"), "\n") else cat("  <vide>\n")
                                  }

                                },

                                theme = function() {
                                  theme_minimal(base_size = 13) +
                                    theme(
                                      plot.title = element_text(face = "bold", size = 16, hjust = 0),
                                      plot.subtitle = element_text(size = 12, hjust = 0),
                                      axis.title = element_text(face = "bold"),
                                      axis.text = element_text(color = "#333333"),
                                      legend.position = "right",
                                      panel.grid.major = element_line(color = "#dddddd", linewidth = 0.3),
                                      panel.grid.minor = element_blank()
                                    )
                                },

                                #' @description
                                #' Graphique de la sélection de k
                                plot_k_selection = function() {
                                  model <- self
                                  df_scores <- model$scores_k_selection
                                  stopifnot(!is.null(df_scores))
                                  k_opt <- model$n_clusters
                                  stopifnot(!is.null(k_opt))

                                  ggplot(df_scores, aes(x = k, y = score)) +
                                    geom_line(size = 1, color = "#1f77b4") +
                                    geom_point(size = 4, color = "#1663b7") +
                                    geom_vline(xintercept = k_opt, linetype = "dashed", color = "black", size = 1) +
                                    annotate("text", x = k_opt, y = max(df_scores$score) * 0.95,
                                             label = paste0("k_opt = ", k_opt), color = "black", fontface = "bold", hjust = -0.1) +
                                    labs(
                                      title = "Méthode du coude — Score pondéré",
                                      subtitle = "Inertie de l’axe 1 pondérée par le nombre de variables",
                                      x = "Nombre de clusters (k)",
                                      y = "Score"
                                    ) +
                                    model$theme()
                                },
                                # Palette utilitaire (colorblind-friendly-ish)
                                varclus_palette = function(k) {
                                  colors <- RColorBrewer::brewer.pal(min(8, max(3, k)), "Set2")
                                  if (k > length(colors)) colors <- grDevices::colorRampPalette(colors)(k)
                                  colors
                                },

                                #' @description
                                #' Barplot des tailles de clusters
                                plot_cluster_sizes = function() {
                                  model <- self
                                  stopifnot(!is.null(model$fitted) && model$fitted)
                                  df <- data.frame(variable = names(model$clusters),
                                                   cluster = factor(model$clusters))
                                  df2 <- df %>% count(cluster) %>% mutate(cluster = factor(cluster, levels = cluster))
                                  ggplot(df2, aes(x = cluster, y = n, fill = cluster)) +
                                    geom_col(width = 0.7, show.legend = FALSE) +
                                    geom_text(aes(label = n), vjust = -0.5) +
                                    scale_fill_manual(values = model$varclus_palette(nrow(df2))) +
                                    labs(title = "Composition des clusters (nombre de variables)",
                                         x = "Cluster", y = "Nombre de variables") +
                                    model$theme()
                                },

                                #' @description
                                #' Heatmap des eta2 (variables x clusters)
                                #' @param show_values Affiche les valeurs dans les cases
                                plot_eta2_heatmap = function( show_values = FALSE) {
                                  model <- self
                                  stopifnot(!is.null(model$fitted) && model$fitted)
                                  eta_df <- as.data.frame(model$eta2_matrix) %>%
                                    tibble::rownames_to_column("variable") %>%
                                    pivot_longer(-variable, names_to = "cluster", values_to = "eta2")
                                  pal <- rev(RColorBrewer::brewer.pal(9, "Blues"))
                                  p <- ggplot(eta_df, aes(x = cluster, y = variable, fill = eta2)) +
                                    geom_tile(color = "white") +
                                    scale_fill_gradientn(colours = pal, limits = c(0, 1), na.value = "grey90") +
                                    labs(title = "Heatmap des η² (variable × cluster)", x = "Cluster", y = "Variable") +
                                    model$theme() +
                                    theme(axis.text.y = element_text(size = 7))
                                  if (show_values) p <- p + geom_text(aes(label = round(eta2, 2)), size = 2.5)
                                  p
                                },

                                #' @description
                                #' Soft membership visualisation (stacked bar)
                                #' @param top_n Nombre de variables à afficher
                                plot_soft_membership_stacked = function(top_n = 30) {
                                  model <- self
                                  stopifnot(!is.null(model$fitted) && model$fitted)
                                  eta <- as.data.frame(model$eta2_matrix)
                                  eta$variable <- rownames(eta)
                                  df <- eta %>% pivot_longer(-variable, names_to = "cluster", values_to = "eta2")
                                  # option pour limiter au top_n variables (par eta2 max)
                                  top_vars <- df %>% group_by(variable) %>% summarize(max_eta = max(eta2, na.rm = TRUE)) %>%
                                    arrange(desc(max_eta)) %>% head(top_n) %>% pull(variable)
                                  df2 <- df %>% filter(variable %in% top_vars)
                                  df2$variable <- factor(df2$variable, levels = rev(unique(df2$variable)))
                                  pal <- .varclus_palette(length(unique(df2$cluster)))
                                  ggplot(df2, aes(x = variable, y = eta2, fill = cluster)) +
                                    geom_col(position = "fill") +
                                    coord_flip() +
                                    scale_fill_manual(values = pal) +
                                    labs(title = paste0("Soft membership (η² normalisé) — top ", top_n, " variables"),
                                         x = "", y = "Proportion (normalisée)") +
                                    model$theme()
                                },

                                #' @description
                                #' Visualisation des inerties principales (axe 1) par cluster
                                plot_inertia = function() {
                                  if (!self$fitted) stop("Appeler $fit() avant plot_inertia().")

                                  inertias <- sapply(self$cluster_mca, function(m) {
                                    if (is.null(m)) return(0)
                                    eig <- m$eig
                                    eig[1, 2] / sum(eig[, 2])
                                  })

                                  inert_df <- data.frame(
                                    cluster = paste0("C", seq_along(inertias)),
                                    inertia_axis1 = inertias
                                  )

                                  ggplot(inert_df, aes(x = cluster, y = inertia_axis1, fill = cluster)) +
                                    geom_col(width = 0.6, show.legend = FALSE) +
                                    geom_text(aes(label = scales::percent(inertia_axis1, accuracy = 1)),
                                              vjust = -0.5, size = 3.5, fontface = "bold") +
                                    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                                       limits = c(0, max(inertias)*1.2)) +
                                    scale_fill_brewer(palette = "Set2") +
                                    labs(title = "Inertie proportionnelle expliquée par l'axe 1",
                                         x = "Cluster", y = "Proportion") +
                                    self$theme() +
                                    theme(axis.text.x = element_text(face = "bold", size = 11),
                                          axis.text.y = element_text(face = "bold", size = 11),
                                          plot.title = element_text(size = 16, hjust = 0.5))
                                }


                              ),

                              private = list(
                                k_max = NULL
                              )
)
