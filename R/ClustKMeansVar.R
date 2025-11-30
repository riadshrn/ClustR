#' VariableKMeansClust: Variable clustering using correlation (PC1-based K-means)
#'
#' @description
#' R6 class implementing a variable clustering algorithm based on
#' the approach of Vigneau & Qannari (2003).
#'
#' Each cluster is represented by the **first principal component (PC1)**
#' computed from the variables it contains.
#' Variables are reallocated by maximizing the squared correlation with the PC1
#' of each cluster.
#' The distance used is:
#'
#' \deqn{d(j, k) = 1 - \mathrm{cor}(X_j, \mathrm{PC1}_k)^2}
#'
#' @details
#'
#' **General principle:**
#'
#' 1. Standardization of variables (optional).
#' 2. Random initialization of a variable partition.
#' 3. For each cluster, computation of the PC1 from its variables
#'    (or copy of the variable if the cluster contains only one).
#' 4. Reallocation of variables by maximizing \eqn{\mathrm{cor}^2}.
#' 5. Repeat until convergence.
#'
#' **Advantages:**
#'
#' - groups **correlated variables**;
#' - directly interpretable structure (PC1 = main direction of the cluster);
#' - fast, robust, unsupervised method;
#' - a standard approach in variable analysis (see Vigneau & Qannari 2003).
#'
#' **Available outputs:**
#' - Final variable clusters
#' - Centroids = PC1 of each cluster
#' - Inertia values W (within), B (between), T (total) and Q = B/T
#' - Qannari-specific silhouette index
#' - Visualizations: global PCA, cluster PCA, heatmap, dendrogram
#'
#' @section Method:
#' For a cluster \eqn{k}, PC1 maximizes the common variance of the variables in the cluster.
#' Each variable is then reassigned to the cluster that maximizes
#' the squared correlation with the centroid (PC1).
#'
#' @field n_clusters Number of clusters.
#' @field max_iter Maximum number of reallocative K-means iterations.
#' @field tol Not used (compatibility).
#' @field scale Boolean: TRUE = standardize variables.
#'
#' @field var_names Names of numerical variables.
#' @field n_obs Number of observations.
#' @field means Means used for standardization.
#' @field sds Standard deviations used for standardization.
#' @field data_raw Centered/scaled data (numeric matrix).
#'
#' @field cluster_assignments Vector of final clusters (length = number of variables).
#' @field centers Matrix (observations x clusters) containing cluster PC1 vectors.
#' @field iter Number of performed iterations.
#' @field convergence Boolean indicating convergence of the procedure.
#'
#' @field W_k Within-cluster inertia.
#' @field W_total Total within-cluster inertia.
#' @field B_total Total between-cluster inertia.
#' @field T_total Total inertia.
#' @field Q Ratio B/T (overall partition quality).
#' @field center_distances Distance matrix between cluster centroids (PC1).
#'
#' @examples
#' \dontrun{
#' library(MASS)
#' data(Boston)
#'
#' model <- VariableKMeansClust$new(n_clusters = 3)
#' model$fit(Boston)
#' model$summary()
#'
#' # Cluster assignments
#' model$cluster_assignments
#'
#' # Silhouette
#' model$compute_silhouette(Boston)
#' model$plot_silhouette()
#'
#' # Visualizations
#' model$plot_global_pca()
#' model$plot_dendrogram()
#' model$plot_cluster_pca(1)
#' model$visualize(Boston)
#' }
#'
#' @seealso
#' Vigneau & Qannari (2003), "Clustering of variables around latent components"
#'
#'
#' @import R6
#' @import ggplot2
#' @import ggrepel
#' @import pheatmap
#' @importFrom stats cor dist prcomp sd setNames
#' @importFrom graphics plot
#' @export

#library(R6)
#library(ggplot2)
#library(pheatmap)
#library(ggrepel)

#source("utils_core.R")

ClustKMeansVar <- R6::R6Class(
  "ClustKMeansVar",
  public = list(
    # ----------------------------------------------------------------------
    # PROPRIÉTÉS
    # ----------------------------------------------------------------------
    n_clusters = NULL,
    max_iter = NULL,
    tol = NULL,
    scale = NULL,
    silhouette = NULL,

    var_names = NULL,
    n_obs = NULL,
    means = NULL,
    sds = NULL,

    data_raw = NULL,
    original_data = NULL,

    cluster_assignments = NULL,
    centers = NULL,      # PC1 de chaque cluster
    iter = NULL,
    convergence = NULL,

    # METRIQUES K-MEANS
    W_k = NULL,     # inertie intra par cluster
    W_total = NULL, # inertie intra totale
    B_total = NULL, # inertie inter totale
    T_total = NULL, # inertie totale
    Q = NULL,       # proportion B/T

    center_distances = NULL, # matrice distances entre centroïdes

    # ----------------------------------------------------------------------
    # CONSTRUCTEUR
    # ----------------------------------------------------------------------
    initialize = function(n_clusters = 3, max_iter = 100, tol = 1e-6, scale = TRUE, seed = NULL) {
      self$n_clusters <- n_clusters
      self$max_iter   <- max_iter
      self$tol        <- tol
      self$scale      <- scale
      private$seed    <- seed
    },

    # ----------------------------------------------------------------------
    # FIT : K-means for variables (Correlation Clustering) avec PC1
    # ----------------------------------------------------------------------
    fit = function(X) {
      if (!is.data.frame(X)) stop("X must be a data.frame.")

      num_vars <- sapply(X, is.numeric)
      self$original_data <- X

      if (!any(num_vars)) stop("No numeric variables found.")
      Xnum <- as.matrix(X[, num_vars, drop = FALSE])

      self$data_raw <- Xnum

      self$var_names <- colnames(Xnum)
      p              <- ncol(Xnum)
      self$n_obs     <- nrow(Xnum)

      # ------------------------------------------------------------------
      # STANDARDISATION
      # ------------------------------------------------------------------
      if (self$scale) {
        self$means <- colMeans(Xnum)
        self$sds   <- apply(Xnum, 2, sd)
        self$sds[self$sds == 0] <- 1
        Z <- scale(Xnum, center = self$means, scale = self$sds)
      } else {
        Z <- Xnum
        self$means <- rep(0, p)
        self$sds   <- rep(1, p)
      }

      # ------------------------------------------------------------------
      # INITIALISATION ALEATOIRE
      # ------------------------------------------------------------------
      if (!is.null(private$seed)) set.seed(private$seed)
      clusters <- sample(1:self$n_clusters, size = p, replace = TRUE)
      while (length(unique(clusters)) < self$n_clusters) {
        clusters <- sample(1:self$n_clusters, size = p, replace = TRUE)
      }

      # ------------------------------------------------------------------
      # BOUCLE K-MEANS RÉALLOCATIF
      # ------------------------------------------------------------------
      iter <- 0
      convergence <- FALSE

      repeat {
        iter <- iter + 1
        old_clusters <- clusters

        # --------------------------------------------------------------
        # 1. Calcul centroïdes = PC1 de chaque cluster
        # --------------------------------------------------------------
        centers <- matrix(0, nrow = self$n_obs, ncol = self$n_clusters)

        for (k in 1:self$n_clusters) {
          vars_in <- which(clusters == k)

          if (length(vars_in) == 1) {
            centers[, k] <- Z[, vars_in]
          } else {
            centers[, k] <- prcomp(Z[, vars_in, drop = FALSE])$x[, 1]
          }
        }

        self$centers <- centers

        # --------------------------------------------------------------
        # 2. Réallocation : cluster = argmax corr^2(Xj, PC1k)
        # --------------------------------------------------------------
        clusters_new <- sapply(1:p, function(j) {
          cors  <- apply(centers, 2, function(mu_k) cor(Z[, j], mu_k))
          cors2 <- cors^2
          which.max(cors2)
        })

        clusters <- clusters_new

        # --------------------------------------------------------------
        # TEST CONVERGENCE
        # --------------------------------------------------------------
        if (all(clusters == old_clusters) || iter >= self$max_iter) {
          convergence <- all(clusters == old_clusters)
          break
        }
      }



      # Stockage
      self$iter               <- iter
      self$convergence        <- convergence
      self$cluster_assignments <- clusters
      names(self$cluster_assignments) <- self$var_names

      # ----------------------------------------------------------------------
      # CALCUL DES METRIQUES K-MEANS (version Vigneau & Qannari, 1 - r^2)
      # ----------------------------------------------------------------------
      W_k <- numeric(self$n_clusters)

      for (k in 1:self$n_clusters) {
        vars_in <- which(clusters == k)
        mu      <- self$centers[, k]

        # r_jk = cor(X_j, PC1_k)
        r_vec <- sapply(vars_in, function(j) cor(Z[, j], mu))
        # distance "squared" = 1 - r_jk^2
        dists <- 1 - r_vec^2
        W_k[k] <- sum(dists, na.rm = TRUE)
      }

      self$W_k     <- W_k
      self$W_total <- sum(W_k)

      # ----------------------------------------------------------------------
      # INERTIE INTER & TOTALE (Vigneau & Qannari)
      # ----------------------------------------------------------------------
      self$T_total <- p                 # variance totale car Z est standardisé
      self$B_total <- p - self$W_total  # inertie inter = T - W = somme des r^2
      self$Q       <- self$B_total / p  # critère Q proportionnel à la moyenne des r^2

      # distances entre centroïdes (distance euclidienne sur les scores PC1)
      self$center_distances <- as.matrix(dist(t(self$centers)))

      cat("K-means for Variables (PC1, distance 1 - r^2) terminé en", iter,
          ifelse(convergence, "itérations (converged)", "itérations (not converged)"), "\n")
    },

    #' @description
    #' Get cluster assignments (compatibility with VariableClustering)
    #'
    #' @return Named vector of cluster assignments
    get_clusters = function() {
      if (is.null(self$cluster_assignments)) {
        stop("Model not fitted. Call $fit() first.")
      }
      return(self$cluster_assignments)
    },

    # ----------------------------------------------------------------------
    # PREDICT
    # ----------------------------------------------------------------------
    predict = function(X_new) {
      if (is.null(self$cluster_assignments))
        stop("Model must be fitted first.")

      if (nrow(X_new) != self$n_obs)
        stop("X_new must have the same number of observations.")

      num_vars <- sapply(X_new, is.numeric)
      Xnum <- as.matrix(X_new[, num_vars, drop = FALSE])

      Z <- scale(Xnum, center = self$means, scale = self$sds)

      assignments <- sapply(1:ncol(Z), function(j) {
        cors  <- apply(self$centers, 2, function(mu_k) cor(Z[, j], mu_k))
        cors2 <- cors^2
        which.max(cors2)
      })

      names(assignments) <- colnames(Xnum)
      return(assignments)
    },

    # ----------------------------------------------------------------------
    # PRINT
    # ----------------------------------------------------------------------
    print = function() {
      cat("VariableKMeansClust (K-means réallocatif, centroïde = PC1, distance 1 - r^2)\n")
      cat("Clusters :", self$n_clusters, "\n")
      if (!is.null(self$cluster_assignments)) {
        cat("Variables :", length(self$cluster_assignments), "\n")
        cat("Itérations :", self$iter, "\n")
        cat("Convergence :", self$convergence, "\n")
        cat("Inertie intra totale W :", round(self$W_total, 4), "\n")
        cat("Inertie inter totale B :", round(self$B_total, 4), "\n")
        cat("Q = B/T :", round(self$Q, 4), "\n")
      } else {
        cat("Modèle non entraîné.\n")
      }
    },

    # ----------------------------------------------------------------------
    # SUMMARY
    # ----------------------------------------------------------------------
    summary = function() {
      if (is.null(self$cluster_assignments)) {
        cat("Model not fitted yet.\n")
        return(invisible(NULL))
      }

      cat("---- VariableKMeansClust summary ----\n")
      cat("Clusters :", self$n_clusters, "\n")
      cat("Itérations :", self$iter, "\n")
      cat("Convergence :", self$convergence, "\n\n")

      cat("Inertie intra W_k par cluster :\n")
      print(self$W_k)
      cat("\nW_total =", self$W_total, "\n\n")

      cat("Inertie inter B =", self$B_total, "\n")
      cat("Inertie totale T =", self$T_total, "\n")
      cat("Critère Q = B/T =", self$Q, "\n\n")

      cat("Distances entre centroïdes :\n")
      print(self$center_distances)

      cat("\nVariables par cluster :\n")
      print(split(self$var_names, self$cluster_assignments))
    },

    # ----------------------------------------------------------------------
    # DENDROGRAMME DES CENTROÏDES
    # ----------------------------------------------------------------------
    plot_dendrogram = function() {
      if (is.null(self$cluster_assignments))
        stop("Model must be fitted first.")

      # Standardisation
      Z <- scale(as.matrix(self$data_raw), center = self$means, scale = self$sds)

      # Distance 1 - r²
      cor_mat <- cor(Z)
      dist_mat <- as.dist(1 - cor_mat^2)

      hc <- hclust(dist_mat, method = "ward.D2")

      # dendrogramme
      dend <- stats::as.dendrogram(hc)

      # Couleurs des clusters
      cols <- c("red", "blue", "darkgreen", "orange", "purple")
      cluster_cols <- cols[self$cluster_assignments][order.dendrogram(dend)]

      # Appliquer proprement les couleurs
      dend <- dendextend::set(dend, "labels_colors", cluster_cols)

      # Plot
      plot(dend, main = "Dendrogramme des variables")

      # Ajouter des rectangles
      dendextend::rect.dendrogram(dend, k = self$n_clusters, border = cols)
    },




    # ----------------------------------------------------------------------
    # SILHOUETTE (version utils_core)
    # ----------------------------------------------------------------------
    compute_silhouette_var = function(X) {
      if (is.null(self$cluster_assignments))
        stop("Model must be fitted first.")

      if (!exists("compute_silhouette_var", mode = "function"))
        stop("compute_silhouette_var() is missing. Load utils_core.R.")

      sil_mean <- compute_silhouette_var(X, self$cluster_assignments)

      self$silhouette <- rep(sil_mean, length(self$cluster_assignments))
      names(self$silhouette) <- self$var_names

      return(self$silhouette)
    },

    # ----------------------------------------------------------------------
    # PLOT SILHOUETTE
    # ----------------------------------------------------------------------
    plot_silhouette = function() {

      if (is.null(self$silhouette))
        stop("Compute silhouette first using $compute_silhouette()")

      sil_df <- data.frame(
        var        = names(self$silhouette),
        cluster    = factor(self$cluster_assignments),
        silhouette = self$silhouette,
        stringsAsFactors = FALSE
      )

      # 🔥 FIX MAJEUR : on calcule l’ordre ici, AVANT ggplot
      sil_df$var <- factor(sil_df$var, levels = sil_df$var[order(sil_df$silhouette)])

      ggplot2::ggplot(
        sil_df,
        ggplot2::aes(x = var, y = silhouette, fill = cluster)
      ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::ggtitle("Indice silhouette des variables") +
        ggplot2::ylab("Silhouette") +
        ggplot2::xlab("Variables")
    },

    # ----------------------------------------------------------------------
    # SILHOUETTE (interne, distance 1 - r^2)
    # ----------------------------------------------------------------------
    compute_silhouette = function(X = NULL) {
      if (is.null(self$cluster_assignments))
        stop("Model must be fitted first.")

      if (is.null(X)) {
        X <- self$original_data
      }

      if (is.null(X)) {
        stop("No data available. Pass X or fit the model with a data.frame.")
      }

      num_vars <- sapply(X, is.numeric)
      Xnum <- as.matrix(X[, num_vars, drop = FALSE])
      Z <- scale(Xnum, center = self$means, scale = self$sds)

      p <- ncol(Z)
      K <- self$n_clusters

      # Distance corrélative : 1 - r^2
      dist_cor <- function(a, b) {
        r <- cor(a, b)
        1 - r^2
      }

      sil <- numeric(p)

      for (j in 1:p) {
        cl_j <- self$cluster_assignments[j]
        xj   <- Z[, j]

        # a(j) : cohésion intra
        vars_in <- which(self$cluster_assignments == cl_j)
        a_j <- if (length(vars_in) > 1) {
          mean(sapply(vars_in[vars_in != j], function(l) dist_cor(xj, Z[, l])))
        } else 0

        # b(j) : meilleure cohésion avec un autre cluster
        b_j <- min(sapply(setdiff(1:K, cl_j), function(k) {
          vars_k <- which(self$cluster_assignments == k)
          mean(sapply(vars_k, function(l) dist_cor(xj, Z[, l])))
        }))

        sil[j] <- (b_j - a_j) / max(a_j, b_j)
      }

      names(sil) <- self$var_names
      self$silhouette <- sil
      self$silhouette_avg <- mean(sil, na.rm = TRUE)

      return(sil)
    },

    # ----------------------------------------------------------------------
    # PLOT TAILLES DES CLUSTERS (wrapper utils_core)
    # ----------------------------------------------------------------------
    plot_cluster_sizes = function() {
      if (is.null(self$cluster_assignments))
        stop("Model must be fitted first.")

      if (!exists("plot_cluster_sizes", mode = "function"))
        stop("plot_cluster_sizes() is missing. Load utils_core.R.")

      plot_cluster_sizes(self$cluster_assignments)
    },

    # ----------------------------------------------------------------------
    # SUMMARY TABLE (wrapper utils_core)
    # ----------------------------------------------------------------------
    summary_table = function(X) {
      if (is.null(self$cluster_assignments))
        stop("Model must be fitted first.")

      if (!exists("cluster_summary_table", mode = "function"))
        stop("cluster_summary_table() missing. Load utils_core.R.")

      cluster_summary_table(X, self$cluster_assignments)
    },

    # ----------------------------------------------------------------------
    # SELECT K
    # ----------------------------------------------------------------------
    select_k = function(X, k_min = 2, k_max = 10,
                        method = c("elbow", "silhouette", "Q")) {
      if (k_max > ncol(X))
        stop("k_max cannot exceed number of variables")
      method <- match.arg(method)

      results <- data.frame(
        k          = k_min:k_max,
        W          = NA_real_,
        B          = NA_real_,
        Q          = NA_real_,
        silhouette = NA_real_
      )

      for (k in k_min:k_max) {
        mod <- VariableKMeansClust$new(
          n_clusters = k,
          max_iter   = self$max_iter,
          scale      = self$scale
        )
        mod$fit(X)
        sil_vec <- mod$compute_silhouette(X)
        sil     <- mean(sil_vec, na.rm = TRUE)

        results[results$k == k, ] <- c(k, mod$W_total, mod$B_total,
                                       mod$Q, sil)
      }

      if (method == "elbow") {
        score <- results$W + 0.1 * results$k
        best  <- results$k[which.min(score)]
      } else if (method == "silhouette") {
        score <- results$silhouette - 0.05 * results$k
        best  <- results$k[which.max(score)]
      } else {
        score <- results$Q - 0.05 * results$k
        best  <- results$k[which.max(score)]
      }

      list(best_k = best, table = results)
    },

    # ----------------------------------------------------------------------
    # PLOT PCA GLOBAL
    # ----------------------------------------------------------------------
    plot_global_pca = function() {
      if (is.null(self$cluster_assignments))
        stop("Model must be fitted first.")

      Z <- scale(self$data_raw, center = self$means, scale = self$sds)

      pca <- prcomp(Z, center = FALSE)
      loadings <- pca$rotation[, 1:2]
      scores   <- pca$x[, 1:2]

      df <- data.frame(
        PC1 = loadings[,1],
        PC2 = loadings[,2],
        var = rownames(loadings),
        cluster = factor(self$cluster_assignments)
      )

      expl <- round(100 * summary(pca)$importance[2, 1:2], 1)

      p <- ggplot(df, aes(PC1, PC2, label = var, color = cluster)) +
        geom_point(size = 3) +
        ggrepel::geom_text_repel(size = 3) +
        ggplot2::stat_ellipse(aes(group = cluster, fill = cluster),
                              geom = "polygon", alpha = 0.1, color = NA)+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
        ggplot2::theme_minimal() +
        ggplot2::ggtitle(sprintf(
          "ACP Globale • PC1 = %.1f%% • PC2 = %.1f%% • Total = %.1f%%",
          expl[1], expl[2], expl[1] + expl[2]
        ))

      print(p)
      invisible(p)
    },

    # ----------------------------------------------------------------------
    # PLOT INTERCLUSTER PCA
    # ----------------------------------------------------------------------
    plot_cluster_pca = function(cluster_id) {
      if (is.null(self$cluster_assignments))
        stop("Model must be fitted first.")

      vars <- which(self$cluster_assignments == cluster_id)
      if (length(vars) < 2)
        stop("Cluster must contain ≥2 variables for PCA.")

      Zc <- scale(self$data_raw[, vars, drop = FALSE],
                  center = self$means[vars],
                  scale = self$sds[vars])

      pca <- prcomp(Zc, center = FALSE)

      loadings <- pca$rotation[, 1:2]
      df <- data.frame(
        PC1 = loadings[, 1],
        PC2 = loadings[, 2],
        var = colnames(Zc)
      )

      expl <- round(100 * summary(pca)$importance[2, 1:2], 1)

      ggplot(df, aes(PC1, PC2, label = var)) +
        geom_point(color = "steelblue", size = 4) +
        ggrepel::geom_text_repel() +
        theme_minimal() +
        ggtitle(sprintf("Cluster %d PCA (PC1=%.1f%%, PC2=%.1f%%)",
                        cluster_id, expl[1], expl[2]))
    },

    # ----------------------------------------------------------------------
    # VISUALISATION
    # ----------------------------------------------------------------------
    visualize = function(X = NULL, alt_method = "heatmap") {
      if (is.null(self$cluster_assignments)) stop("Model must be fitted first.")

      # Utiliser original_data si X non fourni
      if (is.null(X)) {
        X <- self$original_data
      }

      if (is.null(X)) {
        stop("No data available. Pass X or fit with a data.frame.")
      }

      # Si X est une matrice, la convertir en data.frame
      if (is.matrix(X)) {
        X <- as.data.frame(X)
      }

      num_vars <- sapply(X, is.numeric)
      Xnum <- as.matrix(X[, num_vars, drop = FALSE])
      Z <- scale(Xnum)

      pca <- prcomp(Z, center = FALSE)
      expl_var  <- summary(pca)$importance[2, 1:2]
      total_var <- sum(expl_var)

      if (alt_method == "heatmap" || total_var < 0.2) {
        corr_mat <- cor(Z)
        ord      <- order(self$cluster_assignments)

        corr_mat <- corr_mat[ord, ord]
        ann <- data.frame(
          cluster = factor(self$cluster_assignments[ord])
        )
        rownames(ann) <- self$var_names[ord]

        pheatmap::pheatmap(
          corr_mat,
          show_rownames = TRUE,
          show_colnames = TRUE,
          main = "Heatmap des corrélations",
          annotation_row = ann,
          annotation_col = ann,
          cluster_rows   = FALSE,
          cluster_cols   = FALSE
        )
      } else {
        loadings <- pca$rotation[, 1:2]
        df_plot <- data.frame(
          PC1    = loadings[, 1],
          PC2    = loadings[, 2],
          var    = rownames(loadings),
          cluster = factor(self$cluster_assignments)
        )

        ggplot2::ggplot(df_plot,
                        ggplot2::aes(PC1, PC2, label = var, color = cluster)) +
          ggplot2::geom_point(size = 3) +
          ggrepel::geom_text_repel() +
          ggplot2::theme_minimal() +
          ggplot2::ggtitle(sprintf("PCA (%.1f%% variance expliquée)",
                                   total_var * 100))
      }
    }
  ),

  private = list(
    seed = NULL
  )

)
