#' VariableClustering: Unified Interface for Variable Clustering
#'
#' @description
#' Unified R6 wrapper class that provides a scikit-learn-style interface
#' for all variable clustering methods. Supports three algorithms:
#' - "deep": ClustDeepVar for autoencoder-based clustering
#' - "kmeans": ClustKMeansVar for K-means réallocatif (Qannari)
#' - "quali_varclus": ClustQualiVarclus for qualitative VARCLUS
#'
#' @details
#' This class provides a consistent API across all clustering methods,
#' making it easy to switch between algorithms and compare results.
#'
#' @field method Character. Clustering method ("deep", "kmeans", "quali_varclus")
#' @field model The underlying clustering model object
#' @field n_clusters Number of clusters
#'
#' @examples
#' \dontrun{
#' library(VarClustR)
#'
#' # Deep clustering
#' model1 <- VariableClustering$new(
#'   method = "deep",
#'   n_clusters = 3,
#'   latent_dim = 5,
#'   epochs = 50
#' )
#' model1$fit(mtcars)
#' model1$summary()
#' model1$plot()
#'
#' # K-means clustering
#' model2 <- VariableClustering$new(
#'   method = "kmeans",
#'   n_clusters = 3,
#'   seed = 42
#' )
#' model2$fit(mtcars)
#' model2$plot(type = "global_pca")
#'
#' # Qualitative VARCLUS
#' iris_quali <- data.frame(
#'   Species = iris$Species,
#'   Size = cut(iris$Sepal.Length, 3)
#' )
#' model3 <- VariableClustering$new(
#'   method = "quali_varclus",
#'   n_clusters = 2,
#'   seed = 42
#' )
#' model3$fit(iris_quali)
#' model3$plot(type = "eta2_heatmap")
#' }
#'
#' @export
VariableClustering <- R6::R6Class(
  "VariableClustering",

  ## Public members
  public = list(
    method = NULL,
    model = NULL,
    n_clusters = NULL,

    #' @description
    #' Initialize a new VariableClustering object
    #'
    #' @param method Character. Clustering method: "deep", "kmeans", or "quali_varclus"
    #' @param n_clusters Integer. Number of clusters (NULL for auto-selection in quali_varclus)
    #' @param ... Additional arguments passed to specific clustering method
    #'
    #' @return A new VariableClustering object
    initialize = function(method = "deep", n_clusters = 3, ...) {
      # Validate method
      valid_methods <- c("deep", "kmeans", "quali_varclus")

      if (!method %in% valid_methods) {
        stop("Invalid method. Choose from: ", paste(valid_methods, collapse = ", "))
      }

      self$method <- method
      self$n_clusters <- if (!is.null(n_clusters)) as.integer(n_clusters) else NULL

      # Initialize appropriate model
      self$model <- switch(
        method,
        "deep"          = ClustDeepVar$new(n_clusters = n_clusters, ...),
        "kmeans"        = ClustKMeansVar$new(n_clusters = n_clusters, ...),
        "quali_varclus" = ClustQualiVarclus$new(n_clusters = n_clusters, ...),
        stop("Unknown method: ", method)
      )

      message("VariableClustering initialized with method: ", method)
    },

    #' @description
    #' Fit the clustering model to data
    #'
    #' @param X Data matrix or data frame
    #' @param ... Additional arguments passed to the fit method
    #'
    #' @return Self (invisibly) for method chaining
    fit = function(X, ...) {
      if (is.null(X)) {
        stop("Input data X cannot be NULL")
      }

      if (self$method == "quali_varclus") {
        # Qualitative: ensure factors
        if (!is.data.frame(X)) X <- as.data.frame(X)

        non_factors <- which(!sapply(X, is.factor))
        if (length(non_factors) > 0) {
          message("Converting non-factor variables to factors")
          X[non_factors] <- lapply(X[non_factors], as.factor)
        }

      } else if (self$method == "kmeans") {
        # K-means: GARDE le data.frame (ne pas convertir en matrix)
        if (!is.data.frame(X)) {
          X <- as.data.frame(X)
        }
        # Vérifier que c'est numérique
        non_numeric <- which(!sapply(X, is.numeric))
        if (length(non_numeric) > 0) {
          stop("For K-means, all variables must be numeric. Non-numeric found: ",
               paste(names(X)[non_numeric], collapse = ", "))
        }

      } else if (self$method == "deep") {
        # Deep: convertir en matrix
        if (!is.numeric(X) && !is.matrix(X) && !is.data.frame(X)) {
          stop("For method 'deep', X must be numeric")
        }
        X <- as.matrix(X)
        if (!is.numeric(X)) {
          stop("X must contain numeric values for method 'deep'")
        }
      }

      self$model$fit(X, ...)
      invisible(self)
    },

    #' @description
    #' Predict cluster membership for new data
    #'
    #' @param X_new New data matrix or data frame
    #'
    #' @return Vector of predicted cluster assignments
    predict = function(X_new) {
      tryCatch({
        clusters <- self$get_clusters()
      }, error = function(e) {
        stop("Model has not been fitted. Call $fit() first.")
      })

      return(self$model$predict(X_new))
    },

    #' @description
    #' Fit and predict in one step
    #'
    #' @param X Data matrix or data frame
    #'
    #' @return Vector of cluster assignments
    fit_predict = function(X) {
      self$fit(X)
      return(self$get_clusters())
    },

    #' @description
    #' Get cluster assignments
    #'
    #' @return Named vector of cluster assignments
    get_clusters = function() {
      return(self$model$get_clusters())
    },

    #' @description
    #' Print summary of clustering results
    #'
    #' @return Self (invisibly)
    summary = function() {
      cat("=== VariableClustering Summary ===\n\n")
      cat("Method:", self$method, "\n")
      cat("Number of clusters:", self$n_clusters, "\n\n")

      # Delegate to underlying model
      self$model$summary()

      invisible(self)
    },

    #' @description
    #' Plot clustering results
    #'
    #' @param type Character. Type of plot (depends on method)
    #' @param ... Additional arguments passed to plotting function
    #'
    #' @return NULL (creates plot)
    plot = function(type = "default", ...) {
      tryCatch({
        self$get_clusters()
      }, error = function(e) {
        stop("Model has not been fitted. Call $fit() first.")
      })

      # Method-specific default plots
      if (type == "default") {
        if (self$method == "deep") {
          self$model$plot_embeddings_2d(...)
        } else if (self$method == "kmeans") {
          self$model$plot_global_pca(...)
        } else if (self$method == "quali_varclus") {
          self$model$plot_eta2_heatmap(...)
        }

        # Deep clustering plots
      } else if (type == "embeddings" && self$method == "deep") {
        self$model$plot_embeddings_2d(...)
      } else if (type == "embeddings2d" && self$method == "deep") {
        self$model$plot_embeddings_2d(...)
      } else if (type == "embeddings3d" && self$method == "deep") {
        self$model$plot_embeddings_3d(...)
      } else if (type == "reconstruction" && self$method == "deep") {
        self$model$plot_reconstruction(...)

        # K-means clustering plots
      } else if (type == "global_pca" && self$method == "kmeans") {
        self$model$plot_global_pca(...)
      } else if (type == "silhouette" && self$method == "kmeans") {
        if (is.null(self$model$silhouette)) {
          message("Computing silhouette coefficients...")
          self$model$compute_silhouette()
        }
        self$model$plot_silhouette(...)
      } else if (type == "cluster_pca" && self$method == "kmeans") {
        cluster_id <- if (!is.null(list(...)$cluster_id)) list(...)$cluster_id else 1
        self$model$plot_cluster_pca(cluster_id)
      } else if (type == "dendrogram" && self$method == "kmeans") {
        self$model$plot_dendrogram(...)
      } else if (type == "heatmap" && self$method == "kmeans") {
        self$model$visualize(alt_method = "heatmap")

        # Qualitative VARCLUS plots
      } else if (type == "eta2_heatmap" && self$method == "quali_varclus") {
        self$model$plot_eta2_heatmap(...)
      } else if (type == "cluster_sizes" && self$method == "quali_varclus") {
        self$model$plot_cluster_sizes(...)
      } else if (type == "inertia" && self$method == "quali_varclus") {
        self$model$plot_inertia(...)
      } else if (type == "k_selection" && self$method == "quali_varclus") {
        self$model$plot_k_selection(...)

      } else {
        stop("Plot type '", type, "' not available for method '", self$method, "'")
      }
    },

    #' @description
    #' Get model parameters
    #'
    #' @return List of model parameters
    get_params = function() {
      params <- list(
        method = self$method,
        n_clusters = self$n_clusters
      )

      # Add method-specific parameters
      if (self$method == "deep") {
        params$latent_dim <- self$model$latent_dim
      } else if (self$method == "kmeans") {
        params$max_iter <- self$model$max_iter
        params$scale <- self$model$scale
      } else if (self$method == "quali_varclus") {
        params$max_iter <- self$model$max_iter
        params$seed <- self$model$seed
      }

      return(params)
    },

    #' @description
    #' Export cluster assignments to data frame
    #'
    #' @return Data frame with variable names and cluster assignments
    to_dataframe = function() {
      tryCatch({
        clusters <- self$get_clusters()
      }, error = function(e) {
        stop("Model has not been fitted. Call $fit() first.")
      })

      df <- data.frame(
        Variable = names(clusters),
        Cluster = clusters,
        row.names = NULL
      )

      # Add method-specific information
      if (self$method == "deep" && !is.null(self$model$embeddings)) {
        # Add embedding coordinates
        for (i in 1:min(3, ncol(self$model$embeddings))) {
          df[[paste0("Embed_", i)]] <- self$model$embeddings[, i]
        }
      } else if (self$method == "kmeans") {
        # Add Q ratio and silhouette if available
        if (!is.null(self$model$Q)) {
          df$Q_ratio <- self$model$Q
        }
        if (!is.null(self$model$silhouette)) {
          df$Silhouette <- self$model$silhouette[df$Variable]
        }
      } else if (self$method == "quali_varclus" && !is.null(self$model$eta2_matrix)) {
        # Add max eta2 value
        max_eta2 <- apply(self$model$eta2_matrix, 1, max)
        df$Max_Eta2 <- max_eta2[df$Variable]
      }

      return(df)
    },

    #' @description
    #' Compare with another clustering result
    #'
    #' @param other Another VariableClustering object
    #'
    #' @return List with comparison metrics
    compare = function(other) {
      if (!inherits(other, "VariableClustering")) {
        stop("Argument 'other' must be a VariableClustering object")
      }

      clusters1 <- self$get_clusters()
      clusters2 <- other$get_clusters()

      # Ensure same variables
      common_vars <- intersect(names(clusters1), names(clusters2))
      if (length(common_vars) == 0) {
        stop("No common variables between the two clusterings")
      }

      c1 <- clusters1[common_vars]
      c2 <- clusters2[common_vars]

      # Compute Adjusted Rand Index
      ari <- compute_ari(c1, c2)

      # Compute agreement
      agreement <- mean(c1 == c2)

      return(list(
        method1 = self$method,
        method2 = other$method,
        n_common_variables = length(common_vars),
        adjusted_rand_index = ari,
        agreement = agreement
      ))
    },

    #' @description
    #' Print method
    print = function() {
      cat("<VariableClustering>\n")
      cat("  Method:", self$method, "\n")
      cat("  Clusters:", self$n_clusters, "\n")
      if (!is.null(self$model$clusters)) {
        cat("  Variables:", length(self$model$clusters), "\n")
        cat("  Status: Fitted\n")
      } else {
        cat("  Status: Not fitted\n")
      }
      invisible(self)
    }
  )
)
