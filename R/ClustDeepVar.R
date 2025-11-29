#' ClustDeepVar: Deep Variable Clustering via Autoencoder Embeddings
#'
#' @description
#' R6 class for clustering variables using deep autoencoder neural networks.
#' Each variable is represented by its latent embedding learned by an autoencoder,
#' then clustered with k-means in the latent space. This allows capturing
#' non-linear relationships between variables.
#'
#' @details
#' **Algorithm (high level):**
#'
#' 1. Standardize the data matrix \code{X} (observations x variables).
#' 2. Transpose to get \eqn{X^T} (variables x observations).
#' 3. Train an autoencoder on \eqn{X^T} to reconstruct the variables
#'    from their latent representation.
#' 4. Extract the latent codes for each variable (rows of \eqn{X^T}).
#' 5. Cluster these latent codes with k-means.
#' 6. Optionally compute soft cluster memberships and project illustrative
#'    variables into the latent space.
#'
#' **Architecture:**
#'
#' \deqn{\text{Input} (n\_obs) -> hidden layers -> latent (d) -> hidden layers -> output (n\_obs)}
#'
#' where each "observation" of the autoencoder is a **variable** from the
#' original dataset.
#'
#' @section Premium features:
#' \itemize{
#'   \item Configurable depth and width of the autoencoder.
#'   \item Optional dropout and L2 regularisation.
#'   \item Choice of loss: \code{"mse"}, \code{"mae"}, or \code{"huber"}.
#'   \item Soft cluster memberships (probabilities) based on distances in
#'         the latent space.
#'   \item Training history stored + \code{$plot_training()}.
#'   \item 2D and 3D visualisation of embeddings.
#'   \item Projection of illustrative variables (numeric or factor) into
#'         the latent space + helper \code{$nearest_to_illustrative()}.
#' }
#'
#' @field data Numeric matrix of standardized data (observations x variables).
#' @field n_clusters Integer. Number of clusters.
#' @field latent_dim Integer. Dimension of latent space.
#' @field clusters Integer vector of cluster memberships (one per variable).
#' @field embeddings Numeric matrix of latent embeddings (variables x latent_dim).
#' @field soft_membership Numeric matrix (variables x n_clusters) of soft cluster
#'   probabilities (rows sum to 1).
#' @field cluster_centers Numeric matrix (n_clusters x latent_dim) of centers
#'   in the latent space.
#' @field autoencoder Keras model of the full autoencoder.
#' @field encoder Keras model of the encoder part only.
#' @field reconstruction_error Global mean squared reconstruction error.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' X <- matrix(rnorm(200 * 15), nrow = 200)
#' colnames(X) <- paste0("Var", 1:15)
#'
#' model <- ClustDeepVar$new(
#'   n_clusters   = 3,
#'   latent_dim   = 5,
#'   hidden_layers = c(64, 32),
#'   epochs       = 50,
#'   dropout      = 0.1,
#'   l2_reg       = 1e-4
#' )
#'
#' model$fit(X)
#' model$summary()
#'
#' # Hard and soft clusters
#' head(model$clusters)
#' head(model$soft_membership)
#'
#' # Project one illustrative numeric variable
#' illu_emb <- model$project_illustrative(X[, 1])
#' illu_emb
#'
#' # 2D visualisation
#' model$plot_embeddings_2d()
#' model$plot_training()
#' }
#'
#' @export
ClustDeepVar <- R6::R6Class(
  "ClustDeepVar",

  public = list(
    # Public fields
    data               = NULL,
    n_clusters         = NULL,
    latent_dim         = NULL,
    clusters           = NULL,
    embeddings         = NULL,
    soft_membership    = NULL,
    cluster_centers    = NULL,
    autoencoder        = NULL,
    encoder            = NULL,
    reconstruction_error = NULL,

    #' @description
    #' Initialize a new ClustDeepVar object.
    #'
    #' @param n_clusters Integer. Number of clusters.
    #' @param latent_dim Integer. Dimension of latent space. If NULL,
    #'   defaults to \code{2 * n_clusters}.
    #' @param hidden_layers Integer vector. Sizes of hidden layers in the
    #'   encoder (decoder is symmetric).
    #' @param activation Character. Activation function for hidden layers.
    #' @param epochs Integer. Max number of epochs for training.
    #' @param batch_size Integer. Batch size used for training.
    #' @param learning_rate Numeric. Learning rate of Adam optimizer.
    #' @param dropout Numeric in [0,1]. Dropout rate applied after each
    #'   hidden layer (0 = no dropout).
    #' @param l2_reg Numeric. L2 regularisation factor (0 = no regularisation).
    #' @param loss Character. One of "mse", "mae", "huber".
    #' @param validation_split Numeric in (0,1). Fraction of variables
    #'   used for validation during training.
    #' @param temperature Numeric > 0. Temperature used when converting
    #'   distances to soft cluster probabilities (higher = softer).
    #' @param seed Optional integer. Random seed used for reproducibility
    #'   (Keras + R).
    initialize = function(n_clusters      = 3L,
                          latent_dim      = NULL,
                          hidden_layers   = c(64L, 32L),
                          activation      = "relu",
                          epochs          = 100L,
                          batch_size      = 32L,
                          learning_rate   = 1e-3,
                          dropout         = 0,
                          l2_reg          = 0,
                          loss            = c("mse", "mae", "huber"),
                          validation_split = 0.2,
                          temperature     = 1,
                          seed            = NULL) {

      if (!requireNamespace("keras3", quietly = TRUE)) {
        stop("Package 'keras3' is required. Install with install.packages('keras3').")
      }

      loss <- match.arg(loss)

      self$n_clusters   <- as.integer(n_clusters)
      self$latent_dim   <- if (is.null(latent_dim)) 2L * self$n_clusters else as.integer(latent_dim)

      private$hidden_layers    <- as.integer(hidden_layers)
      private$activation       <- activation
      private$epochs           <- as.integer(epochs)
      private$batch_size       <- as.integer(batch_size)
      private$learning_rate    <- learning_rate
      private$dropout          <- dropout
      private$l2_reg           <- l2_reg
      private$loss             <- loss
      private$validation_split <- validation_split
      private$temperature      <- temperature
      private$seed             <- seed

      message(
        "ClustDeepVar initialized: ",
        self$n_clusters, " clusters, latent_dim = ", self$latent_dim,
        ", hidden_layers = ", paste(private$hidden_layers, collapse = "-")
      )
    },

    #' @description
    #' Fit the deep variable clustering model on a data matrix.
    #'
    #' @param X Numeric matrix or data.frame (observations x variables).
    #' @param verbose Integer (0, 1 or 2). Verbosity of Keras training.
    #'
    #' @return Invisibly returns \code{self}.
    fit = function(X, verbose = 1) {
      if (is.null(X) || nrow(X) < 10) {
        stop("Input X must have at least 10 observations (rows).")
      }

      X <- as.matrix(X)
      if (!is.numeric(X)) {
        stop("X must be numeric (observations x variables).")
      }
      if (any(is.na(X))) {
        stop("Input contains missing values. Please impute or remove NA before fitting.")
      }

      # Standardize: observations x variables
      X_scaled <- scale(X)
      private$data_mean <- attr(X_scaled, "scaled:center")
      private$data_sd   <- attr(X_scaled, "scaled:scale")
      self$data <- X_scaled

      n_obs  <- nrow(X_scaled)
      n_vars <- ncol(X_scaled)

      message("Building autoencoder for ", n_vars, " variables (", n_obs, " observations)...")

      # Variables as "observations" for the autoencoder
      X_t <- t(X_scaled)
      storage.mode(X_t) <- "double"

      # Optional seeding for reproducibility
      private$set_global_seed()

      # Build models
      models <- private$build_autoencoder(input_dim = n_obs)
      self$autoencoder <- models$autoencoder
      self$encoder     <- models$encoder

      # Train autoencoder
      message("Training autoencoder...")
      history <- self$autoencoder$fit(
        x = X_t,
        y = X_t,
        epochs = private$epochs,
        batch_size = min(private$batch_size, n_vars),
        validation_split = private$validation_split,
        verbose = verbose,
        callbacks = list(
          keras3::callback_early_stopping(
            monitor = "val_loss",
            patience = 10,
            restore_best_weights = TRUE
          )
        )
      )

      # Récupération robuste de l'historique d'entraînement
      h <- tryCatch(
        history$metrics,            # certains backends
        error = function(e) NULL
      )
      if (is.null(h)) {
        h <- tryCatch(
          history$history,          # keras3 / TF récents
          error = function(e) NULL
        )
      }
      if (!is.null(h)) {
        private$history <- as.list(h)
      } else {
        private$history <- NULL
      }

      # Embeddings (variables x latent_dim)
      message("Computing latent embeddings...")
      self$embeddings <- self$encoder$predict(X_t, verbose = 0)
      rownames(self$embeddings) <- colnames(X)

      # K-means in latent space
      message("Clustering embeddings with k-means (k = ", self$n_clusters, ")...")
      km <- stats::kmeans(
        self$embeddings,
        centers  = self$n_clusters,
        nstart   = 25,
        iter.max = 200
      )
      self$clusters        <- km$cluster
      names(self$clusters) <- colnames(X)
      self$cluster_centers <- private$get_cluster_centers()

      # Soft memberships
      self$soft_membership <- private$compute_soft_membership(self$embeddings)

      # Global reconstruction error
      X_recon <- self$autoencoder$predict(X_t, verbose = 0)
      self$reconstruction_error <- mean((X_t - X_recon)^2)

      message("Deep clustering completed.")
      message("  Global reconstruction MSE: ", signif(self$reconstruction_error, 4))

      invisible(self)
    },

    #' @description
    #' Predict hard cluster membership for new variables.
    #'
    #' @param X_new Numeric matrix or data.frame (observations x NEW variables).
    #' @param mode Character. Scaling mode:
    #'   \itemize{
    #'     \item "strict" = reuse mean and sd from training data per variable.
    #'     \item "flex"   = global mean/sd on the fitted data.
    #'   }
    #'
    #' @return Integer vector of cluster assignments for the new variables.
    predict = function(X_new, mode = c("flex", "strict")) {
      mode <- match.arg(mode)

      if (is.null(self$encoder)) {
        stop("Model has not been fitted yet. Call $fit() first.")
      }

      X_new <- as.matrix(X_new)
      emb_new <- private$encode_new(X_new, mode = mode)

      centers <- self$cluster_centers
      d2 <- private$compute_dist2(emb_new, centers)

      apply(d2, 1L, which.min)
    },

    #' @description
    #' Predict embeddings, hard clusters and soft probabilities for new variables.
    #'
    #' @param X_new Numeric matrix or data.frame (observations x NEW variables).
    #' @param mode Scaling mode, see \code{$predict()}.
    #'
    #' @return List with elements \code{embeddings}, \code{clusters},
    #'   and \code{soft_membership}.
    predict_new = function(X_new, mode = c("flex", "strict")) {
      mode <- match.arg(mode)

      if (is.null(self$encoder)) {
        stop("Model has not been fitted yet. Call $fit() first.")
      }

      X_new <- as.matrix(X_new)
      emb_new <- private$encode_new(X_new, mode = mode)
      centers <- self$cluster_centers

      d2 <- private$compute_dist2(emb_new, centers)
      clusters <- apply(d2, 1L, which.min)
      names(clusters) <- colnames(X_new)

      soft <- private$dist2_to_soft(d2)

      list(
        embeddings      = emb_new,
        clusters        = clusters,
        soft_membership = soft
      )
    },

    #' @description
    #' Get cluster assignments (compatibility with VariableClustering)
    #'
    #' @return Named integer vector of cluster assignments
    get_clusters = function() {
      if (is.null(self$clusters)) {
        stop("Model has not been fitted. Call $fit() first.")
      }
      return(self$clusters)
    },

    #' @description
    #' Project illustrative variables into the latent space.
    #'
    #' @param X_illu Either:
    #'   \itemize{
    #'     \item a numeric or factor vector (single illustrative variable), or
    #'     \item a matrix / data.frame with several illustrative variables in columns.
    #'   }
    #' @param labels Optional character vector of labels to use as row names
    #'   in the returned matrix. If \code{NULL}, uses column names (for
    #'   matrices/data.frames) or:
    #'   \itemize{
    #'     \item "Illustrative" for a single numeric vector,
    #'     \item the factor levels for factor variables.
    #'   }
    #'
    #' @return Numeric matrix of size (n_illu_levels x latent_dim), where
    #'   rows correspond to illustrative variables (or their levels) and
    #'   columns to latent dimensions.
    project_illustrative = function(X_illu, labels = NULL) {
      if (is.null(self$embeddings)) {
        stop("Model must be fitted before projecting illustrative variables.")
      }

      X <- self$data
      emb <- self$embeddings
      ld <- ncol(emb)

      # Helper for a single vector (numeric or factor)
      project_one <- function(var, label_override = NULL) {
        var <- as.vector(var)

        if (is.numeric(var)) {
          var_sc <- scale(var)
          cors <- apply(X, 2L, function(x) stats::cor(x, var_sc))
          cors[!is.finite(cors)] <- 0
          ill <- as.numeric(cors %*% emb)
          if (sum(ill^2) > 0) ill <- ill / sqrt(sum(ill^2))

          mat <- matrix(ill, nrow = 1L)
          rownames(mat) <- if (!is.null(label_override)) label_override else "Illustrative"
          colnames(mat) <- colnames(emb)
          return(mat)
        }

        # Factor / character
        var <- as.factor(var)
        lev <- levels(var)
        res <- matrix(0, nrow = length(lev), ncol = ld)
        rownames(res) <- lev
        colnames(res) <- colnames(emb)

        for (i in seq_along(lev)) {
          l <- lev[i]
          dummy <- as.numeric(var == l)
          cors <- apply(X, 2L, function(x) stats::cor(x, dummy))
          cors[!is.finite(cors)] <- 0
          ill <- as.numeric(cors %*% emb)
          if (sum(ill^2) > 0) ill <- ill / sqrt(sum(ill^2))
          res[i, ] <- ill
        }

        if (!is.null(label_override)) rownames(res) <- unname(label_override)

        return(res)
      }

      # Single vector
      if (is.vector(X_illu) || is.factor(X_illu) || is.character(X_illu)) {
        return(project_one(X_illu, labels))
      }

      # Matrix / data.frame: project each column and stack
      Xdf <- as.data.frame(X_illu)
      all_res <- list()
      for (j in seq_along(Xdf)) {
        lab <- if (!is.null(labels) && length(labels) >= j) labels[j] else names(Xdf)[j]
        res_j <- project_one(Xdf[[j]], lab)
        all_res[[length(all_res) + 1L]] <- res_j
      }

      do.call(rbind, all_res)
    },

    #' @description
    #' Find variables closest to a given illustrative embedding (e.g. one level).
    #'
    #' @param illustrative_embeddings Matrix returned by \code{$project_illustrative()}.
    #' @param level Row name in \code{illustrative_embeddings} to use as target.
    #' @param k Integer. Number of closest variables to return.
    #'
    #' @return Data frame with columns \code{Variable} and \code{Distance}.
    nearest_to_illustrative = function(illustrative_embeddings,
                                       level,
                                       k = 5L) {

      if (is.null(self$embeddings)) {
        stop("Model must be fitted before using nearest_to_illustrative().")
      }

      if (!(level %in% rownames(illustrative_embeddings))) {
        stop("Requested level '", level,
             "' is not present in 'illustrative_embeddings' row names.")
      }

      target <- illustrative_embeddings[level, , drop = FALSE]
      emb <- self$embeddings

      dists <- sqrt(rowSums(
        (emb - matrix(
          target,
          nrow = nrow(emb),
          ncol = ncol(emb),
          byrow = TRUE
        ))^2
      ))

      idx <- order(dists)[seq_len(min(k, length(dists)))]

      data.frame(
        Variable = names(self$clusters)[idx],
        Distance = round(dists[idx], 4L),
        row.names = NULL
      )
    },

    #' @description
    #' Print a compact summary of the model.
    print = function() {
      cat("<ClustDeepVar>\n")
      cat("  Clusters      :", self$n_clusters, "\n")
      cat("  Latent dim    :", self$latent_dim, "\n")
      cat("  Hidden layers :", paste(private$hidden_layers, collapse = "-"), "\n")
      if (!is.null(self$clusters)) {
        cat("  Variables     :", length(self$clusters), "\n")
        cat("  MSE           :", signif(self$reconstruction_error, 4), "\n")
        cat("  Status        : Fitted\n")
      } else {
        cat("  Status        : Not fitted\n")
      }
      invisible(self)
    },

    #' @description
    #' Detailed textual summary of clustering results.
    summary = function() {
      if (is.null(self$clusters)) {
        cat("ClustDeepVar model (not fitted)\n")
        cat("Number of clusters :", self$n_clusters, "\n")
        cat("Latent dimension   :", self$latent_dim, "\n")
        return(invisible(self))
      }

      cat("=== ClustDeepVar Summary ===\n\n")
      cat("Method              : Deep Autoencoder Variable Clustering\n")
      cat("Number of clusters  :", self$n_clusters, "\n")
      cat("Latent dimension    :", self$latent_dim, "\n")
      cat("Number of variables :", length(self$clusters), "\n")
      cat("Number of obs.      :", nrow(self$data), "\n")
      cat("Hidden layers       :", paste(private$hidden_layers, collapse = ", "), "\n")
      cat("Activation          :", private$activation, "\n")
      cat("Dropout             :", private$dropout, "\n")
      cat("L2 regularisation   :", private$l2_reg, "\n")
      cat("Loss                :", private$loss, "\n\n")

      cat("Model Performance:\n")
      cat("  Reconstruction MSE:", signif(self$reconstruction_error, 6), "\n\n")

      cat("Cluster Composition:\n")
      var_names <- names(self$clusters)
      for (k in seq_len(self$n_clusters)) {
        vars_k <- var_names[self$clusters == k]
        cat(sprintf(
          "  Cluster %d (%d variables): %s\n",
          k, length(vars_k),
          paste(vars_k, collapse = ", ")
        ))
      }

      cat("\nCluster Size (%):\n")
      cluster_sizes <- table(self$clusters)
      for (k in seq_len(self$n_clusters)) {
        pct <- 100 * cluster_sizes[k] / length(self$clusters)
        cat(sprintf("  Cluster %d: %d variables (%.1f%%)\n",
                    k, cluster_sizes[k], pct))
      }

      invisible(self)
    },

    #' @description
    #' Plot variable embeddings in 2D latent space.
    #'
    #' @param dims Integer vector of length 2 indicating latent dimensions.
    #' @param new_embeddings Optional matrix of new embeddings to overlay.
    #' @param new_clusters Optional integer vector of cluster assignments
    #'   for new embeddings.
    #' @param new_labels Optional labels for new embeddings.
    #' @param illustrative_embeddings Optional matrix of illustrative
    #'   embeddings to overlay.
    #' @param illustrative_labels Optional labels for illustrative embeddings.
    #' @param alpha_new Numeric alpha transparency for new variables.
    plot_embeddings_2d = function(dims = c(1L, 2L),
                                  use_pca = FALSE,
                                  new_embeddings = NULL,
                                  new_clusters = NULL,
                                  new_labels = NULL,
                                  illustrative_embeddings = NULL,
                                  illustrative_labels = NULL,
                                  alpha_new = 0.9) {

      if (is.null(self$embeddings)) {
        stop("Model has not been fitted. Call $fit() first.")
      }

      # ============================================================
      # NOUVEAU: Option PCA
      # ============================================================
      if(use_pca && ncol(self$embeddings) > 2){

        # PCA sur les embeddings
        pca <- prcomp(self$embeddings, center = TRUE, scale. = FALSE)
        emb <- pca$x[, 1:2]

        # Variance expliquée
        variance_explained <- summary(pca)$importance[2, 1:2] * 100
        main_title <- sprintf(
          "Variable Embeddings (2D) - PCA\nPC1: %.1f%%, PC2: %.1f%%",
          variance_explained[1], variance_explained[2]
        )
        xlab <- "PC1"
        ylab <- "PC2"

      } else {

        # Sélection de dimensions (comportement original)
        if (self$latent_dim < 2L) {
          stop("Latent dimension must be >= 2 for 2D plotting.")
        }
        dims <- as.integer(dims)
        emb <- self$embeddings[, dims, drop = FALSE]
        main_title <- "Variable Embeddings (2D)"
        xlab <- paste("Latent", dims[1L])
        ylab <- paste("Latent", dims[2L])
      }

      # Garde les noms de lignes
      rownames(emb) <- rownames(self$embeddings)

      # ============================================================
      # Plot (reste inchangé)
      # ============================================================
      cols <- grDevices::rainbow(self$n_clusters)[self$clusters]

      pad <- 0.2
      x_range <- range(emb[, 1L])
      y_range <- range(emb[, 2L])
      xlim <- c(x_range[1L] - diff(x_range) * pad,
                x_range[2L] + diff(x_range) * pad)
      ylim <- c(y_range[1L] - diff(y_range) * pad,
                y_range[2L] + diff(y_range) * pad)

      graphics::plot(
        emb,
        col  = cols,
        pch  = 19,
        cex  = 1.5,
        main = main_title,
        xlab = xlab,
        ylab = ylab,
        xlim = xlim,
        ylim = ylim
      )

      graphics::text(
        emb,
        labels = rownames(emb),
        pos    = 3L,
        cex    = 0.7
      )

      # Cluster centroids
      for (k in seq_len(self$n_clusters)) {
        cluster_emb <- emb[self$clusters == k, , drop = FALSE]
        center <- colMeans(cluster_emb)
        graphics::points(center[1L], center[2L],
                         pch = 4L, cex = 2, lwd = 3, col = "black")
      }

      # ============================================================
      # Projeter new_embeddings et illustrative_embeddings
      # ============================================================

      # New variables
      if (!is.null(new_embeddings)) {
        new_embeddings <- as.matrix(new_embeddings)

        if(use_pca && ncol(self$embeddings) > 2){
          # Projeter dans l'espace PCA
          emb_new <- predict(pca, newdata = new_embeddings)[, 1:2]
        } else {
          emb_new <- new_embeddings[, dims, drop = FALSE]
        }

        if (is.null(new_clusters)) {
          stop("If 'new_embeddings' is provided, 'new_clusters' must also be provided.")
        }

        new_cols <- grDevices::adjustcolor(
          grDevices::rainbow(self$n_clusters)[new_clusters],
          alpha.f = alpha_new
        )

        graphics::points(
          emb_new[, 1L], emb_new[, 2L],
          col = new_cols,
          pch = 17L,
          cex = 2
        )

        if (!is.null(new_labels)) {
          graphics::text(
            emb_new[, 1L], emb_new[, 2L],
            labels = new_labels,
            pos    = 4L,
            offset = 0.6,
            cex    = 0.8
          )
        }
      }

      # Illustrative variables
      if (!is.null(illustrative_embeddings)) {

        if(use_pca && ncol(self$embeddings) > 2){
          # Projeter dans l'espace PCA
          ill <- predict(pca, newdata = as.matrix(illustrative_embeddings))[, 1:2]
        } else {
          ill <- as.matrix(illustrative_embeddings)[, dims, drop = FALSE]
        }

        graphics::points(
          ill[, 1L], ill[, 2L],
          pch = 8L, cex = 2.2, lwd = 2,
          col = "black"
        )

        if (!is.null(illustrative_labels)) {
          graphics::text(
            ill[, 1L], ill[, 2L],
            labels = illustrative_labels,
            pos    = 4L,
            cex    = 0.9
          )
        }
      }

      graphics::legend(
        "topright",
        legend = c(
          paste("Cluster", seq_len(self$n_clusters)),
          "Illustrative"
        ),
        col = c(grDevices::rainbow(self$n_clusters), "black"),
        pch = c(rep(19L, self$n_clusters), 8L),
        cex = 0.8
      )
    },

    #' @description
    #' Plot variable embeddings in 3D latent space (requires rgl).
    #' @description
    #' Plot variable embeddings in 3D latent space (requires rgl).
    #' Automatically applies PCA if latent_dim > 3.
    plot_embeddings_3d = function(dims = c(1L, 2L, 3L),
                                  use_pca = TRUE,
                                  new_embeddings = NULL,
                                  new_clusters = NULL,
                                  new_labels = NULL,
                                  illustrative_embeddings = NULL,
                                  illustrative_labels = NULL,
                                  alpha_new = 0.9) {

      if (!requireNamespace("rgl", quietly = TRUE)) {
        stop("Package 'rgl' is required for 3D plots.")
      }
      if (is.null(self$embeddings)) {
        stop("Model has not been fitted.")
      }

      # ============================================================
      # NOUVEAU: Réduction PCA si latent_dim > 3
      # ============================================================
      if (use_pca && self$latent_dim > 3L) {

        message("Using PCA to reduce latent_dim=", self$latent_dim, " to 3D")

        # PCA sur embeddings
        pca <- prcomp(self$embeddings, center = TRUE, scale. = FALSE)
        emb <- pca$x[, 1:3]
        rownames(emb) <- rownames(self$embeddings)

        # Variance expliquée
        variance_explained <- summary(pca)$importance[2, 1:3] * 100

        main_title <- sprintf(
          "Variable Embeddings (3D via PCA)\nPC1: %.1f%%, PC2: %.1f%%, PC3: %.1f%%",
          variance_explained[1], variance_explained[2], variance_explained[3]
        )

        xlab <- "PC1"
        ylab <- "PC2"
        zlab <- "PC3"

        # Projeter new/illustrative embeddings dans l'espace PCA
        if (!is.null(new_embeddings)) {
          new_embeddings <- predict(pca, newdata = as.matrix(new_embeddings))[, 1:3]
        }
        if (!is.null(illustrative_embeddings)) {
          illustrative_embeddings <- predict(pca, newdata = as.matrix(illustrative_embeddings))[, 1:3]
        }

      } else {

        # Sélection directe des 3 premières dimensions (comportement original)
        if (self$latent_dim < 3L) {
          stop("Latent dimension must be >= 3 for 3D plotting.")
        }

        dims <- as.integer(dims)
        emb <- self$embeddings[, dims, drop = FALSE]

        main_title <- "Variable Embeddings (3D)"
        xlab <- paste("Latent", dims[1L])
        ylab <- paste("Latent", dims[2L])
        zlab <- paste("Latent", dims[3L])

        # Sélection des dims pour new/illustrative
        if (!is.null(new_embeddings)) {
          new_embeddings <- as.matrix(new_embeddings)[, dims, drop = FALSE]
        }
        if (!is.null(illustrative_embeddings)) {
          illustrative_embeddings <- as.matrix(illustrative_embeddings)[, dims, drop = FALSE]
        }
      }

      # ============================================================
      # Plot (reste identique)
      # ============================================================
      cols <- grDevices::rainbow(self$n_clusters)[self$clusters]

      rgl::open3d()
      rgl::plot3d(
        emb,
        col  = cols,
        size = 10,
        type = "s",
        xlab = xlab,
        ylab = ylab,
        zlab = zlab,
        main = main_title
      )

      rgl::text3d(emb, texts = rownames(emb), cex = 0.8)

      # Centroids
      for (k in seq_len(self$n_clusters)) {
        center <- colMeans(emb[self$clusters == k, , drop = FALSE])
        rgl::points3d(center[1L], center[2L], center[3L],
                      col = "black", size = 15)
      }

      # New variables
      if (!is.null(new_embeddings)) {
        new_cols <- grDevices::adjustcolor(
          grDevices::rainbow(self$n_clusters)[new_clusters],
          alpha.f = alpha_new
        )
        rgl::points3d(new_embeddings, col = new_cols, size = 12)
        if (!is.null(new_labels)) {
          rgl::text3d(new_embeddings, texts = new_labels, cex = 1)
        }
      }

      # Illustrative
      if (!is.null(illustrative_embeddings)) {
        rgl::points3d(illustrative_embeddings, col = "black", size = 15)
        if (!is.null(illustrative_labels)) {
          rgl::text3d(illustrative_embeddings, texts = illustrative_labels, cex = 1.1)
        }
      }

      message("3D plot generated with ",
              ifelse(use_pca && self$latent_dim > 3, "PCA reduction", "direct dimension selection"))
    },

    #' @description
    #' Backwards-compatible alias for \code{$plot_embeddings_3d()}.
    plot_embeddings_3d_ext = function(...) {
      self$plot_embeddings_3d(...)
    },

    #' @description
    #' Plot reconstruction error per variable.
    plot_reconstruction = function() {
      if (is.null(self$autoencoder)) {
        stop("Model has not been fitted. Call $fit() first.")
      }

      X_t <- t(self$data)
      storage.mode(X_t) <- "double"
      X_recon <- self$autoencoder$predict(X_t, verbose = 0)

      var_err <- rowMeans((X_t - X_recon)^2)
      names(var_err) <- colnames(self$data)
      var_err <- sort(var_err, decreasing = TRUE)

      clust <- self$clusters[names(var_err)]
      cols <- grDevices::rainbow(self$n_clusters)[clust]

      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par))

      graphics::par(mar = c(5, 10, 4, 8), xpd = TRUE)
      graphics::barplot(
        var_err,
        horiz = TRUE,
        las   = 1,
        col   = cols,
        main  = "Reconstruction Error by Variable",
        xlab  = "MSE"
      )
      graphics::legend(
        "topright",
        legend = paste("Cluster", seq_len(self$n_clusters)),
        fill   = grDevices::rainbow(self$n_clusters),
        border = NA,
        cex    = 1
      )
    },

    #' @description
    #' Plot training curves (loss and optionally validation loss).
    #'
    #' @param metric Character, one of "loss", "val_loss", "mae", "val_mae"
    #'   if available in the training history.
    plot_training = function(metric = c("loss", "val_loss", "mae", "val_mae")) {
      metric <- match.arg(metric)
      h <- private$history
      if (is.null(h)) {
        stop("No training history stored. Did you call $fit() successfully?")
      }
      if (!metric %in% names(h)) {
        stop("Metric '", metric, "' not found in training history. Available: ",
             paste(names(h), collapse = ", "))
      }

      y <- as.numeric(h[[metric]])
      x <- seq_along(y)

      graphics::plot(
        x, y,
        type = "l",
        xlab = "Epoch",
        ylab = metric,
        main = paste("Training curve -", metric)
      )
      graphics::grid()
    }
  ),

  private = list(
    hidden_layers    = c(64L, 32L),
    activation       = "relu",
    epochs           = 100L,
    batch_size       = 32L,
    learning_rate    = 1e-3,
    dropout          = 0,
    l2_reg           = 0,
    loss             = "mse",
    validation_split = 0.2,
    temperature      = 1,
    seed             = NULL,

    data_mean = NULL,
    data_sd   = NULL,
    history   = NULL,

    set_global_seed = function() {
      if (!is.null(private$seed)) {
        set.seed(private$seed)
        if (requireNamespace("tensorflow", quietly = TRUE)) {
          tensorflow::tf$random$set_seed(as.integer(private$seed))
        }
      }
    },

    get_cluster_centers = function() {
      # Compute centers in latent space from current clusters & embeddings
      centers <- sapply(seq_len(self$n_clusters), function(k) {
        idx <- which(self$clusters == k)
        if (length(idx) == 0) {
          rep(0, self$latent_dim)
        } else {
          colMeans(self$embeddings[idx, , drop = FALSE])
        }
      })
      t(centers)
    },

    compute_dist2 = function(emb, centers) {
      # Squared Euclidean distances: rows = observations, cols = centers
      n <- nrow(emb)
      K <- nrow(centers)

      d2 <- matrix(0, nrow = n, ncol = K)
      for (k in seq_len(K)) {
        diff <- emb - matrix(
          centers[k, ],
          nrow = n,
          ncol = ncol(emb),
          byrow = TRUE
        )
        d2[, k] <- rowSums(diff^2)
      }
      colnames(d2) <- paste0("Cluster", seq_len(K))
      d2
    },

    dist2_to_soft = function(d2) {
      # Convert squared distances to soft cluster probabilities
      # via temperature-scaled softmax of negative distances.
      temp <- private$temperature
      logits <- -d2 / temp
      logits <- logits - apply(logits, 1L, max)  # numerical stability
      expL <- exp(logits)
      probs <- expL / rowSums(expL)
      probs
    },

    compute_soft_membership = function(emb) {
      centers <- self$cluster_centers
      d2 <- private$compute_dist2(emb, centers)
      private$dist2_to_soft(d2)
    },

    encode_new = function(X_new, mode = c("flex", "strict")) {
      mode <- match.arg(mode)

      if (is.null(self$data)) {
        stop("Training data not stored; cannot scale new data.")
      }

      X_new <- as.matrix(X_new)

      if (mode == "strict") {
        # Use per-variable mean/sd
        if (ncol(X_new) != length(private$data_mean)) {
          warning("Number of variables in X_new differs from training data. ",
                  "Strict scaling may be inconsistent.")
        }
        X_new_sc <- scale(
          X_new,
          center = private$data_mean,
          scale  = private$data_sd
        )
      } else {
        # Global mean/sd over all entries of training data
        mu <- mean(as.numeric(self$data))
        sigma <- stats::sd(as.numeric(self$data))
        X_new_sc <- scale(
          X_new,
          center = rep(mu, ncol(X_new)),
          scale  = rep(sigma, ncol(X_new))
        )
      }

      X_new_t <- t(X_new_sc)
      storage.mode(X_new_t) <- "double"
      self$encoder$predict(X_new_t, verbose = 0)
    },

    build_autoencoder = function(input_dim) {
      # Build symmetric autoencoder with optional dropout & L2 regularisation
      reg <- if (private$l2_reg > 0) {
        keras3::regularizer_l2(private$l2_reg)
      } else {
        NULL
      }

      input_layer <- keras3::layer_input(shape = list(input_dim), name = "input")

      # Encoder
      encoded <- input_layer
      for (units in private$hidden_layers) {
        encoded <- keras3::layer_dense(
          encoded,
          units      = units,
          activation = private$activation,
          kernel_regularizer = reg
        )
        if (private$dropout > 0) {
          encoded <- keras3::layer_dropout(encoded, rate = private$dropout)
        }
      }

      latent <- keras3::layer_dense(
        encoded,
        units      = self$latent_dim,
        activation = "linear",
        name       = "latent"
      )

      # Decoder (mirror)
      decoded <- latent
      for (units in rev(private$hidden_layers)) {
        decoded <- keras3::layer_dense(
          decoded,
          units      = units,
          activation = private$activation,
          kernel_regularizer = reg
        )
        if (private$dropout > 0) {
          decoded <- keras3::layer_dropout(decoded, rate = private$dropout)
        }
      }

      output_layer <- keras3::layer_dense(
        decoded,
        units      = input_dim,
        activation = "linear",
        name       = "output"
      )

      autoencoder <- keras3::keras_model(input_layer, output_layer)
      encoder     <- keras3::keras_model(input_layer, latent)

      opt <- keras3::optimizer_adam(learning_rate = private$learning_rate)

      autoencoder$compile(
        optimizer = opt,
        loss      = private$loss,
        metrics   = list("mae")
      )

      list(
        autoencoder = autoencoder,
        encoder     = encoder
      )
    }
  )
)
