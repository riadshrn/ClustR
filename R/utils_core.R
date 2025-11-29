# =====================================================================
# utils_core.R – PREMIUM
# =====================================================================

#' Compute cluster centers in latent space (DeepVar)
#'
#' @param embeddings Matrix (variables × latent_dim)
#' @param clusters Vector of cluster indices
#' @return Matrix of cluster centers
#'
#' @keywords internal
compute_centers <- function(embeddings, clusters) {
  k <- max(clusters)
  centers <- matrix(0, k, ncol(embeddings))
  for (i in 1:k) {
    idx <- which(clusters == i)
    centers[i, ] <- colMeans(embeddings[idx, , drop = FALSE])
  }
  centers
}

# =====================================================================

#' Inertia (within / between / total) for variable embeddings
#'
#' @param embeddings Matrix (variables × latent_dim)
#' @param clusters Vector of cluster assignments
#'
#' @return List with within, between, total
#' @export
compute_inertia <- function(embeddings, clusters) {
  centers <- compute_centers(embeddings, clusters)
  k <- max(clusters)

  # Within
  within <- 0
  for (i in 1:k) {
    idx <- which(clusters == i)
    if (length(idx) > 0) {
      E <- embeddings[idx, , drop = FALSE]
      C <- matrix(centers[i, ], nrow = nrow(E), ncol = ncol(E), byrow = TRUE)
      within <- within + sum((E - C)^2)
    }
  }

  # Total
  global_center <- colMeans(embeddings)
  total <- sum((embeddings - matrix(global_center,
                                    nrow = nrow(embeddings),
                                    ncol = ncol(embeddings),
                                    byrow = TRUE))^2)

  # Between
  between <- total - within

  list(
    within = within,
    between = between,
    total = total,
    ratio_within = within / total,
    ratio_between = between / total
  )
}

# =====================================================================

#' Silhouette score for variables (correlation distance)
#'
#' @param X Original data (observations × variables)
#' @param clusters Cluster assignments over variables
#'
#' @return Mean silhouette
#' @export
compute_silhouette_var <- function(X, clusters) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package 'cluster' required for silhouette.")
  }

  cor_mat <- cor(X)
  dist_mat <- as.dist(1 - abs(cor_mat))

  sil <- cluster::silhouette(clusters, dist_mat)
  mean(sil[, 3])
}

# =====================================================================

#' Variable importance (DeepVar)
#'
#' Importance = 1 / (1 + distance_to_center)
#' Normalized in [0,1]
#'
#' @param embeddings Matrix (variables × latent_dim)
#' @param clusters Cluster vector
#'
#' @return Data frame Variable, Cluster, Importance
#' @export
importance_deep <- function(embeddings, clusters) {
  centers <- compute_centers(embeddings, clusters)

  dists <- sapply(1:nrow(embeddings), function(i) {
    k <- clusters[i]
    sqrt(sum((embeddings[i, ] - centers[k, ])^2))
  })

  imp <- 1 / (1 + dists)
  imp <- imp / max(imp)

  data.frame(
    Variable = rownames(embeddings),
    Cluster  = clusters,
    Importance = imp,
    stringsAsFactors = FALSE
  )
}

# =====================================================================

#' Bootstrap stability (ARI) for Deep clustering
#'
#' @param X Original data (obs × vars)
#' @param model ClustDeepVar fitted model
#' @param B Number of bootstrap samples
#'
#' @return Mean ARI across bootstrap samples
#' @export
stability_bootstrap <- function(X, model, B = 30) {
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' required for ARI.")
  }

  true <- model$clusters
  n <- nrow(X)
  ari_vec <- numeric(B)

  for (b in 1:B) {
    idx <- sample(1:n, n, replace = TRUE)
    Xb <- X[idx, ]

    m2 <- ClustDeepVar$new(
      n_clusters   = model$n_clusters,
      latent_dim   = model$latent_dim,
      hidden_layers = model$hidden_layers,
      epochs       = model$epochs
    )

    suppressMessages(m2$fit(Xb))
    ari_vec[b] <- mclust::adjustedRandIndex(true, m2$clusters)
  }

  mean(ari_vec, na.rm = TRUE)
}

# =====================================================================

#' Summary table of clusters
#'
#' @param X Data matrix (obs × vars)
#' @param model ClustDeepVar
#'
#' @return Data frame summary
#' @export
cluster_summary <- function(X, model) {
  imp <- importance_deep(model$embeddings, model$clusters)
  inert <- compute_inertia(model$embeddings, model$clusters)

  k <- model$n_clusters
  vars <- names(model$clusters)

  res <- lapply(1:k, function(i) {
    v <- vars[model$clusters == i]
    subX <- X[, v, drop = FALSE]

    mean_cor <- if (ncol(subX) > 1) {
      mean(cor(subX)[upper.tri(cor(subX))])
    } else NA

    data.frame(
      Cluster = i,
      n_vars = length(v),
      Mean_Correlation = round(mean_cor, 3),
      Top_Variables = paste(head(imp$Variable[imp$Cluster == i][order(
        -imp$Importance[imp$Cluster == i]
      )], 5), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, res)
}

# =====================================================================

#' Export clustering metrics
#'
#' @param X Data matrix
#' @param model Deep clustering model
#' @param file Base path (without extension)
#' @export
export_metrics <- function(X, model, file = "deep_metrics") {
  imp <- importance_deep(model$embeddings, model$clusters)
  sil <- compute_silhouette_var(X, model$clusters)
  inert <- compute_inertia(model$embeddings, model$clusters)
  summary <- cluster_summary(X, model)

  saveRDS(list(
    importance = imp,
    silhouette = sil,
    inertia = inert,
    summary = summary
  ), paste0(file, ".rds"))

  write.csv(imp, paste0(file, "_importance.csv"), row.names = FALSE)
  write.csv(summary, paste0(file, "_summary.csv"), row.names = FALSE)

  message("Metrics exported ✔")
}


# ============================================================
# PLOT CLUSTER SIZES (Deep / Sparse / Quali)
# ============================================================

#' Plot number of variables per cluster
#' @param clusters Named vector of cluster assignments
#' @export
plot_cluster_sizes <- function(clusters){
  tab <- sort(table(clusters))
  barplot(tab, main="Taille des clusters", col="skyblue",
          ylab="Nb variables", xlab="Cluster")
}

cluster_summary_table <- function(X, clusters){
  X <- as.data.frame(X)
  stopifnot(ncol(X) == length(clusters))

  res <- lapply(sort(unique(clusters)), function(k){
    vars_k <- names(clusters)[clusters==k]
    sub <- X[, vars_k, drop=FALSE]

    mean_cor <- NA
    if (ncol(sub) >= 2) {
      cm <- cor(sub)
      mean_cor <- mean(cm[upper.tri(cm)], na.rm=TRUE)
    }

    top_vars <- head(vars_k, 5)

    data.frame(
      Cluster = k,
      n_vars = length(vars_k),
      Mean_Correlation = round(mean_cor,3),
      Top_Variables = paste(top_vars, collapse=", ")
    )
  })
  do.call(rbind, res)
}

# ============================================================
# CLUSTER SUMMARY TABLE
# ============================================================

#' Summary table of cluster composition (Deep or numeric)
#' @param X Data frame of active variables
#' @param clusters Named vector of clusters
#' @export
cluster_summary_table <- function(X, clusters) {
  stopifnot(!is.null(X), !is.null(clusters))

  df <- data.frame(
    Variable = names(clusters),
    Cluster  = as.integer(clusters),
    row.names = NULL
  )

  res <- do.call(rbind, lapply(split(df, df$Cluster), function(grp) {
    vars <- grp$Variable
    sub  <- X[, vars, drop = FALSE]

    # moyenne des correlations (optionnel)
    if (ncol(sub) >= 2 && all(sapply(sub, is.numeric))) {
      cor_mat <- cor(sub)
      mean_cor <- mean(abs(cor_mat[upper.tri(cor_mat)]))
    } else {
      mean_cor <- NA
    }

    # top variables utilisant variance
    var_imp <- sort(apply(sub, 2, var), decreasing = TRUE)
    top_vars <- paste(names(var_imp)[1:min(5, length(var_imp))], collapse = ", ")

    data.frame(
      Cluster = unique(grp$Cluster),
      n_vars  = length(vars),
      Mean_Correlation = round(mean_cor, 3),
      Top_Variables = top_vars
    )
  }))

  return(res)
}
