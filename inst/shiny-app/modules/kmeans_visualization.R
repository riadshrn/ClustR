# ==========================================================
# MODULE : kmeans_visualization (ENHANCED - VERSION PRO)
# - Toutes les visualisations K-means (PCA, Silhouette, Dendro, Heatmap)
# - Métriques de qualité (Q, inertie W/B/T)
# - Support des variables ILLUSTRATIVES (multi)
# - Visualisation AVANT/APRÈS clustering
# - Interprétation LLM (Mistral)
# - Section résultats détaillés
# - Plot importance des variables
# - Options PCA avancées
# - Compatible avec VariableClustering + ClustKMeansVar
# ==========================================================

kmeans_visualization_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # ========== CSS ==========
    tags$style(HTML("
      .metric-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin: 10px 0;
        text-align: center;
      }
      .metric-value {
        font-size: 32px;
        font-weight: bold;
        margin: 10px 0;
      }
      .metric-label {
        font-size: 14px;
        opacity: 0.9;
      }

      .interpret-box {
         background: linear-gradient(135deg, #fafafa 0%, #f5f7fa 100%);
         border-left: 5px solid #2c3e50;
         padding: 25px 30px;
         border-radius: 10px;
         font-size: 16px;
         line-height: 1.8;
         color: #2c3e50;
         margin-top: 25px;
         box-shadow: 0 4px 6px rgba(0,0,0,0.07);
         white-space: normal;
      }

      .interpret-box h1 {
         color: #1a237e;
         font-size: 28px;
         font-weight: 700;
         margin-top: 30px;
         margin-bottom: 15px;
         border-bottom: 3px solid #3f51b5;
         padding-bottom: 10px;
      }

      .interpret-box h2 {
         color: #283593;
         font-size: 24px;
         font-weight: 600;
         margin-top: 25px;
         margin-bottom: 12px;
         border-bottom: 2px solid #5c6bc0;
         padding-bottom: 8px;
      }

      .interpret-box h3 {
         color: #303f9f;
         font-size: 20px;
         font-weight: 600;
         margin-top: 20px;
         margin-bottom: 10px;
      }

      .interpret-box table {
         width: 100%;
         border-collapse: collapse;
         margin: 20px 0;
         background: white;
         box-shadow: 0 2px 4px rgba(0,0,0,0.08);
         border-radius: 8px;
         overflow: hidden;
      }

      .interpret-box thead {
         background: linear-gradient(135deg, #3f51b5 0%, #283593 100%);
         color: white;
      }

      .interpret-box th {
         padding: 14px 16px;
         text-align: left;
         font-weight: 600;
         font-size: 15px;
         text-transform: uppercase;
         letter-spacing: 0.5px;
         border: none;
      }

      .interpret-box td {
         padding: 12px 16px;
         border-bottom: 1px solid #e0e0e0;
         font-size: 15px;
      }

      .interpret-box tbody tr:hover {
         background-color: #f5f7fa;
         transition: background-color 0.2s ease;
      }

      .spinner-line {
        display: flex;
        align-items: center;
        gap: 12px;
        font-weight: 600;
        color: #2c3e50;
        font-size: 18px;
      }

      .before-after-info {
        background: #e8f4f8;
        border-left: 4px solid #2196F3;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }

      .section-card {
        background: white;
        border-radius: 8px;
        padding: 20px;
        margin: 15px 0;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }

      .section-card h4 {
        margin-top: 0;
        color: #667eea;
        border-bottom: 2px solid #667eea;
        padding-bottom: 10px;
        margin-bottom: 15px;
      }
    ")),

    # ========== Header avec métriques ==========
    fluidRow(
      box(
        width = 12,
        title = "Métriques de qualité K-means",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,

        fluidRow(
          column(3, uiOutput(ns("metric_Q"))),
          column(3, uiOutput(ns("metric_W"))),
          column(3, uiOutput(ns("metric_B"))),
          column(3, uiOutput(ns("metric_convergence")))
        ),

        hr(),

        fluidRow(
          column(6,
                 h4("Inertie par cluster"),
                 plotOutput(ns("inertia_plot"), height = "250px")
          ),
          column(6,
                 h4("Distances entre centroïdes"),
                 DTOutput(ns("centroid_distances"))
          )
        )
      )
    ),

    # ========== Onglets de visualisation ==========
    fluidRow(
      box(
        width = 3,
        title = "Options de visualisation",
        status = "info",
        solidHeader = TRUE,

        selectInput(
          ns("plot_type"), "Type de plot :",
          choices = c(
            "PCA Globale" = "pca_global",
            "Avant/Après Clustering" = "before_after",
            "Silhouette" = "silhouette",
            "PCA par Cluster" = "pca_cluster",
            "Dendrogramme" = "dendrogram",
            "Heatmap" = "heatmap",
            "Importance Variables" = "importance",
            "Tableau Variables" = "table"
          )
        ),

        hr(),

        # Options PCA
        conditionalPanel(
          condition = sprintf("input['%s'] == 'pca_global' || input['%s'] == 'before_after'",
                              ns("plot_type"), ns("plot_type")),
          checkboxInput(ns("use_pca"), "Utiliser PCA", value = TRUE),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("use_pca")),
            numericInput(ns("pc_x"), "Axe X (PC):", value = 1, min = 1, max = 10),
            numericInput(ns("pc_y"), "Axe Y (PC):", value = 2, min = 1, max = 10)
          )
        ),

        # Options Silhouette
        conditionalPanel(
          condition = sprintf("input['%s'] == 'silhouette'", ns("plot_type")),
          actionButton(ns("compute_sil"), "Calculer silhouette",
                       class = "btn-primary btn-block"),
          br(),
          uiOutput(ns("sil_info"))
        ),

        # Options cluster spécifique
        conditionalPanel(
          condition = sprintf("input['%s'] == 'pca_cluster'", ns("plot_type")),
          uiOutput(ns("cluster_selector"))
        ),

        hr(),

        numericInput(ns("plot_width"), "Largeur (px):", value = 1200, min = 600, max = 2000),
        numericInput(ns("plot_height"), "Hauteur (px):", value = 800, min = 400, max = 1600),

        hr(),

        downloadButton(ns("download_plot"), "Télécharger Plot", class = "btn-success btn-block")
      ),

      box(
        width = 9,
        title = "Visualisation K-means",
        status = "success",
        solidHeader = TRUE,

        # Plot PLOTLY (affiché seulement pour pca_global)
        conditionalPanel(
          condition = sprintf("input['%s'] == 'pca_global'", ns("plot_type")),
          plotly::plotlyOutput(ns("main_plot_interactive"), height = "700px")
        ),

        # Plot STATIQUE (affiché pour tous les autres)
        conditionalPanel(
          condition = sprintf("input['%s'] != 'pca_global'", ns("plot_type")),
          plotOutput(ns("main_plot_static"), height = "700px")
        )
      )
    ),

    # ========== Variables Illustratives ==========
    fluidRow(
      box(
        width = 12,
        title = "Variables Illustratives (projection)",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        fluidRow(
          column(4,
                 h4("Sélection des illustratives"),
                 uiOutput(ns("illustrative_selector")),
                 actionButton(ns("project_illustratives"),
                              "Projeter sur les clusters",
                              class = "btn-primary btn-block",
                              icon = icon("project-diagram"))
          ),
          column(8,
                 h4("Projection PCA avec illustratives"),
                 plotly::plotlyOutput(ns("illustrative_plot"), height = "600px")
          )
        )
      )
    ),

    # ========== Résultats détaillés ==========
    fluidRow(
      box(
        width = 12,
        title = "Résultats détaillés du clustering",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        fluidRow(
          column(6,
                 div(class = "section-card",
                     h4("Top Variables par Cluster"),
                     DTOutput(ns("top_vars_table"))
                 )
          ),
          column(6,
                 div(class = "section-card",
                     h4("Statistiques par Cluster"),
                     DTOutput(ns("cluster_stats_table"))
                 )
          )
        ),

        hr(),

        fluidRow(
          column(12,
                 div(class = "section-card",
                     h4("Matrice de distances inter-clusters"),
                     plotOutput(ns("inter_cluster_distances"), height = "400px")
                 )
          )
        )
      )
    ),

  )
}

kmeans_visualization_server <- function(id, model, data, clusters,
                                        illustrative_vars = NULL,
                                        method = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    cat("\n========================================\n")
    cat("KMEANS VIZ SERVER - ENHANCED - INIT\n")
    cat("========================================\n")

    # ========== Reactive Values ==========
    rv <- reactiveValues(
      sil_computed = FALSE,
      sil_results = NULL,
      illu_proj = NULL,
      last_prompt = NULL
    )

    # ====================================================
    # HELPER : Vérifier si c'est un modèle K-means
    # ====================================================
    is_kmeans <- reactive({
      cat("\n--- is_kmeans() appelé ---\n")

      if(is.null(model())) {
        cat("❌ model() est NULL\n")
        return(FALSE)
      }

      cat("✓ model() existe, classe:", paste(class(model()), collapse = " "), "\n")

      if (inherits(model(), "VariableClustering")) {
        cat("✓ C'est un VariableClustering\n")
        cat("  Method:", model()$method, "\n")
        result <- model()$method == "kmeans"
        cat("  is_kmeans?", result, "\n")
        return(result)
      }

      if (inherits(model(), "ClustKMeansVar")) {
        cat("✓ C'est directement un ClustKMeansVar\n")
        return(TRUE)
      }

      cat("❌ Type de modèle non reconnu\n")
      FALSE
    })

    # ====================================================
    # HELPER : Récupérer le modèle interne K-means
    # ====================================================
    get_kmeans_model <- reactive({
      cat("\n--- get_kmeans_model() appelé ---\n")

      if(is.null(model())) {
        cat("❌ model() est NULL\n")
        return(NULL)
      }

      if (inherits(model(), "VariableClustering")) {
        cat("✓ Extraction depuis VariableClustering\n")
        km <- model()$model
        cat("  Modèle interne classe:", paste(class(km), collapse = " "), "\n")
        return(km)
      }

      if (inherits(model(), "ClustKMeansVar")) {
        cat("✓ Déjà un ClustKMeansVar\n")
        return(model())
      }

      cat("❌ Impossible d'extraire le modèle K-means\n")
      NULL
    })

    # ====================================================
    # MÉTRIQUES
    # ====================================================

    output$metric_Q <- renderUI({
      req(is_kmeans())
      km <- get_kmeans_model()
      req(km)

      Q <- km$Q
      if(is.null(Q)) Q <- 0

      tags$div(
        class = "metric-box",
        tags$div(class = "metric-label", "Qualité (Q = B/T)"),
        tags$div(class = "metric-value", sprintf("%.3f", Q)),
        tags$div(class = "metric-label", "Plus haut = mieux")
      )
    })

    output$metric_W <- renderUI({
      req(is_kmeans())
      km <- get_kmeans_model()
      req(km)

      W <- km$W_total
      if(is.null(W)) W <- 0

      tags$div(
        class = "metric-box",
        tags$div(class = "metric-label", "Inertie Intra (W)"),
        tags$div(class = "metric-value", sprintf("%.2f", W)),
        tags$div(class = "metric-label", "Plus bas = mieux")
      )
    })

    output$metric_B <- renderUI({
      req(is_kmeans())
      km <- get_kmeans_model()
      req(km)

      B <- km$B_total
      if(is.null(B)) B <- 0

      tags$div(
        class = "metric-box",
        tags$div(class = "metric-label", "Inertie Inter (B)"),
        tags$div(class = "metric-value", sprintf("%.2f", B)),
        tags$div(class = "metric-label", "Plus haut = mieux")
      )
    })

    output$metric_convergence <- renderUI({
      req(is_kmeans())
      km <- get_kmeans_model()
      req(km)

      conv <- km$convergence
      iter <- km$iter

      if(is.null(conv)) conv <- FALSE
      if(is.null(iter)) iter <- 0

      tags$div(
        class = "metric-box",
        tags$div(class = "metric-label", "Convergence"),
        tags$div(class = "metric-value",
                 if(conv) "✅" else "❌"),
        tags$div(class = "metric-label", paste(iter, "itérations"))
      )
    })

    # ====================================================
    # INERTIE PAR CLUSTER
    # ====================================================
    output$inertia_plot <- renderPlot({
      req(is_kmeans())
      km <- get_kmeans_model()
      req(km)

      W_k <- km$W_k

      if(is.null(W_k) || length(W_k) == 0) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Inerties par cluster non disponibles", cex = 1.5)
        return()
      }

      df <- data.frame(
        Cluster = factor(1:length(W_k)),
        Inertia = W_k
      )

      ggplot2::ggplot(df, ggplot2::aes(x = Cluster, y = Inertia, fill = Cluster)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Inertia)),
                           vjust = -0.5, size = 4) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Inertie intra par cluster",
                      y = "W_k") +
        ggplot2::theme(legend.position = "none")
    })

    # ====================================================
    # DISTANCES ENTRE CENTROÏDES
    # ====================================================
    output$centroid_distances <- renderDT({
      req(is_kmeans())
      km <- get_kmeans_model()
      req(km)

      dist_mat <- km$center_distances

      if(is.null(dist_mat) || !is.matrix(dist_mat) || nrow(dist_mat) == 0) {
        return(datatable(data.frame(Message = "Distances non disponibles")))
      }

      dist_df <- as.data.frame(round(dist_mat, 2))

      datatable(
        dist_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = TRUE
      )
    })

    # ====================================================
    # FONCTION HELPER : Calculer coords AVANT clustering
    # ====================================================
    compute_before_coords <- function(X_numeric) {
      cor_mat <- cor(X_numeric, use = "pairwise.complete.obs")
      dist_mat <- as.dist(1 - abs(cor_mat))
      mds_result <- cmdscale(dist_mat, k = 2)
      rownames(mds_result) <- colnames(X_numeric)
      colnames(mds_result) <- c("Dim1", "Dim2")
      mds_result
    }

    # ====================================================
    # FONCTION HELPER : Calculer coords APRÈS (utiliser centroids directement)
    # ====================================================
    compute_after_coords <- function(km, clusters_vec) {
      # Utiliser les centroids directement du modèle
      centroids <- km$centers

      if(is.null(centroids)) {
        stop("Centroids non disponibles dans le modèle")
      }

      cat("\n--- compute_after_coords DEBUG ---\n")
      cat("Nombre de variables dans clusters_vec:", length(clusters_vec), "\n")
      cat("Noms clusters_vec:", paste(names(clusters_vec), collapse = ", "), "\n")
      cat("Dimensions centroids:", nrow(centroids), "x", ncol(centroids), "\n")

      # CORRECTION CRITIQUE : Filtrer clusters_vec pour ne garder que les variables du modèle
      if(!is.null(km$var_names)) {
        # Le modèle a une liste explicite de variables
        valid_vars <- intersect(names(clusters_vec), km$var_names)
        cat("Variables valides (dans le modèle):", length(valid_vars), "\n")

        if(length(valid_vars) == 0) {
          stop("Aucune variable commune entre clusters_vec et km$var_names")
        }

        clusters_vec <- clusters_vec[valid_vars]
      }

      cat("Après filtrage:", length(clusters_vec), "variables\n")

      # Chaque variable est représentée par son centroid de cluster
      X_centroids <- centroids[clusters_vec, , drop = FALSE]

      # Assigner les noms de variables
      if(!is.null(names(clusters_vec))) {
        rownames(X_centroids) <- names(clusters_vec)
      }

      cat("Dimensions X_centroids:", nrow(X_centroids), "x", ncol(X_centroids), "\n")

      if(ncol(X_centroids) > 2) {
        pca <- prcomp(X_centroids, center = TRUE, scale. = FALSE)
        coords <- pca$x[, 1:2]
      } else {
        coords <- X_centroids[, 1:2]
      }

      cat("Coordonnées calculées:", nrow(coords), "lignes\n")
      cat("--- FIN DEBUG ---\n\n")

      coords
    }

    # ====================================================
    # PLOT INTERACTIF (PLOTLY) - PCA GLOBALE
    # ====================================================
    output$main_plot_interactive <- plotly::renderPlotly({
      req(is_kmeans())
      km <- get_kmeans_model()
      req(km)

      cat("\n--- main_plot_interactive renderPlotly ---\n")
      cat("plot_type:", input$plot_type, "\n")

      tryCatch({

        cat("\n========================================\n")
        cat("PCA GLOBALE - DÉBUT\n")
        cat("========================================\n")

        cl <- clusters()

        cat("✓ Clusters reçus:", length(cl), "variables\n")
        cat("  Noms (5 premiers):", paste(names(cl)[1:min(5, length(cl))], collapse = ", "), "\n")
        cat("  Clusters uniques:", paste(sort(unique(cl)), collapse = ", "), "\n")

        # IMPORTANT : Filtrer pour ne garder que les variables du modèle
        if(!is.null(km$var_names)) {
          cat("\n--- FILTRAGE DES VARIABLES ---\n")
          cat("Variables dans km$var_names:", length(km$var_names), "\n")
          cat("  Noms:", paste(km$var_names[1:min(5, length(km$var_names))], collapse = ", "), "...\n")

          valid_vars <- intersect(names(cl), km$var_names)
          cat("Variables valides (intersection):", length(valid_vars), "\n")
          cat("  Noms:", paste(valid_vars[1:min(5, length(valid_vars))], collapse = ", "), "...\n")

          cl <- cl[valid_vars]
          cat("✓ Clusters filtrés:", length(cl), "variables\n")
        }

        # APPROCHE CORRECTE : PCA sur les données brutes (comme ClustKMeansVar)
        # Récupérer les données standardisées
        cat("\n--- RÉCUPÉRATION DES DONNÉES ---\n")
        if(!is.null(km$data_raw)) {
          Z <- km$data_raw
          cat("✓ Utilisation de km$data_raw\n")
          cat("  Dimensions:", nrow(Z), "observations x", ncol(Z), "variables\n")
        } else {
          cat("⚠️ km$data_raw non disponible, fallback sur data()\n")
          # Fallback : récupérer les données depuis data()
          X_orig <- data()
          valid_vars_data <- intersect(names(cl), colnames(X_orig))
          Z <- as.matrix(X_orig[, valid_vars_data, drop = FALSE])

          cat("  Variables récupérées:", length(valid_vars_data), "\n")

          # Standardiser si nécessaire
          if(!is.null(km$means) && !is.null(km$sds)) {
            cat("  Standardisation avec km$means et km$sds\n")
            Z <- scale(Z, center = km$means[valid_vars_data], scale = km$sds[valid_vars_data])
          } else {
            cat("  Standardisation par défaut (scale)\n")
            Z <- scale(Z)
          }
          cat("  Dimensions après standardisation:", nrow(Z), "x", ncol(Z), "\n")
        }

        # PCA sur les DONNÉES (observations x variables)
        # Les LOADINGS (rotation) donnent les coordonnées des VARIABLES
        cat("\n--- CALCUL PCA ---\n")
        pca <- prcomp(Z, center = FALSE, scale. = FALSE)

        cat("✓ PCA calculée\n")
        cat("  Dimensions pca$x (scores):", nrow(pca$x), "x", ncol(pca$x), "\n")
        cat("  Dimensions pca$rotation (loadings):", nrow(pca$rotation), "x", ncol(pca$rotation), "\n")

        # LOADINGS = coordonnées des variables dans l'espace PCA
        loadings <- pca$rotation[, 1:2, drop = FALSE]
        variance_explained <- summary(pca)$importance[2, 1:2] * 100

        cat("✓ Loadings extraits (2 premières composantes)\n")
        cat("  Dimensions:", nrow(loadings), "x", ncol(loadings), "\n")
        cat("  Variance expliquée:\n")
        cat("    PC1 =", round(variance_explained[1], 2), "%\n")
        cat("    PC2 =", round(variance_explained[2], 2), "%\n")
        cat("    Total =", round(sum(variance_explained), 2), "%\n")

        # Préparer les données pour plotly
        cat("\n--- PRÉPARATION DONNÉES PLOTLY ---\n")
        plot_df <- data.frame(
          PC1 = loadings[, 1],
          PC2 = loadings[, 2],
          Variable = rownames(loadings),
          Cluster = factor(cl[rownames(loadings)]),
          stringsAsFactors = FALSE
        )

        cat("✓ DataFrame plotly créé:", nrow(plot_df), "lignes\n")
        cat("  Variables:", paste(plot_df$Variable[1:min(5, nrow(plot_df))], collapse = ", "), "...\n")
        cat("  Clusters uniques:", paste(sort(unique(plot_df$Cluster)), collapse = ", "), "\n")
        cat("  Plage PC1: [", round(min(plot_df$PC1), 3), ",", round(max(plot_df$PC1), 3), "]\n")
        cat("  Plage PC2: [", round(min(plot_df$PC2), 3), ",", round(max(plot_df$PC2), 3), "]\n")

        cat("\n--- CRÉATION PLOT PLOTLY ---\n")
        # Créer le plot plotly avec clusters bien visibles
        p <- plotly::plot_ly(
          data = plot_df,
          x = ~PC1,
          y = ~PC2,
          text = ~Variable,
          color = ~Cluster,
          colors = rainbow(max(cl)),
          type = "scatter",
          mode = "markers+text",
          marker = list(
            size = 16,
            line = list(color = "white", width = 2)
          ),
          textposition = "top center",
          textfont = list(size = 11, color = "black"),
          hoverinfo = "text",
          hovertext = ~paste0(
            "Variable: ", Variable, "<br>",
            "Cluster: ", Cluster, "<br>",
            "PC1: ", round(PC1, 3), "<br>",
            "PC2: ", round(PC2, 3)
          )
        )

        cat("✓ Objet plotly créé (scatter)\n")
        cat("  Type: scatter\n")
        cat("  Mode: markers+text\n")
        cat("  Nombre de points:", nrow(plot_df), "\n")

        # Ajouter le layout
        cat("\n--- AJOUT LAYOUT ---\n")
        p <- p %>%
          plotly::layout(
            title = list(
              text = sprintf(
                "PCA Globale K-means (Loadings - Vraies Coordonnées)<br><sub>PC1: %.1f%% | PC2: %.1f%% | Total: %.1f%%</sub>",
                variance_explained[1], variance_explained[2], sum(variance_explained)
              ),
              font = list(size = 16)
            ),
            xaxis = list(
              title = sprintf("PC1 (%.1f%% variance)", variance_explained[1]),
              zeroline = TRUE,
              showgrid = TRUE
            ),
            yaxis = list(
              title = sprintf("PC2 (%.1f%% variance)", variance_explained[2]),
              zeroline = TRUE,
              showgrid = TRUE
            ),
            hovermode = "closest",
            showlegend = TRUE,
            legend = list(
              title = list(text = "Clusters"),
              orientation = "v",
              x = 1.02,
              y = 1
            ),
            shapes = list(
              # Ligne horizontale
              list(
                type = "line",
                x0 = min(plot_df$PC1) - 0.1,
                x1 = max(plot_df$PC1) + 0.1,
                y0 = 0,
                y1 = 0,
                line = list(color = "gray", width = 1, dash = "dash")
              ),
              # Ligne verticale
              list(
                type = "line",
                x0 = 0,
                x1 = 0,
                y0 = min(plot_df$PC2) - 0.1,
                y1 = max(plot_df$PC2) + 0.1,
                line = list(color = "gray", width = 1, dash = "dash")
              )
            )
          )

        cat("✓ Layout ajouté\n")
        cat("  Titre configuré\n")
        cat("  Axes configurés\n")
        cat("  Lignes à x=0 et y=0 ajoutées\n")
        cat("  Légende configurée\n")

        cat("\n========================================\n")
        cat("PCA GLOBALE - FIN\n")
        cat("✓ Plot prêt à être retourné\n")
        cat("========================================\n\n")

        return(p)

      }, error = function(e) {
        # Plotly error plot
        plotly::plot_ly() %>%
          plotly::layout(
            title = paste("Erreur:", e$message),
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      })
    })

    # ====================================================
    # PLOT STATIQUE - TOUS LES AUTRES TYPES
    # ====================================================
    output$main_plot_static <- renderPlot({

      cat("\n========================================\n")
      cat("MAIN_PLOT_STATIC - ENTRY\n")
      cat("========================================\n")

      req(is_kmeans())
      cat("✓ is_kmeans validé\n")

      km <- get_kmeans_model()
      req(km)
      cat("✓ km récupéré\n")

      cat("plot_type actuel:", input$plot_type, "\n")

      tryCatch({
        if(input$plot_type == "before_after") {

          cat("\n========================================\n")
          cat("PLOT AVANT/APRÈS - DÉBUT\n")
          cat("========================================\n")

          # Visualisation Avant/Après
          X_orig <- data()
          numeric_cols <- sapply(X_orig, is.numeric)
          X_numeric <- X_orig[, numeric_cols, drop = FALSE]

          cat("Données originales:", nrow(X_orig), "obs x", ncol(X_orig), "vars\n")
          cat("Variables numériques:", ncol(X_numeric), "\n")

          cl <- clusters()
          cat("Clusters:", length(cl), "variables\n")
          cat("Clusters uniques:", paste(unique(cl), collapse = ", "), "\n")

          colors <- rainbow(max(cl))[cl]

          # AVANT : MDS sur corrélations
          cat("\n--- AVANT : Calcul MDS sur corrélations ---\n")
          coords_before <- compute_before_coords(X_numeric)
          cat("Coords AVANT dimensions:", nrow(coords_before), "x", ncol(coords_before), "\n")

          # APRÈS : PCA loadings (vraies coordonnées des VARIABLES)
          cat("\n--- APRÈS : Calcul PCA loadings ---\n")

          # Filtrer pour ne garder que les variables du modèle
          if(!is.null(km$var_names)) {
            valid_vars <- intersect(names(cl), km$var_names)
            cat("Variables valides dans le modèle:", length(valid_vars), "\n")
            cl_filtered <- cl[valid_vars]
          } else {
            cl_filtered <- cl
          }

          # Récupérer les données standardisées
          if(!is.null(km$data_raw)) {
            Z <- km$data_raw
            cat("Utilisation de km$data_raw:", nrow(Z), "x", ncol(Z), "\n")
          } else {
            cat("km$data_raw non disponible, standardisation manuelle\n")
            Z <- scale(X_numeric[, names(cl_filtered), drop = FALSE])
          }

          # PCA sur les données
          pca_after <- prcomp(Z, center = FALSE, scale. = FALSE)

          # LOADINGS = coordonnées des VARIABLES
          coords_after <- pca_after$rotation[, 1:2, drop = FALSE]
          variance_explained <- summary(pca_after)$importance[2, 1:2] * 100

          cat("Coords APRÈS (loadings):", nrow(coords_after), "x", ncol(coords_after), "\n")
          cat("Variance expliquée: PC1 =", round(variance_explained[1], 1), "%, PC2 =", round(variance_explained[2], 1), "%\n")

          # Synchroniser les clusters
          cl_plot <- cl_filtered[rownames(coords_after)]
          colors_after <- rainbow(max(cl_plot))[cl_plot]

          cat("\n--- GÉNÉRATION DES PLOTS ---\n")
          par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

          # AVANT
          cat("Plot AVANT...\n")
          plot(coords_before[, 1], coords_before[, 2],
               col = "gray50", pch = 19, cex = 1.5,
               xlab = "Dimension 1 (MDS)", ylab = "Dimension 2 (MDS)",
               main = "AVANT clustering\n(MDS - Corrélations)")
          text(coords_before[, 1], coords_before[, 2],
               labels = rownames(coords_before), pos = 3, cex = 0.7)
          grid()

          # APRÈS
          cat("Plot APRÈS...\n")
          plot(coords_after[, 1], coords_after[, 2],
               col = colors_after, pch = 19, cex = 1.5,
               xlab = sprintf("PC1 (%.1f%% var)", variance_explained[1]),
               ylab = sprintf("PC2 (%.1f%% var)", variance_explained[2]),
               main = "APRÈS clustering K-means\n(PCA Loadings - Variables)")
          text(coords_after[, 1], coords_after[, 2],
               labels = rownames(coords_after), pos = 3, cex = 0.7)
          grid()
          abline(h = 0, v = 0, lty = 2, col = "gray")

          # Légende
          legend("topright",
                 legend = paste("Cluster", sort(unique(cl_plot))),
                 fill = rainbow(max(cl_plot)),
                 cex = 0.7)

          cat("✓ Plot AVANT/APRÈS terminé\n")
          cat("========================================\n\n")

        } else if(input$plot_type == "silhouette") {

          cat("→ Génération plot SILHOUETTE\n")
          req(rv$sil_computed, rv$sil_results)

          sil_res <- rv$sil_results
          sil <- sil_res$silhouette
          clusters_vec <- clusters()

          order_idx <- order(clusters_vec, -sil)
          sil_ordered <- sil[order_idx]
          clusters_ordered <- clusters_vec[order_idx]
          var_names_ordered <- names(sil)[order_idx]

          n_clust <- max(clusters_vec)
          colors <- rainbow(n_clust)[clusters_ordered]

          par(mar = c(5, 8, 4, 2))
          barplot(
            sil_ordered,
            horiz = TRUE,
            names.arg = var_names_ordered,
            las = 1,
            col = colors,
            border = NA,
            main = "Silhouette par variable",
            xlab = "Coefficient de silhouette",
            xlim = c(min(sil_ordered) - 0.1, 1)
          )
          abline(v = mean(sil, na.rm = TRUE), lty = 2, lwd = 2, col = "black")
          legend("bottomright",
                 legend = paste("Cluster", 1:n_clust),
                 fill = rainbow(n_clust),
                 cex = 0.8)

        } else if(input$plot_type == "pca_cluster") {

          cat("→ Génération plot PCA PAR CLUSTER\n")
          req(input$selected_cluster)
          cl_id <- as.integer(input$selected_cluster)

          vars_in_cluster <- sum(clusters() == cl_id)

          if (vars_in_cluster < 2) {
            plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, paste("Cluster", cl_id, "contient < 2 variables.\nPCA impossible."),
                 cex = 1.5)
          } else {
            km$plot_cluster_pca(cl_id)
          }

        } else if(input$plot_type == "dendrogram") {

          cat("→ Génération plot DENDROGRAMME\n")
          km$plot_dendrogram()

        } else if(input$plot_type == "heatmap") {

          cat("→ Génération plot HEATMAP\n")

          if(!is.null(data()) && !is.null(clusters())) {
            data_mat <- as.data.frame(data())
            num_cols <- sapply(data_mat, is.numeric)

            if(!any(num_cols)) {
              plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
              text(1, 1, "Données non numériques", cex = 1.5)
              return()
            }

            data_numeric <- as.matrix(data_mat[, num_cols, drop = FALSE])
            order_idx <- order(clusters())
            data_ordered <- data_numeric[, order_idx, drop = FALSE]
            cor_mat <- cor(data_ordered, use = "pairwise.complete.obs")

            pheatmap::pheatmap(
              cor_mat,
              color = colorRampPalette(c("blue", "white", "red"))(100),
              breaks = seq(-1, 1, length.out = 101),
              cluster_rows = FALSE,
              cluster_cols = FALSE,
              main = "Heatmap de corrélation (ordonnée par cluster)"
            )
          }

        } else if(input$plot_type == "importance") {

          cat("→ Génération plot IMPORTANCE\n")

          # Importance basée sur distance au centroid
          X_orig <- data()
          numeric_cols <- sapply(X_orig, is.numeric)
          X_numeric <- as.matrix(X_orig[, numeric_cols, drop = FALSE])

          cl <- clusters()
          centroids <- km$centroids

          # Distance de chaque variable à son centroid
          dists <- sapply(seq_along(cl), function(i) {
            k <- cl[i]
            var_data <- X_numeric[, i]
            centroid <- centroids[k, ]

            # Distance euclidienne dans l'espace des observations
            sqrt(sum((var_data - mean(var_data))^2))
          })

          # Importance = 1 / (1 + distance normalisée)
          imp <- 1 / (1 + dists / max(dists))
          names(imp) <- names(cl)
          imp <- sort(imp, decreasing = TRUE)

          par(mar = c(5, 10, 4, 2))
          barplot(imp, horiz = TRUE, las = 1,
                  col = rainbow(max(cl))[cl[names(imp)]],
                  main = "Importance des variables (proximité au centroid)",
                  xlab = "Score d'importance")

        } else if(input$plot_type == "table") {

          cat("→ Type TABLE - pas de plot graphique\n")
          # Afficher un message car le tableau est géré ailleurs
          plot.new()
          text(0.5, 0.5, "Tableau affiché ci-dessous", cex = 1.5, col = "blue")

        } else {
          cat("→ Type de plot NON RECONNU:", input$plot_type, "\n")
          plot.new()
          text(0.5, 0.5, paste("Type de plot non supporté:", input$plot_type), cex = 1.5)
        }

      }, error = function(e) {
        cat("❌ ERREUR dans main_plot_static:", e$message, "\n")
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Erreur plot:", e$message), cex = 1.2, col = "red")
      })
    })

    # ====================================================
    # SILHOUETTE - COMPUTE
    # ====================================================
    observeEvent(input$compute_sil, {
      req(is_kmeans())

      withProgress(message = "Calcul de la silhouette...", value = 0.5, {
        tryCatch({

          # Récupérer TOUTES les données nécessaires AVANT le calcul
          km <- isolate(get_kmeans_model())
          req(km)

          X_for_sil <- if(!is.null(km$original_data)) {
            km$original_data
          } else {
            isolate(data())
          }

          if(is.null(X_for_sil)) {
            showNotification("Données non disponibles pour la silhouette", type = "error")
            return()
          }

          # Extraire TOUTES les infos du modèle AVANT le calcul
          cluster_assignments <- km$cluster_assignments
          means <- km$means
          sds <- km$sds
          var_names <- km$var_names

          if(is.null(cluster_assignments) || is.null(means) || is.null(sds)) {
            showNotification("Informations de clustering manquantes", type = "error")
            return()
          }

          # Calcul de la silhouette avec les données isolées
          X_scaled <- scale(X_for_sil, center = means, scale = sds)
          cor_mat <- cor(X_scaled)
          dist_mat <- as.dist(1 - abs(cor_mat))

          if(!requireNamespace("cluster", quietly = TRUE)) {
            showNotification("Package 'cluster' requis", type = "error")
            return()
          }

          sil_obj <- cluster::silhouette(cluster_assignments, dist_mat)
          sil_values <- sil_obj[, 3]
          names(sil_values) <- var_names

          # Stocker dans rv (reactive values)
          rv$sil_results <- list(
            silhouette = sil_values,
            silhouette_mean = mean(sil_values, na.rm = TRUE),
            silhouette_obj = sil_obj
          )
          rv$sil_computed <- TRUE

          showNotification("Silhouette calculée avec succès!", type = "message")

        }, error = function(e) {
          showNotification(paste("Erreur silhouette:", e$message),
                           type = "error", duration = 10)
        })
      })
    })

    output$sil_info <- renderUI({
      if(rv$sil_computed && !is.null(rv$sil_results)) {
        tagList(
          p(strong("Silhouette moyenne:"),
            sprintf("%.3f", rv$sil_results$silhouette_mean)),
          p(em("Valeurs: [-1, 1]")),
          p(em("Plus proche de 1 = meilleur clustering"))
        )
      } else {
        p("Cliquez pour calculer la silhouette")
      }
    })

    # ====================================================
    # CLUSTER SELECTOR
    # ====================================================
    output$cluster_selector <- renderUI({
      req(is_kmeans(), clusters())
      n_clusters <- max(clusters())

      selectInput(
        ns("selected_cluster"),
        "Cluster à visualiser:",
        choices = 1:n_clusters,
        selected = 1
      )
    })

    # ====================================================
    # VARIABLES ILLUSTRATIVES - SELECTOR
    # ====================================================
    output$illustrative_selector <- renderUI({
      req(data())

      # Variables disponibles (toutes sauf les actives)
      all_vars <- colnames(data())
      active_vars <- names(clusters())
      available_vars <- setdiff(all_vars, active_vars)

      if(length(available_vars) == 0) {
        return(p("Aucune variable illustrative disponible"))
      }

      checkboxGroupInput(
        ns("selected_illustratives"),
        "Sélectionner illustratives:",
        choices = available_vars,
        selected = NULL
      )
    })

    # ====================================================
    # PROJECTION DES ILLUSTRATIVES - AVEC DEBUG
    # ====================================================
    observeEvent(input$project_illustratives, {
      req(input$selected_illustratives)
      req(is_kmeans())
      km <- get_kmeans_model()
      req(km)

      cat("\n========================================\n")
      cat("PROJECTION ILLUSTRATIVES - DEBUG\n")
      cat("========================================\n")
      cat("Variables sélectionnées:", paste(input$selected_illustratives, collapse = ", "), "\n")

      withProgress(message = "Projection des illustratives...", value = 0.5, {

        illu_list <- list()

        for(var_name in input$selected_illustratives) {

          cat("\n--- Traitement:", var_name, "---\n")

          var_data <- data()[[var_name]]
          cat("Type:", class(var_data), "\n")
          cat("Length:", length(var_data), "\n")
          cat("NA count:", sum(is.na(var_data)), "\n")

          if(is.numeric(var_data)) {
            cat("→ Variable NUMÉRIQUE\n")

            # Variable numérique : projection standard
            var_type <- "numeric"

            # Trouver le cluster le plus proche (corrélation avec les variables du cluster)
            X_active <- data()[, names(clusters()), drop = FALSE]

            cat("  Calcul corrélations avec clusters...\n")
            correlations <- sapply(1:max(clusters()), function(k) {
              cluster_vars <- names(clusters())[clusters() == k]
              cat("    Cluster", k, ":", length(cluster_vars), "variables\n")

              cluster_data <- X_active[, cluster_vars, drop = FALSE]

              if(ncol(cluster_data) == 0) return(0)

              cors <- cor(var_data, cluster_data, use = "pairwise.complete.obs")
              mean_cor <- mean(cors, na.rm = TRUE)
              cat("      Corrélation moyenne:", round(mean_cor, 3), "\n")
              mean_cor
            })

            assigned_cluster <- which.max(abs(correlations))
            cat("  ✓ Assigné au cluster:", assigned_cluster, "\n")
            cat("  ✓ Corrélation:", round(correlations[assigned_cluster], 3), "\n")

            illu_list[[var_name]] <- list(
              type = var_type,
              cluster = assigned_cluster,
              correlation = correlations[assigned_cluster],
              coords = NULL  # Sera calculé pour le plot
            )

          } else if(is.factor(var_data) || is.character(var_data)) {
            cat("→ Variable CATÉGORIELLE\n")

            # Variable catégorielle
            var_type <- "factor"

            if(is.character(var_data)) var_data <- as.factor(var_data)

            levels_list <- levels(var_data)
            cat("  Niveaux:", paste(levels_list, collapse = ", "), "\n")

            # Limiter le nombre de niveaux si trop nombreux
            if(length(levels_list) > 15) {
              cat("  ⚠️ Trop de niveaux (", length(levels_list), "), limite à 15\n")
              levels_list <- levels_list[1:15]
            }

            # Stocker la variable avec ses niveaux
            illu_list[[var_name]] <- list(
              type = var_type,
              levels = levels_list,
              n_levels = length(levels_list),
              coords = NULL
            )

            cat("  → Variable catégorielle enregistrée avec", length(levels_list), "niveaux\n")

          } else {
            cat("→ Type NON SUPPORTÉ, ignoré\n")
          }
        }

        cat("\n========================================\n")
        cat("RÉSUMÉ:\n")
        cat("Nombre de variables traitées:", length(illu_list), "\n")
        cat("Noms:", paste(names(illu_list), collapse = ", "), "\n")
        cat("========================================\n\n")

        rv$illu_proj <- illu_list
        showNotification(
          paste(length(illu_list), "variable(s) illustrative(s) projetée(s)!"),
          type = "message"
        )
      })
    })

    # ====================================================
    # PLOT AVEC ILLUSTRATIVES - PLOTLY INTERACTIF + DEBUG
    # ====================================================
    output$illustrative_plot <- plotly::renderPlotly({

      cat("\n========================================\n")
      cat("ILLUSTRATIVE PLOT - RENDER\n")
      cat("========================================\n")

      req(is_kmeans())

      # Si pas de projection, afficher un message
      if(is.null(rv$illu_proj) || length(rv$illu_proj) == 0) {
        cat("❌ Aucune projection trouvée\n")
        cat("   rv$illu_proj est:", if(is.null(rv$illu_proj)) "NULL" else paste("vide, length =", length(rv$illu_proj)), "\n")
        return(
          plotly::plot_ly() %>%
            plotly::add_annotations(
              text = "Aucune variable illustrative projetée.\n\nSélectionnez des variables et cliquez sur\n'Projeter sur les clusters'",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5,
              showarrow = FALSE,
              font = list(size = 16, color = "gray")
            ) %>%
            plotly::layout(
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
            )
        )
      }

      cat("✓ Projections trouvées:", length(rv$illu_proj), "\n")
      cat("  Variables:", paste(names(rv$illu_proj), collapse = ", "), "\n")
      cat("  Types:", paste(sapply(rv$illu_proj, function(x) x$type), collapse = ", "), "\n")

      km <- get_kmeans_model()
      req(km)

      tryCatch({
        cat("\n--- RÉCUPÉRATION DONNÉES ACTIVES ---\n")
        # PCA des variables actives
        X_orig <- data()
        active_vars <- names(clusters())
        X_active <- X_orig[, active_vars, drop = FALSE]

        cat("✓ Données originales:", nrow(X_orig), "obs x", ncol(X_orig), "vars\n")
        cat("✓ Variables actives sélectionnées:", length(active_vars), "\n")
        cat("  Noms (5 premiers):", paste(active_vars[1:min(5, length(active_vars))], collapse = ", "), "...\n")

        # Ne garder que les numériques pour la PCA
        numeric_cols <- sapply(X_active, is.numeric)
        X_active_numeric <- X_active[, numeric_cols, drop = FALSE]

        cat("✓ Variables actives numériques:", ncol(X_active_numeric), "\n")
        cat("  Noms:", paste(colnames(X_active_numeric)[1:min(5, ncol(X_active_numeric))], collapse = ", "), "...\n")

        if(ncol(X_active_numeric) < 2) {
          cat("❌ Pas assez de variables numériques (<2)\n")
          return(
            plotly::plot_ly() %>%
              plotly::add_annotations(
                text = "Pas assez de variables numériques actives pour la PCA",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
                showarrow = FALSE,
                font = list(size = 16, color = "red")
              )
          )
        }

        cat("\n--- CALCUL PCA ---\n")
        # PCA sur les données (observations x variables)
        # On veut les LOADINGS (rotation) qui donnent les coordonnées des VARIABLES
        if(!is.null(km$data_raw)) {
          Z <- km$data_raw[, colnames(X_active_numeric), drop = FALSE]
          cat("✓ Utilisation de km$data_raw\n")
        } else {
          Z <- scale(X_active_numeric)
          cat("✓ Standardisation manuelle\n")
        }

        cat("  Dimensions Z:", nrow(Z), "x", ncol(Z), "\n")

        pca <- prcomp(Z, center = FALSE, scale. = FALSE)

        cat("✓ PCA calculée\n")
        cat("  pca$x dimensions:", nrow(pca$x), "x", ncol(pca$x), "\n")
        cat("  pca$rotation dimensions:", nrow(pca$rotation), "x", ncol(pca$rotation), "\n")

        # LOADINGS = coordonnées des variables dans l'espace PCA
        loadings_active <- pca$rotation[, 1:2, drop = FALSE]
        variance_explained <- summary(pca)$importance[2, 1:2] * 100

        cat("✓ Loadings extraits\n")
        cat("  Dimensions:", nrow(loadings_active), "x", ncol(loadings_active), "\n")
        cat("  Variance expliquée:\n")
        cat("    PC1 =", round(variance_explained[1], 2), "%\n")
        cat("    PC2 =", round(variance_explained[2], 2), "%\n")
        cat("    Total =", round(sum(variance_explained), 2), "%\n")

        cl <- clusters()[rownames(loadings_active)]
        cat("✓ Clusters des variables actives:", length(cl), "\n")
        cat("  Clusters uniques:", paste(sort(unique(cl)), collapse = ", "), "\n")

        # Préparer données actives pour plotly
        df_active <- data.frame(
          PC1 = loadings_active[, 1],
          PC2 = loadings_active[, 2],
          Variable = rownames(loadings_active),
          Cluster = factor(cl),
          Type = "Active",
          stringsAsFactors = FALSE
        )

        # Créer le plot de base avec variables actives
        p <- plotly::plot_ly() %>%
          plotly::add_markers(
            data = df_active,
            x = ~PC1,
            y = ~PC2,
            text = ~Variable,
            color = ~Cluster,
            colors = rainbow(max(cl)),
            name = ~paste("Cluster", Cluster),
            marker = list(size = 12, line = list(color = "white", width = 1)),
            hoverinfo = "text",
            hovertext = ~paste0(
              "Variable: ", Variable, "<br>",
              "Cluster: ", Cluster, "<br>",
              "Type: Active<br>",
              "PC1: ", round(PC1, 3), "<br>",
              "PC2: ", round(PC2, 3)
            )
          )

        # Ajouter les variables illustratives
        n_illu_plotted <- 0

        for(var_name in names(rv$illu_proj)) {
          illu <- rv$illu_proj[[var_name]]

          cat("\n--- Plot illustrative:", var_name, "---\n")
          cat("Type:", illu$type, "\n")

          if(illu$type == "numeric") {
            # Projection de la variable numérique dans l'espace PCA
            var_data <- X_orig[[var_name]]

            cat("Valeurs NA:", sum(is.na(var_data)), "/", length(var_data), "\n")

            if(is.numeric(var_data) && !all(is.na(var_data))) {
              # Corrélation avec les VARIABLES actives (pas les observations)
              # var_data: vecteur de n observations
              # X_active_numeric: matrice n x p (n observations, p variables)

              # Calculer la corrélation entre var_data et CHAQUE variable active
              cors_with_vars <- numeric(ncol(X_active_numeric))
              for(j in 1:ncol(X_active_numeric)) {
                cors_with_vars[j] <- cor(var_data, X_active_numeric[, j], use = "pairwise.complete.obs")
              }
              names(cors_with_vars) <- colnames(X_active_numeric)

              cat("Length cors_with_vars:", length(cors_with_vars), "\n")
              cat("Length rotation[,1]:", nrow(pca$rotation), "\n")

              # Maintenant les dimensions correspondent !
              cor_pc1 <- sum(cors_with_vars * pca$rotation[, 1], na.rm = TRUE)
              cor_pc2 <- sum(cors_with_vars * pca$rotation[, 2], na.rm = TRUE)

              cat("Coordonnées PCA: (", round(cor_pc1, 3), ",", round(cor_pc2, 3), ")\n")

              # Facteur d'échelle pour les vecteurs
              scale_factor <- 3

              # Ajouter le vecteur comme annotation (flèche)
              p <- p %>%
                plotly::add_annotations(
                  x = cor_pc1 * scale_factor,
                  y = cor_pc2 * scale_factor,
                  ax = 0,
                  ay = 0,
                  xref = "x", yref = "y",
                  axref = "x", ayref = "y",
                  text = "",
                  showarrow = TRUE,
                  arrowhead = 2,
                  arrowsize = 1.5,
                  arrowwidth = 3,
                  arrowcolor = "blue"
                ) %>%
                plotly::add_annotations(
                  x = cor_pc1 * scale_factor * 1.15,
                  y = cor_pc2 * scale_factor * 1.15,
                  text = var_name,
                  showarrow = FALSE,
                  font = list(size = 12, color = "blue", family = "Arial Black"),
                  bgcolor = "rgba(255,255,255,0.8)",
                  bordercolor = "blue",
                  borderwidth = 1,
                  borderpad = 4
                )

              n_illu_plotted <- n_illu_plotted + 1
              cat("✓ Variable plotée\n")
            } else {
              cat("❌ Variable ignorée (non numérique ou tout NA)\n")
            }

          } else if(illu$type == "factor") {
            # Projection de chaque niveau du facteur
            cat("→ Projection des niveaux du facteur\n")

            var_data <- X_orig[[var_name]]
            if(is.character(var_data)) var_data <- as.factor(var_data)

            levels_to_plot <- illu$levels
            cat("  Niveaux à projeter:", length(levels_to_plot), "\n")

            # Pour chaque niveau, calculer ses coordonnées PCA
            for(lev in levels_to_plot) {
              # Créer une variable indicatrice (0/1) pour ce niveau
              indicator <- as.numeric(var_data == lev)

              # Corrélation de cette indicatrice avec chaque variable active
              cors_lev <- numeric(ncol(X_active_numeric))
              for(j in 1:ncol(X_active_numeric)) {
                cors_lev[j] <- cor(indicator, X_active_numeric[, j], use = "pairwise.complete.obs")
              }

              # Projection sur PC1 et PC2
              cor_pc1_lev <- sum(cors_lev * pca$rotation[, 1], na.rm = TRUE)
              cor_pc2_lev <- sum(cors_lev * pca$rotation[, 2], na.rm = TRUE)

              cat("  Niveau '", lev, "': (", round(cor_pc1_lev, 3), ",", round(cor_pc2_lev, 3), ")\n", sep = "")

              # Facteur d'échelle pour les vecteurs
              scale_factor <- 2.5

              # Ajouter le vecteur comme annotation (flèche verte pour facteurs)
              p <- p %>%
                plotly::add_annotations(
                  x = cor_pc1_lev * scale_factor,
                  y = cor_pc2_lev * scale_factor,
                  ax = 0,
                  ay = 0,
                  xref = "x", yref = "y",
                  axref = "x", ayref = "y",
                  text = "",
                  showarrow = TRUE,
                  arrowhead = 2,
                  arrowsize = 1.3,
                  arrowwidth = 2.5,
                  arrowcolor = "darkgreen"
                ) %>%
                plotly::add_annotations(
                  x = cor_pc1_lev * scale_factor * 1.15,
                  y = cor_pc2_lev * scale_factor * 1.15,
                  text = paste0(var_name, ".", lev),
                  showarrow = FALSE,
                  font = list(size = 10, color = "darkgreen", family = "Arial"),
                  bgcolor = "rgba(240,255,240,0.9)",
                  bordercolor = "darkgreen",
                  borderwidth = 1,
                  borderpad = 3
                )

              n_illu_plotted <- n_illu_plotted + 1
            }

            cat("✓", length(levels_to_plot), "niveaux plotés\n")
          }
        }

        cat("\n========================================\n")
        cat("RÉSUMÉ PLOT:\n")
        cat("Variables actives:", nrow(df_active), "\n")
        cat("Variables illustratives plotées:", n_illu_plotted, "\n")
        cat("========================================\n\n")

        # Layout final
        p <- p %>%
          plotly::layout(
            title = list(
              text = sprintf(
                "PCA avec Variables Illustratives<br><sub>PC1: %.1f%% | PC2: %.1f%% variance | %d illustrative(s)</sub>",
                variance_explained[1], variance_explained[2], n_illu_plotted
              ),
              font = list(size = 16)
            ),
            xaxis = list(
              title = sprintf("PC1 (%.1f%% variance)", variance_explained[1]),
              zeroline = TRUE,
              showgrid = TRUE
            ),
            yaxis = list(
              title = sprintf("PC2 (%.1f%% variance)", variance_explained[2]),
              zeroline = TRUE,
              showgrid = TRUE
            ),
            hovermode = "closest",
            showlegend = TRUE,
            legend = list(
              title = list(text = "Variables Actives"),
              orientation = "v",
              x = 1.02,
              y = 1
            )
          )

        return(p)

      }, error = function(e) {
        cat("❌ ERREUR:", e$message, "\n")
        plotly::plot_ly() %>%
          plotly::add_annotations(
            text = paste("Erreur:", e$message),
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5,
            showarrow = FALSE,
            font = list(size = 14, color = "red")
          )
      })
    })

    # ====================================================
    # RÉSULTATS DÉTAILLÉS - TOP VARIABLES
    # ====================================================
    output$top_vars_table <- renderDT({
      req(is_kmeans(), clusters())

      # Calculer l'importance
      X_orig <- data()
      numeric_cols <- sapply(X_orig, is.numeric)
      X_numeric <- as.matrix(X_orig[, numeric_cols, drop = FALSE])

      cl <- clusters()
      km <- get_kmeans_model()
      centroids <- km$centroids

      dists <- sapply(seq_along(cl), function(i) {
        k <- cl[i]
        var_data <- X_numeric[, i]
        centroid <- centroids[k, ]
        sqrt(sum((var_data - mean(var_data))^2))
      })

      imp <- 1 / (1 + dists / max(dists))
      names(imp) <- names(cl)

      # Top 10
      top_idx <- order(imp, decreasing = TRUE)[1:min(10, length(imp))]

      df <- data.frame(
        Rang = 1:length(top_idx),
        Variable = names(imp)[top_idx],
        Cluster = cl[top_idx],
        Importance = round(imp[top_idx], 3),
        stringsAsFactors = FALSE
      )

      datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE
      )
    })

    # ====================================================
    # RÉSULTATS DÉTAILLÉS - STATS PAR CLUSTER
    # ====================================================
    output$cluster_stats_table <- renderDT({
      req(is_kmeans(), clusters())
      km <- get_kmeans_model()
      req(km)

      cl <- clusters()
      n_clusters <- max(cl)

      W_k <- km$W_k
      if(is.null(W_k)) W_k <- rep(NA, n_clusters)

      stats_list <- lapply(1:n_clusters, function(k) {
        vars_k <- names(cl)[cl == k]
        inertia_k <- if(!is.null(W_k) && length(W_k) >= k) W_k[k] else NA

        data.frame(
          Cluster = k,
          N_Variables = length(vars_k),
          Inertie = if(!is.na(inertia_k)) round(inertia_k, 2) else NA,
          Pct_Total = sprintf("%.1f%%", 100 * length(vars_k) / length(cl)),
          stringsAsFactors = FALSE
        )
      })

      df <- do.call(rbind, stats_list)

      datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE
      )
    })

    # ====================================================
    # RÉSULTATS DÉTAILLÉS - DISTANCES INTER-CLUSTERS
    # ====================================================
    output$inter_cluster_distances <- renderPlot({
      req(is_kmeans())
      km <- get_kmeans_model()
      req(km)

      dist_mat <- km$center_distances

      if(is.null(dist_mat) || !is.matrix(dist_mat) || nrow(dist_mat) == 0) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Distances inter-clusters non disponibles", cex = 1.5)
        return()
      }

      pheatmap::pheatmap(
        dist_mat,
        color = colorRampPalette(c("white", "orange", "red"))(100),
        cluster_rows = FALSE,
        cluster_cols = FALSE,
        display_numbers = TRUE,
        number_format = "%.2f",
        main = "Distances euclidiennes entre centroïdes"
      )
    })



    # ====================================================
    # DOWNLOAD PLOT
    # ====================================================
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("kmeans_", input$plot_type, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        png(file, width = input$plot_width, height = input$plot_height)

        km <- get_kmeans_model()

        tryCatch({
          if(input$plot_type == "pca_global") {
            if(input$use_pca) {
              X_orig <- data()
              numeric_cols <- sapply(X_orig, is.numeric)
              X_numeric <- X_orig[, numeric_cols, drop = FALSE]

              pca <- prcomp(t(X_numeric), center = TRUE, scale. = TRUE)

              pc_x <- min(input$pc_x, ncol(pca$x))
              pc_y <- min(input$pc_y, ncol(pca$x))

              coords <- pca$x[, c(pc_x, pc_y)]
              variance_explained <- summary(pca)$importance[2, c(pc_x, pc_y)] * 100

              cl <- clusters()
              colors <- rainbow(max(cl))[cl]

              plot(coords[, 1], coords[, 2],
                   col = colors, pch = 19, cex = 1.8,
                   xlab = sprintf("PC%d (%.1f%% variance)", pc_x, variance_explained[1]),
                   ylab = sprintf("PC%d (%.1f%% variance)", pc_y, variance_explained[2]),
                   main = sprintf("PCA Globale K-means\nPC%d vs PC%d", pc_x, pc_y))
              text(coords[, 1], coords[, 2],
                   labels = rownames(coords), pos = 3, cex = 0.7)
              grid()
            } else {
              km$plot_global_pca()
            }
          } else if(input$plot_type == "before_after") {
            X_orig <- data()
            numeric_cols <- sapply(X_orig, is.numeric)
            X_numeric <- X_orig[, numeric_cols, drop = FALSE]

            cl <- clusters()
            colors <- rainbow(max(cl))[cl]

            coords_before <- compute_before_coords(X_numeric)
            coords_after <- compute_after_coords(km, cl)

            par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

            plot(coords_before[, 1], coords_before[, 2],
                 col = "gray50", pch = 19, cex = 1.5,
                 xlab = "Dimension 1 (MDS)", ylab = "Dimension 2 (MDS)",
                 main = "AVANT clustering\n(Matrice de corrélation)")
            text(coords_before[, 1], coords_before[, 2],
                 labels = rownames(coords_before), pos = 3, cex = 0.7)
            grid()

            plot(coords_after[, 1], coords_after[, 2],
                 col = colors, pch = 19, cex = 1.5,
                 xlab = "PC1", ylab = "PC2",
                 main = "APRÈS clustering K-means\n(Espace des centroids)")
            text(coords_after[, 1], coords_after[, 2],
                 labels = rownames(coords_after), pos = 3, cex = 0.7)
            grid()
          } else if(input$plot_type == "silhouette" && rv$sil_computed) {
            sil_res <- rv$sil_results
            sil <- sil_res$silhouette
            clusters_vec <- clusters()

            order_idx <- order(clusters_vec, -sil)
            sil_ordered <- sil[order_idx]
            clusters_ordered <- clusters_vec[order_idx]
            var_names_ordered <- names(sil)[order_idx]

            n_clust <- max(clusters_vec)
            colors <- rainbow(n_clust)[clusters_ordered]

            par(mar = c(5, 8, 4, 2))
            barplot(
              sil_ordered,
              horiz = TRUE,
              names.arg = var_names_ordered,
              las = 1,
              col = colors,
              border = NA,
              main = "Silhouette par variable",
              xlab = "Coefficient de silhouette"
            )
            abline(v = mean(sil, na.rm = TRUE), lty = 2, lwd = 2, col = "black")
          } else if(input$plot_type == "dendrogram") {
            km$plot_dendrogram()
          } else if(input$plot_type == "importance") {
            X_orig <- data()
            numeric_cols <- sapply(X_orig, is.numeric)
            X_numeric <- as.matrix(X_orig[, numeric_cols, drop = FALSE])

            cl <- clusters()
            centroids <- km$centroids

            dists <- sapply(seq_along(cl), function(i) {
              k <- cl[i]
              var_data <- X_numeric[, i]
              sqrt(sum((var_data - mean(var_data))^2))
            })

            imp <- 1 / (1 + dists / max(dists))
            names(imp) <- names(cl)
            imp <- sort(imp, decreasing = TRUE)

            par(mar = c(5, 10, 4, 2))
            barplot(imp, horiz = TRUE, las = 1,
                    col = rainbow(max(cl))[cl[names(imp)]],
                    main = "Importance des variables",
                    xlab = "Score d'importance")
          }
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Erreur:", e$message))
        })

        dev.off()
      }
    )
  })
}
