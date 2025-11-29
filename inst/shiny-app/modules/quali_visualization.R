# =============================================================================
# QUALI VISUALIZATION MODULE - COMPLET
# Module de visualisation pour ClustQualiVarclus (ClustQualiVar)
# =============================================================================

#' @title Quali Visualization UI
#' @description Interface utilisateur pour la visualisation du clustering qualitatif
quali_visualization_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # En-tête avec infos
    fluidRow(
      valueBoxOutput(ns("n_clusters_box"), width = 3),
      valueBoxOutput(ns("n_variables_box"), width = 3),
      valueBoxOutput(ns("avg_eta2_box"), width = 3),
      valueBoxOutput(ns("convergence_box"), width = 3)
    ),

    # Options de visualisation
    fluidRow(
      box(
        width = 3,
        title = "Options de Visualisation",
        status = "primary",
        solidHeader = TRUE,

        selectInput(
          ns("plot_type"),
          "Type de Visualisation:",
          choices = c(
            "Matrice η² (Heatmap)" = "eta2_heatmap",
            "Tailles des Clusters" = "cluster_sizes",
            "Inertie par Cluster" = "inertia",
            "Sélection de k" = "k_selection",
            "Biplot MCA Global" = "mca_global",
            "MCA 3D Interactif (modalités)" = "mca_3d",
            "MCA avec Ellipses (modalités)" = "mca_ellipses",
            "Distances Inter-Clusters" = "distances_heatmap",
            "MCA Variables 2D CLUSTERING" = "mca_variables_2d",
            "MCA Variables 3D CLUSTERING" = "mca_variables_3d",
            "Distances Entre Variables" = "distances_variables",
            "MCA par Cluster" = "mca_cluster",
            "Composition Détaillée" = "composition",
            "Comparaison Avant/Après Clustering" = "before_after"
          ),
          selected = "eta2_heatmap"
        ),

        hr(),

        # Options spécifiques selon le type de plot
        conditionalPanel(
          condition = "input.plot_type == 'eta2_heatmap'",
          ns = ns,
          checkboxInput(ns("show_values"), "Afficher les valeurs", TRUE),
          sliderInput(ns("text_size"), "Taille du texte:", 8, 16, 11, step = 1)
        ),

        conditionalPanel(
          condition = "input.plot_type == 'mca_cluster'",
          ns = ns,
          uiOutput(ns("cluster_selector"))
        ),

        hr(),

        h5("Exporter"),
        downloadButton(ns("download_plot"), "Télécharger le Plot", class = "btn-sm"),
        downloadButton(ns("download_all"), "Télécharger Tout (PDF)", class = "btn-sm")
      ),

      # Zone de visualisation principale
      box(
        width = 9,
        title = textOutput(ns("plot_title")),
        status = "info",
        solidHeader = TRUE,

        conditionalPanel(
          condition = "input.plot_type == 'mca_global' || input.plot_type == 'before_after' || input.plot_type == 'mca_3d' || input.plot_type == 'mca_ellipses' || input.plot_type == 'distances_heatmap' || input.plot_type == 'mca_variables_2d' || input.plot_type == 'mca_variables_3d' || input.plot_type == 'distances_variables'",
          ns = ns,
          plotly::plotlyOutput(ns("main_plot_interactive"), height = "600px")
        ),

        conditionalPanel(
          condition = "input.plot_type != 'mca_global' && input.plot_type != 'before_after' && input.plot_type != 'mca_3d' && input.plot_type != 'mca_ellipses' && input.plot_type != 'distances_heatmap' && input.plot_type != 'mca_variables_2d' && input.plot_type != 'mca_variables_3d' && input.plot_type != 'distances_variables'",
          ns = ns,
          plotOutput(ns("main_plot_static"), height = "600px")
        )
      )
    ),

    # Résumé textuel
    fluidRow(
      box(
        width = 12,
        title = "Résumé du Clustering",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,

        verbatimTextOutput(ns("model_summary"))
      )
    ),

    # Tableau des assignments
    fluidRow(
      box(
        width = 6,
        title = "Affectations des Variables",
        status = "warning",
        solidHeader = TRUE,

        DTOutput(ns("assignments_table"))
      ),

      box(
        width = 6,
        title = "Matrice η² (Valeurs)",
        status = "warning",
        solidHeader = TRUE,

        DTOutput(ns("eta2_table"))
      )
    )
  )
}

#' @title Quali Visualization Server
#' @description Logique serveur pour la visualisation du clustering qualitatif
quali_visualization_server <- function(id, model, data, clusters) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Reactive values pour projections illustratives
    rv <- reactiveValues(
      illu_proj = NULL
    )

    # =========================================================================
    # HELPER FUNCTIONS
    # =========================================================================

    # ⭐ NOUVEAU : Calculer coordonnées des VARIABLES (pas modalités)
    compute_variable_coordinates <- function(mca_result, data, n_dims = 2) {

      # Coordonnées des modalités
      modal_coords <- as.data.frame(mca_result$var$coord[, 1:n_dims, drop = FALSE])

      # Préparer résultat
      var_names <- colnames(data)
      var_coords <- matrix(NA, nrow = length(var_names), ncol = n_dims)
      rownames(var_coords) <- var_names
      colnames(var_coords) <- paste0("Dim.", 1:n_dims)

      # Pour chaque variable
      for (v in var_names) {

        # Modalités de cette variable
        modalities <- levels(data[[v]])

        # Fréquences des modalités
        freqs <- table(data[[v]]) / nrow(data)
        freqs <- as.numeric(freqs[modalities])

        # Coordonnées des modalités
        modal_positions <- matrix(NA, nrow = length(modalities), ncol = n_dims)

        for (i in seq_along(modalities)) {
          # Chercher la modalité dans les rownames
          pattern <- modalities[i]
          idx <- grep(pattern, rownames(modal_coords), fixed = TRUE)

          if (length(idx) > 0) {
            modal_positions[i, ] <- as.numeric(modal_coords[idx[1], 1:n_dims])
          }
        }

        # Barycentre pondéré
        if (any(!is.na(modal_positions))) {
          var_coords[v, ] <- colSums(modal_positions * freqs, na.rm = TRUE)
        }
      }

      return(as.data.frame(var_coords))
    }

    # Vérifier que c'est un modèle ClustQualiVarclus
    is_varclus_quali <- reactive({
      req(model())

      cat("\n--- is_varclus_quali() appelé ---\n")
      cat("Classe du modèle:", paste(class(model()), collapse = ", "), "\n")

      # Vérifier si c'est un ClustQualiVarclus
      is_quali <- inherits(model(), "ClustQualiVarclus")

      cat("✓ Est ClustQualiVarclus:", is_quali, "\n")

      return(is_quali)
    })

    # Récupérer le modèle ClustQualiVarclus
    get_quali_model <- reactive({
      req(is_varclus_quali())

      cat("\n--- get_quali_model() appelé ---\n")

      m <- model()

      if (!m$fitted) {
        cat("❌ Modèle non fitted\n")
        return(NULL)
      }

      cat("✓ Modèle fitted récupéré\n")
      cat("  n_clusters:", m$n_clusters, "\n")
      cat("  n_variables:", length(m$clusters), "\n")

      return(m)
    })

    # =========================================================================
    # VALUE BOXES
    # =========================================================================

    output$n_clusters_box <- renderValueBox({
      req(is_varclus_quali())
      m <- get_quali_model()
      req(m)

      valueBox(
        m$n_clusters,
        "Clusters",
        icon = icon("layer-group"),
        color = "blue"
      )
    })

    output$n_variables_box <- renderValueBox({
      req(is_varclus_quali())
      m <- get_quali_model()
      req(m)

      valueBox(
        length(m$clusters),
        "Variables Qualitatives",
        icon = icon("tags"),
        color = "green"
      )
    })

    output$avg_eta2_box <- renderValueBox({
      req(is_varclus_quali())
      m <- get_quali_model()
      req(m)

      # η² moyen des variables dans leur cluster assigné
      avg_eta2 <- mean(sapply(1:length(m$clusters), function(i) {
        var_name <- names(m$clusters)[i]
        cluster_id <- m$clusters[i]
        m$eta2_matrix[var_name, cluster_id]
      }), na.rm = TRUE)

      valueBox(
        round(avg_eta2, 3),
        "η² Moyen",
        icon = icon("chart-bar"),
        color = "orange"
      )
    })

    output$convergence_box <- renderValueBox({
      req(is_varclus_quali())
      m <- get_quali_model()
      req(m)

      valueBox(
        "Convergé",
        "Statut",
        icon = icon("check-circle"),
        color = "green"
      )
    })

    # =========================================================================
    # TITRE DU PLOT
    # =========================================================================

    output$plot_title <- renderText({
      switch(input$plot_type,
             "eta2_heatmap" = "Matrice η² : Variables × Clusters",
             "cluster_sizes" = "Tailles des Clusters",
             "inertia" = "Inertie Expliquée par l'Axe 1 de chaque Cluster",
             "k_selection" = "Sélection du Nombre Optimal de Clusters",
             "mca_global" = "Biplot MCA Global (Toutes les Variables)",
             "mca_cluster" = "MCA du Cluster Sélectionné",
             "composition" = "Composition Détaillée des Clusters",
             "before_after" = "Comparaison Avant/Après Clustering",
             "Visualisation"
      )
    })

    # =========================================================================
    # SÉLECTEUR DE CLUSTER (pour MCA par cluster)
    # =========================================================================

    output$cluster_selector <- renderUI({
      req(is_varclus_quali())
      m <- get_quali_model()
      req(m)

      selectInput(
        ns("selected_cluster"),
        "Sélectionner un Cluster:",
        choices = setNames(1:m$n_clusters, paste("Cluster", 1:m$n_clusters)),
        selected = 1
      )
    })

    # =========================================================================
    # SÉLECTEUR VARIABLES ILLUSTRATIVES
    # =========================================================================

    output$illustrative_vars_selector <- renderUI({
      req(data())

      all_vars <- colnames(data())
      active_vars <- names(clusters())

      # Variables disponibles = toutes sauf les actives
      available_vars <- setdiff(all_vars, active_vars)

      if (length(available_vars) == 0) {
        return(p("Aucune variable illustrative disponible."))
      }

      checkboxGroupInput(
        ns("illustrative_vars"),
        "Variables Illustratives:",
        choices = available_vars,
        selected = NULL
      )
    })

    # =========================================================================
    # PROJECTION ILLUSTRATIVES
    # =========================================================================

    observeEvent(input$project_illu, {

      cat("\n========================================\n")
      cat("PROJECTION ILLUSTRATIVES - QUALI - DEBUG\n")

      req(is_varclus_quali(), data())

      illu_vars <- input$illustrative_vars

      if (is.null(illu_vars) || length(illu_vars) == 0) {
        showNotification("Aucune variable illustrative sélectionnée", type = "warning")
        return()
      }

      cat("Variables sélectionnées:", paste(illu_vars, collapse = ", "), "\n")

      m <- get_quali_model()
      req(m)

      X <- data()

      # Traiter chaque variable illustrative
      illu_list <- list()

      for (var_name in illu_vars) {
        cat("\n--- Traitement:", var_name, "---\n")

        var_data <- X[[var_name]]

        cat("Type:", class(var_data), "\n")
        cat("Length:", length(var_data), "\n")
        cat("NA count:", sum(is.na(var_data)), "\n")

        if (is.factor(var_data) || is.character(var_data)) {

          cat("→ Variable CATÉGORIELLE\n")

          if (is.character(var_data)) {
            var_data <- as.factor(var_data)
          }

          levs <- levels(var_data)
          cat("  Niveaux:", paste(levs, collapse = ", "), "\n")

          # Limiter à 15 niveaux
          if (length(levs) > 15) {
            cat("  ⚠️ Trop de niveaux (", length(levs), "), limite à 15\n")
            levs <- levs[1:15]
          }

          illu_list[[var_name]] <- list(
            type = "factor",
            levels = levs
          )

          cat("  → Variable catégorielle enregistrée avec", length(levs), "niveaux\n")

        } else if (is.numeric(var_data)) {

          cat("→ Variable NUMÉRIQUE\n")

          illu_list[[var_name]] <- list(
            type = "numeric"
          )

          cat("  → Variable numérique enregistrée\n")
        } else {
          cat("⚠️ Type non supporté, ignorée\n")
        }
      }

      cat("\n========================================\n")
      cat("RÉSUMÉ:\n")
      cat("Nombre de variables traitées:", length(illu_list), "\n")
      cat("Noms:", paste(names(illu_list), collapse = ", "), "\n")
      cat("========================================\n\n")

      rv$illu_proj <- illu_list

      showNotification(
        paste(length(illu_list), "variable(s) illustrative(s) projetée(s)"),
        type = "message"
      )
    })

    # =========================================================================
    # PLOTS PRINCIPAUX
    # =========================================================================

    # -------------------------------------------------------------------------
    # Plot Statique (tous sauf MCA global et illustrative)
    # -------------------------------------------------------------------------

    output$main_plot_static <- renderPlot({

      cat("\n========================================\n")

      req(is_varclus_quali())

      m <- get_quali_model()
      req(m)


      tryCatch({

        if (input$plot_type == "eta2_heatmap") {

          cat("→ Génération HEATMAP η²\n")

          eta2_mat <- m$eta2_matrix

          cat("  Dimensions η²:", nrow(eta2_mat), "x", ncol(eta2_mat), "\n")

          # Convertir en format long
          df_long <- as.data.frame(eta2_mat) %>%
            tibble::rownames_to_column("Variable") %>%
            tidyr::pivot_longer(
              cols = starts_with("C"),
              names_to = "Cluster",
              values_to = "eta2"
            )

          # Ajouter les affectations
          df_long$Assigned <- sapply(1:nrow(df_long), function(i) {
            var <- df_long$Variable[i]
            clust <- as.integer(sub("C", "", df_long$Cluster[i]))
            m$clusters[var] == clust
          })

          cat("  DataFrame long:", nrow(df_long), "lignes\n")

          # Plot
          p <- ggplot(df_long, aes(x = Cluster, y = Variable, fill = eta2)) +
            geom_tile(color = "white", linewidth = 0.5) +
            scale_fill_gradient2(
              low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
              midpoint = 0.5, limits = c(0, 1),
              name = "η²"
            ) +
            geom_tile(
              data = df_long %>% filter(Assigned),
              aes(x = Cluster, y = Variable),
              color = "#00FF00", fill = NA, linewidth = 2
            ) +
            labs(
              title = "Matrice η² : Association Variable × Cluster",
              subtitle = "Cadre vert = affectation de la variable",
              x = "Cluster", y = "Variable"
            ) +
            theme_minimal(base_size = 13) +
            theme(
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              axis.text.y = element_text(size = 10),
              panel.grid = element_blank()
            )

          # Ajouter valeurs si demandé
          if (input$show_values) {
            p <- p + geom_text(
              aes(label = sprintf("%.2f", eta2)),
              size = input$text_size / 3,
              color = ifelse(df_long$eta2 > 0.5, "white", "black")
            )
          }

          cat("✓ Heatmap η² créée\n")
          print(p)

        } else if (input$plot_type == "cluster_sizes") {

          cat("→ Génération CLUSTER SIZES\n")

          sizes <- table(m$clusters)

          df_sizes <- data.frame(
            Cluster = factor(paste("Cluster", names(sizes)),
                             levels = paste("Cluster", 1:m$n_clusters)),
            Size = as.numeric(sizes)
          )

          cat("  Tailles:", paste(df_sizes$Size, collapse = ", "), "\n")

          p <- ggplot(df_sizes, aes(x = Cluster, y = Size, fill = Cluster)) +
            geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
            geom_text(aes(label = Size), vjust = -0.5, size = 6, fontface = "bold") +
            scale_fill_brewer(palette = "Set3") +
            labs(
              title = "Tailles des Clusters",
              subtitle = paste(m$n_clusters, "clusters,", length(m$clusters), "variables"),
              x = "", y = "Nombre de Variables"
            ) +
            theme_minimal(base_size = 14) +
            theme(
              legend.position = "none",
              axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold")
            ) +
            ylim(0, max(df_sizes$Size) * 1.15)

          cat("✓ Barplot tailles créé\n")
          print(p)

        } else if (input$plot_type == "inertia") {

          cat("→ Génération INERTIA\n")

          # Calculer l'inertie expliquée par l'axe 1 de chaque cluster
          inertias <- sapply(1:m$n_clusters, function(j) {
            mca_j <- m$cluster_mca[[j]]
            if (is.null(mca_j)) return(NA)

            # Inertie = valeur propre / somme valeurs propres
            eig <- mca_j$eig[1, 2] # % variance
            return(eig)
          })

          df_inertia <- data.frame(
            Cluster = factor(paste("Cluster", 1:m$n_clusters)),
            Inertia = inertias
          )

          cat("  Inerties:", paste(round(df_inertia$Inertia, 1), collapse = ", "), "\n")

          p <- ggplot(df_inertia, aes(x = Cluster, y = Inertia, fill = Cluster)) +
            geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
            geom_text(aes(label = sprintf("%.1f%%", Inertia)),
                      vjust = -0.5, size = 5, fontface = "bold") +
            scale_fill_brewer(palette = "Pastel1") +
            labs(
              title = "Inertie Expliquée par l'Axe 1 de chaque Cluster",
              subtitle = "Plus l'inertie est élevée, mieux l'axe 1 résume le cluster",
              x = "", y = "Inertie (%)"
            ) +
            theme_minimal(base_size = 14) +
            theme(
              legend.position = "none",
              axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold")
            ) +
            ylim(0, max(df_inertia$Inertia, na.rm = TRUE) * 1.15)

          cat("✓ Barplot inertie créé\n")
          print(p)

        } else if (input$plot_type == "k_selection") {

          cat("→ Génération K SELECTION\n")

          if (is.null(m$scores_k_selection)) {
            plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, "Sélection automatique de k non effectuée", cex = 1.5)
            return()
          }

          df_scores <- m$scores_k_selection
          k_opt <- m$n_clusters

          cat("  k optimal:", k_opt, "\n")
          cat("  Plage k:", min(df_scores$k), "-", max(df_scores$k), "\n")

          p <- ggplot(df_scores, aes(x = k, y = score)) +
            geom_line(color = "#1f77b4", linewidth = 1.5) +
            geom_point(size = 3, color = "#1f77b4") +
            geom_point(
              data = df_scores %>% filter(k == k_opt),
              size = 6, color = "#FF0000", shape = 18
            ) +
            geom_vline(xintercept = k_opt, linetype = "dashed",
                       color = "#FF0000", linewidth = 1) +
            annotate(
              "text", x = k_opt, y = max(df_scores$score) * 0.95,
              label = paste("k optimal =", k_opt),
              color = "#FF0000", fontface = "bold", size = 5, hjust = -0.1
            ) +
            labs(
              title = "Sélection du Nombre Optimal de Clusters",
              subtitle = "Score = moyenne des η² max par variable",
              x = "Nombre de Clusters (k)", y = "Score"
            ) +
            theme_minimal(base_size = 14) +
            theme(
              axis.text = element_text(size = 12),
              axis.title = element_text(face = "bold")
            ) +
            scale_x_continuous(breaks = unique(df_scores$k))

          cat("✓ Plot sélection k créé\n")
          print(p)

        } else if (input$plot_type == "composition") {

          cat("→ Génération COMPOSITION\n")

          # Créer un dataframe avec toutes les variables et leurs clusters
          df_comp <- data.frame(
            Variable = names(m$clusters),
            Cluster = paste("Cluster", m$clusters),
            stringsAsFactors = FALSE
          )

          # Ajouter l'η² max
          df_comp$eta2_max <- sapply(1:nrow(df_comp), function(i) {
            var <- df_comp$Variable[i]
            clust <- m$clusters[var]
            m$eta2_matrix[var, clust]
          })

          # Trier par cluster puis η²
          df_comp <- df_comp %>%
            arrange(Cluster, desc(eta2_max))

          cat("  Variables totales:", nrow(df_comp), "\n")

          # Plot
          p <- ggplot(df_comp, aes(x = reorder(Variable, -eta2_max),
                                   y = eta2_max, fill = Cluster)) +
            geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
            scale_fill_brewer(palette = "Set3") +
            labs(
              title = "Composition Détaillée : Variables et leur η²",
              subtitle = "Chaque barre = une variable, hauteur = η² avec son cluster",
              x = "", y = "η²"
            ) +
            theme_minimal(base_size = 12) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
              legend.position = "right"
            ) +
            facet_wrap(~ Cluster, scales = "free_x", ncol = 2)

          cat("✓ Plot composition créé\n")
          print(p)

        } else if (input$plot_type == "mca_cluster") {

          cat("→ Génération MCA CLUSTER\n")

          req(input$selected_cluster)
          cl_id <- as.integer(input$selected_cluster)

          cat("  Cluster sélectionné:", cl_id, "\n")

          # Récupérer le MCA du cluster
          mca_cl <- m$cluster_mca[[cl_id]]

          if (is.null(mca_cl)) {
            plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, paste("MCA du cluster", cl_id, "non disponible"), cex = 1.5)
            return()
          }

          # Variables du cluster
          vars_cl <- names(m$clusters)[m$clusters == cl_id]
          cat("  Variables:", length(vars_cl), "\n")

          # Plot MCA (modalités)
          if (requireNamespace("factoextra", quietly = TRUE)) {
            p <- factoextra::fviz_mca_var(
              mca_cl,
              axes = c(1, 2),
              col.var = "#E74C3C",
              repel = TRUE,
              labelsize = 4,
              title = paste("MCA - Cluster", cl_id, ":", length(vars_cl), "variables")
            )
            print(p)
          } else {
            # Fallback FactoMineR
            plot(mca_cl, choix = "var", axes = c(1, 2),
                 title = paste("MCA - Cluster", cl_id))
          }

          cat("✓ MCA cluster créé\n")

        } else {
        }

      }, error = function(e) {
        cat("❌ ERREUR dans main_plot_static:", e$message, "\n")
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Erreur:", e$message), cex = 1.2, col = "red")
      })

      cat("========================================\n\n")
    })

    # -------------------------------------------------------------------------
    # Plot Interactif (MCA global et illustrative)
    # -------------------------------------------------------------------------

    output$main_plot_interactive <- plotly::renderPlotly({

      cat("\n========================================\n")

      req(is_varclus_quali())

      m <- get_quali_model()
      req(m)


      if (input$plot_type == "mca_global") {

        cat("→ Génération MCA GLOBAL (biplot)\n")

        # MCA sur toutes les variables actives
        X <- data()
        active_vars <- names(m$clusters)
        X_active <- X[, active_vars, drop = FALSE]

        cat("  Variables actives:", length(active_vars), "\n")

        # Assurer que toutes sont des facteurs
        X_active <- X_active %>%
          mutate(across(everything(), as.factor))

        # MCA global
        mca_global <- FactoMineR::MCA(X_active, graph = FALSE, ncp = 5)

        cat("  MCA calculée, dimensions:", nrow(mca_global$var$coord), "x", ncol(mca_global$var$coord), "\n")

        # Coordonnées des modalités
        coords_var <- as.data.frame(mca_global$var$coord[, 1:2])

        # ⭐ CORRECTION CRITIQUE : Renommer les colonnes (FactoMineR utilise "Dim 1" avec espace)
        colnames(coords_var) <- c("Dim.1", "Dim.2")

        coords_var$Modalite <- rownames(coords_var)

        cat("  DEBUG: coords_var après création - colonnes:", paste(colnames(coords_var), collapse=", "), "\n")
        cat("  DEBUG: coords_var - dimensions:", nrow(coords_var), "x", ncol(coords_var), "\n")

        # CORRECTION : Trouver la variable source pour chaque modalité
        get_var_for_modality <- function(modality, data) {
          for (col_name in colnames(data)) {
            if (is.factor(data[[col_name]]) && modality %in% levels(data[[col_name]])) {
              return(col_name)
            }
          }
          return(NA_character_)
        }

        coords_var$Variable <- sapply(coords_var$Modalite, function(m) {
          get_var_for_modality(m, X_active)
        })

        cat("  DEBUG: Variables trouvées (NA count):", sum(is.na(coords_var$Variable)), "/", nrow(coords_var), "\n")

        # Ajouter le cluster
        coords_var$Cluster <- factor(m$clusters[coords_var$Variable])

        cat("  DEBUG: Clusters ajoutés (NA count):", sum(is.na(coords_var$Cluster)), "/", nrow(coords_var), "\n")
        cat("  DEBUG: coords_var avant filtrage - colonnes:", paste(colnames(coords_var), collapse=", "), "\n")

        # Filtrer les modalités sans cluster (NA) - GARDER LES COLONNES
        coords_var <- coords_var[!is.na(coords_var$Cluster), , drop = FALSE]

        cat("  DEBUG: coords_var après filtrage - colonnes:", paste(colnames(coords_var), collapse=", "), "\n")
        cat("  DEBUG: coords_var après filtrage - dimensions:", nrow(coords_var), "x", ncol(coords_var), "\n")

        # Vérifier qu'il reste des données
        if (nrow(coords_var) == 0) {
          cat("❌ ERREUR: Aucune modalité n'a pu être mappée\n")
          return(
            plotly::plot_ly() %>%
              plotly::add_annotations(
                text = "Erreur : Impossible de mapper les modalités aux variables.\nVérifiez le format de vos données.",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
                showarrow = FALSE,
                font = list(size = 16, color = "red")
              )
          )
        }

        cat("  DEBUG: Dim.1 - range:", paste(range(coords_var$Dim.1, na.rm=TRUE), collapse=" à "), "\n")
        cat("  DEBUG: Dim.2 - range:", paste(range(coords_var$Dim.2, na.rm=TRUE), collapse=" à "), "\n")
        cat("  Modalités valides:", nrow(coords_var), "\n")

        # Variance expliquée
        variance_explained <- mca_global$eig[1:2, 2]

        cat("  Variance: Dim1 =", round(variance_explained[1], 1), "%, Dim2 =", round(variance_explained[2], 1), "%\n")

        # Plot plotly
        p <- plotly::plot_ly(
          data = coords_var,
          x = ~Dim.1,
          y = ~Dim.2,
          text = ~Modalite,
          color = ~Cluster,
          colors = rainbow(m$n_clusters),
          type = "scatter",
          mode = "markers+text",
          marker = list(
            size = 10,
            line = list(color = "white", width = 1)
          ),
          textposition = "top center",
          textfont = list(size = 9),
          hoverinfo = "text",
          hovertext = ~paste0(
            "Modalité: ", Modalite, "<br>",
            "Variable: ", Variable, "<br>",
            "Cluster: ", Cluster, "<br>",
            "Dim1: ", round(Dim.1, 3), "<br>",
            "Dim2: ", round(Dim.2, 3)
          )
        ) %>%
          plotly::layout(
            title = list(
              text = sprintf(
                "MCA Global - Biplot des Modalités<br><sub>Dim1: %.1f%% | Dim2: %.1f%% | Total: %.1f%%</sub>",
                variance_explained[1], variance_explained[2], sum(variance_explained[1:2])
              ),
              font = list(size = 16)
            ),
            xaxis = list(
              title = sprintf("Dimension 1 (%.1f%%)", variance_explained[1]),
              zeroline = TRUE,
              showgrid = TRUE
            ),
            yaxis = list(
              title = sprintf("Dimension 2 (%.1f%%)", variance_explained[2]),
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
                x0 = min(coords_var$Dim.1) - 0.1,
                x1 = max(coords_var$Dim.1) + 0.1,
                y0 = 0,
                y1 = 0,
                line = list(color = "gray", width = 1, dash = "dash")
              ),
              # Ligne verticale
              list(
                type = "line",
                x0 = 0,
                x1 = 0,
                y0 = min(coords_var$Dim.2) - 0.1,
                y1 = max(coords_var$Dim.2) + 0.1,
                line = list(color = "gray", width = 1, dash = "dash")
              )
            )
          )

        cat("✓ MCA global plotly créé\n")

        return(p)

      } else if (input$plot_type == "before_after") {

        cat("\n→ Génération COMPARAISON AVANT/APRÈS\n\n")

        # MCA des variables actives
        X <- data()
        active_vars <- names(m$clusters)
        X_active <- X[, active_vars, drop = FALSE]

        X_active <- X_active %>%
          mutate(across(everything(), as.factor))

        mca_global <- FactoMineR::MCA(X_active, graph = FALSE, ncp = 5)

        # Coordonnées des modalités
        coords_var <- as.data.frame(mca_global$var$coord[, 1:2])

        # ⭐ CORRECTION CRITIQUE : Renommer les colonnes (FactoMineR utilise "Dim 1" avec espace)
        colnames(coords_var) <- c("Dim.1", "Dim.2")

        coords_var$Modalite <- rownames(coords_var)

        # CORRECTION : Trouver la variable source pour chaque modalité
        get_var_for_modality <- function(modality, data) {
          for (col_name in colnames(data)) {
            if (is.factor(data[[col_name]]) && modality %in% levels(data[[col_name]])) {
              return(col_name)
            }
          }
          return(NA_character_)
        }

        coords_var$Variable <- sapply(coords_var$Modalite, function(m) {
          get_var_for_modality(m, X_active)
        })

        # Ajouter le cluster
        coords_var$Cluster <- factor(m$clusters[coords_var$Variable])

        cat("  DEBUG BA: coords_var avant filtrage - colonnes:", paste(colnames(coords_var), collapse=", "), "\n")

        # Filtrer les modalités sans cluster (NA) - GARDER LES COLONNES
        coords_var <- coords_var[!is.na(coords_var$Cluster), , drop = FALSE]

        cat("  DEBUG BA: coords_var après filtrage - colonnes:", paste(colnames(coords_var), collapse=", "), "\n")
        cat("  DEBUG BA: Dim.1 existe?", "Dim.1" %in% colnames(coords_var), "| Dim.2 existe?", "Dim.2" %in% colnames(coords_var), "\n")

        # Vérifier qu'il reste des données
        if (nrow(coords_var) == 0) {
          cat("❌ ERREUR: Aucune modalité n'a pu être mappée pour before_after\n")
          return(
            plotly::plot_ly() %>%
              plotly::add_annotations(
                text = "Erreur : Impossible de mapper les modalités aux variables.\nVérifiez le format de vos données.",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
                showarrow = FALSE,
                font = list(size = 16, color = "red")
              )
          )
        }

        cat("  Modalités valides:", nrow(coords_var), "\n")

        # Variance expliquée
        variance_explained <- mca_global$eig[1:2, 2]

        # SUBPLOT 1: AVANT (toutes les modalités en gris)
        p1 <- plotly::plot_ly(
          data = coords_var,
          x = ~Dim.1,
          y = ~Dim.2,
          text = ~Modalite,
          type = "scatter",
          mode = "markers+text",
          marker = list(
            size = 8,
            color = "gray",
            line = list(color = "darkgray", width = 1)
          ),
          textposition = "top center",
          textfont = list(size = 8, color = "gray"),
          hoverinfo = "text",
          hovertext = ~paste0(
            "Modalité: ", Modalite, "<br>",
            "Variable: ", Variable, "<br>",
            "Dim1: ", round(Dim.1, 3), "<br>",
            "Dim2: ", round(Dim.2, 3)
          ),
          showlegend = FALSE
        ) %>%
          plotly::layout(
            title = list(
              text = "<b>AVANT</b> Clustering<br><sub>Toutes les modalités sans distinction</sub>",
              font = list(size = 14)
            ),
            xaxis = list(
              title = sprintf("Dim 1 (%.1f%%)", variance_explained[1]),
              zeroline = TRUE,
              showgrid = TRUE
            ),
            yaxis = list(
              title = sprintf("Dim 2 (%.1f%%)", variance_explained[2]),
              zeroline = TRUE,
              showgrid = TRUE
            ),
            hovermode = "closest",
            shapes = list(
              list(
                type = "line",
                x0 = min(coords_var$Dim.1) - 0.1,
                x1 = max(coords_var$Dim.1) + 0.1,
                y0 = 0, y1 = 0,
                line = list(color = "lightgray", width = 1, dash = "dash")
              ),
              list(
                type = "line",
                x0 = 0, x1 = 0,
                y0 = min(coords_var$Dim.2) - 0.1,
                y1 = max(coords_var$Dim.2) + 0.1,
                line = list(color = "lightgray", width = 1, dash = "dash")
              )
            )
          )

        # SUBPLOT 2: APRÈS (modalités colorées par cluster)
        p2 <- plotly::plot_ly(
          data = coords_var,
          x = ~Dim.1,
          y = ~Dim.2,
          text = ~Modalite,
          color = ~Cluster,
          colors = rainbow(m$n_clusters),
          type = "scatter",
          mode = "markers+text",
          marker = list(
            size = 10,
            line = list(color = "white", width = 1)
          ),
          textposition = "top center",
          textfont = list(size = 9),
          hoverinfo = "text",
          hovertext = ~paste0(
            "Modalité: ", Modalite, "<br>",
            "Variable: ", Variable, "<br>",
            "Cluster: ", Cluster, "<br>",
            "Dim1: ", round(Dim.1, 3), "<br>",
            "Dim2: ", round(Dim.2, 3)
          )
        ) %>%
          plotly::layout(
            title = list(
              text = "<b>APRÈS</b> Clustering<br><sub>Modalités colorées par cluster</sub>",
              font = list(size = 14)
            ),
            xaxis = list(
              title = sprintf("Dim 1 (%.1f%%)", variance_explained[1]),
              zeroline = TRUE,
              showgrid = TRUE
            ),
            yaxis = list(
              title = sprintf("Dim 2 (%.1f%%)", variance_explained[2]),
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
              list(
                type = "line",
                x0 = min(coords_var$Dim.1) - 0.1,
                x1 = max(coords_var$Dim.1) + 0.1,
                y0 = 0, y1 = 0,
                line = list(color = "gray", width = 1, dash = "dash")
              ),
              list(
                type = "line",
                x0 = 0, x1 = 0,
                y0 = min(coords_var$Dim.2) - 0.1,
                y1 = max(coords_var$Dim.2) + 0.1,
                line = list(color = "gray", width = 1, dash = "dash")
              )
            )
          )

        # Combiner en subplot côte à côte
        p_combined <- plotly::subplot(
          p1, p2,
          nrows = 1,
          shareX = TRUE,
          shareY = TRUE,
          titleX = TRUE,
          titleY = TRUE,
          margin = 0.05
        ) %>%
          plotly::layout(
            title = list(
              text = sprintf(
                "<b>Comparaison Avant/Après Clustering</b><br><sub>%d variables • %d clusters • MCA Dim1+Dim2: %.1f%%</sub>",
                length(active_vars), m$n_clusters, sum(variance_explained[1:2])
              ),
              font = list(size = 18, family = "Arial")
            ),
            showlegend = TRUE,
            margin = list(t = 100)
          )

        cat("✓ Comparaison avant/après créée\n\n")

        return(p_combined)
      }

      # ═══════════════════════════════════════════════════════════════
      # 🌟 PLOT : MCA 3D INTERACTIF
      # ═══════════════════════════════════════════════════════════════
      if (input$plot_type == "mca_3d") {

        cat("\n→ Génération MCA 3D INTERACTIF\n")

        X_active <- data()[, names(m$clusters), drop = FALSE]

        mca_global <- FactoMineR::MCA(X_active, graph = FALSE, ncp = 5)

        coords_3d <- as.data.frame(mca_global$var$coord[, 1:3])
        colnames(coords_3d) <- c("Dim.1", "Dim.2", "Dim.3")
        coords_3d$Modalite <- rownames(coords_3d)

        get_var_for_modality <- function(modality, data) {
          for (col_name in colnames(data)) {
            if (is.factor(data[[col_name]]) && modality %in% levels(data[[col_name]])) {
              return(col_name)
            }
          }
          return(NA_character_)
        }

        coords_3d$Variable <- sapply(coords_3d$Modalite, function(modal) {
          get_var_for_modality(modal, X_active)
        })

        coords_3d$Cluster <- factor(m$clusters[coords_3d$Variable])
        coords_3d <- coords_3d[!is.na(coords_3d$Cluster), , drop = FALSE]

        if (nrow(coords_3d) == 0) {
          cat("❌ ERREUR: Aucune modalité pour MCA 3D\n")
          return(
            plotly::plot_ly() %>%
              plotly::add_annotations(
                text = "Erreur : Aucune donnée disponible",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
                showarrow = FALSE,
                font = list(size = 16, color = "red")
              )
          )
        }

        variance_3d <- mca_global$eig[1:3, 2]

        cat("  Modalités : ", nrow(coords_3d), "\n", sep = "")
        cat("  Variance : Dim1=", round(variance_3d[1], 1), "%, ",
            "Dim2=", round(variance_3d[2], 1), "%, ",
            "Dim3=", round(variance_3d[3], 1), "%\n", sep = "")

        couleurs_3d <- rainbow(m$n_clusters)

        p <- plotly::plot_ly(
          data = coords_3d,
          x = ~Dim.1,
          y = ~Dim.2,
          z = ~Dim.3,
          color = ~Cluster,
          colors = couleurs_3d,
          text = ~paste0(
            "<b>", Modalite, "</b><br>",
            "Variable: ", Variable, "<br>",
            "Cluster: ", Cluster, "<br>",
            "Dim1: ", round(Dim.1, 2), "<br>",
            "Dim2: ", round(Dim.2, 2), "<br>",
            "Dim3: ", round(Dim.3, 2)
          ),
          type = "scatter3d",
          mode = "markers+text",
          marker = list(
            size = 8,
            line = list(color = "white", width = 1),
            opacity = 0.9
          ),
          textfont = list(size = 9),
          hoverinfo = "text"
        ) %>%
          plotly::layout(
            title = list(
              text = sprintf(
                "🌟 MCA 3D Interactif - Clustering de Variables<br><sub>Dim1: %.1f%% | Dim2: %.1f%% | Dim3: %.1f%% | Total: %.1f%%</sub>",
                variance_3d[1], variance_3d[2], variance_3d[3], sum(variance_3d[1:3])
              ),
              font = list(size = 16, family = "Arial, sans-serif")
            ),
            scene = list(
              xaxis = list(
                title = sprintf("Dim 1 (%.1f%%)", variance_3d[1]),
                gridcolor = "rgb(240, 240, 240)",
                showbackground = TRUE,
                backgroundcolor = "rgb(250, 250, 250)"
              ),
              yaxis = list(
                title = sprintf("Dim 2 (%.1f%%)", variance_3d[2]),
                gridcolor = "rgb(240, 240, 240)",
                showbackground = TRUE,
                backgroundcolor = "rgb(250, 250, 250)"
              ),
              zaxis = list(
                title = sprintf("Dim 3 (%.1f%%)", variance_3d[3]),
                gridcolor = "rgb(240, 240, 240)",
                showbackground = TRUE,
                backgroundcolor = "rgb(250, 250, 250)"
              ),
              camera = list(
                eye = list(x = 1.5, y = 1.5, z = 1.5)
              )
            ),
            showlegend = TRUE,
            legend = list(
              title = list(text = "Clusters"),
              orientation = "v",
              x = 1.02,
              y = 0.5
            )
          )

        cat("✓ MCA 3D créé - FAITES-LE TOURNER ! 🔄\n")
        return(p)
      }

      # ═══════════════════════════════════════════════════════════════
      # 🎯 PLOT : MCA AVEC ELLIPSES DE CONFIANCE
      # ═══════════════════════════════════════════════════════════════
      if (input$plot_type == "mca_ellipses") {

        cat("\n→ Génération MCA AVEC ELLIPSES\n")

        X_active <- data()[, names(m$clusters), drop = FALSE]

        mca_global <- FactoMineR::MCA(X_active, graph = FALSE, ncp = 5)

        coords_var <- as.data.frame(mca_global$var$coord[, 1:2])
        colnames(coords_var) <- c("Dim.1", "Dim.2")
        coords_var$Modalite <- rownames(coords_var)

        get_var_for_modality <- function(modality, data) {
          for (col_name in colnames(data)) {
            if (is.factor(data[[col_name]]) && modality %in% levels(data[[col_name]])) {
              return(col_name)
            }
          }
          return(NA_character_)
        }

        coords_var$Variable <- sapply(coords_var$Modalite, function(m) {
          get_var_for_modality(m, X_active)
        })

        coords_var$Cluster <- factor(m$clusters[coords_var$Variable])
        coords_var <- coords_var[!is.na(coords_var$Cluster), , drop = FALSE]

        if (nrow(coords_var) == 0) {
          cat("❌ ERREUR: Aucune modalité pour MCA ellipses\n")
          return(
            plotly::plot_ly() %>%
              plotly::add_annotations(
                text = "Erreur : Aucune donnée disponible",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
                showarrow = FALSE,
                font = list(size = 16, color = "red")
              )
          )
        }

        variance_explained <- mca_global$eig[1:2, 2]

        cat("  Modalités : ", nrow(coords_var), "\n", sep = "")

        # Vérifier ggplot2 et ggrepel
        if (!requireNamespace("ggplot2", quietly = TRUE) ||
            !requireNamespace("ggrepel", quietly = TRUE)) {

          cat("  ⚠️  Packages ggplot2/ggrepel manquants, fallback vers plotly simple\n")

          p <- plotly::plot_ly(
            data = coords_var,
            x = ~Dim.1,
            y = ~Dim.2,
            color = ~Cluster,
            colors = rainbow(m$n_clusters),
            text = ~Modalite,
            type = "scatter",
            mode = "markers+text",
            marker = list(size = 10),
            textposition = "top center"
          ) %>%
            plotly::layout(
              title = "MCA avec Ellipses (packages ggplot2/ggrepel requis pour ellipses)",
              xaxis = list(title = sprintf("Dim 1 (%.1f%%)", variance_explained[1])),
              yaxis = list(title = sprintf("Dim 2 (%.1f%%)", variance_explained[2]))
            )

          return(p)
        }

        # Plot avec ellipses (ggplot2)
        library(ggplot2)
        library(ggrepel)

        couleurs <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")
        couleurs <- couleurs[1:m$n_clusters]

        p_gg <- ggplot(coords_var, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
          geom_point(size = 4, alpha = 0.8) +
          stat_ellipse(level = 0.95, linewidth = 1.5, alpha = 0.3) +
          geom_text_repel(
            aes(label = Modalite),
            size = 3.5,
            max.overlaps = 50,
            box.padding = 0.5,
            point.padding = 0.3,
            segment.color = "gray50",
            segment.alpha = 0.6
          ) +
          scale_color_manual(values = couleurs, name = "Cluster") +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            legend.position = "right",
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA)
          ) +
          labs(
            title = "🎯 MCA Global avec Ellipses de Confiance (95%)",
            subtitle = sprintf(
              "Dim1: %.1f%% | Dim2: %.1f%% | Total: %.1f%%",
              variance_explained[1], variance_explained[2], sum(variance_explained)
            ),
            x = sprintf("Dimension 1 (%.1f%%)", variance_explained[1]),
            y = sprintf("Dimension 2 (%.1f%%)", variance_explained[2])
          )

        cat("✓ MCA ellipses créé\n")

        p <- plotly::ggplotly(p_gg, tooltip = "text")

        return(p)
      }

      # ═══════════════════════════════════════════════════════════════
      # 📊 PLOT : HEATMAP DISTANCES INTER-CLUSTERS
      # ═══════════════════════════════════════════════════════════════
      if (input$plot_type == "distances_heatmap") {

        cat("\n→ Génération HEATMAP DISTANCES\n")

        X_active <- data()[, names(m$clusters), drop = FALSE]

        mca_global <- FactoMineR::MCA(X_active, graph = FALSE, ncp = 5)

        coords_var <- as.data.frame(mca_global$var$coord[, 1:2])
        colnames(coords_var) <- c("Dim.1", "Dim.2")
        coords_var$Modalite <- rownames(coords_var)

        get_var_for_modality <- function(modality, data) {
          for (col_name in colnames(data)) {
            if (is.factor(data[[col_name]]) && modality %in% levels(data[[col_name]])) {
              return(col_name)
            }
          }
          return(NA_character_)
        }

        coords_var$Variable <- sapply(coords_var$Modalite, function(m) {
          get_var_for_modality(m, X_active)
        })

        coords_var$Cluster <- factor(m$clusters[coords_var$Variable])
        coords_var <- coords_var[!is.na(coords_var$Cluster), , drop = FALSE]

        if (nrow(coords_var) == 0) {
          cat("❌ ERREUR: Aucune modalité pour heatmap distances\n")
          return(
            plotly::plot_ly() %>%
              plotly::add_annotations(
                text = "Erreur : Aucune donnée disponible",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
                showarrow = FALSE,
                font = list(size = 16, color = "red")
              )
          )
        }

        cluster_centers <- aggregate(
          coords_var[, c("Dim.1", "Dim.2")],
          by = list(Cluster = coords_var$Cluster),
          FUN = mean
        )

        dist_matrix <- as.matrix(dist(cluster_centers[, -1]))
        rownames(dist_matrix) <- paste("Cluster", cluster_centers$Cluster)
        colnames(dist_matrix) <- paste("Cluster", cluster_centers$Cluster)

        cat("  Centres calculés pour", nrow(cluster_centers), "clusters\n")
        cat("  Distance minimale :", round(min(dist_matrix[dist_matrix > 0]), 3), "\n")
        cat("  Distance maximale :", round(max(dist_matrix), 3), "\n")

        p <- plotly::plot_ly(
          z = dist_matrix,
          x = colnames(dist_matrix),
          y = rownames(dist_matrix),
          type = "heatmap",
          colorscale = list(
            c(0, "white"),
            c(0.5, "orange"),
            c(1, "red")
          ),
          colorbar = list(title = "Distance"),
          hovertemplate = paste0(
            "<b>%{y}</b> → <b>%{x}</b><br>",
            "Distance: %{z:.3f}<br>",
            "<extra></extra>"
          )
        ) %>%
          plotly::layout(
            title = list(
              text = "📊 Heatmap des Distances Inter-Clusters<br><sub>Distances euclidiennes dans l'espace MCA</sub>",
              font = list(size = 16)
            ),
            xaxis = list(title = "", tickangle = 0),
            yaxis = list(title = ""),
            margin = list(l = 100, r = 50, t = 100, b = 50)
          )

        annotations <- list()
        for (i in 1:nrow(dist_matrix)) {
          for (j in 1:ncol(dist_matrix)) {
            annotations[[length(annotations) + 1]] <- list(
              x = colnames(dist_matrix)[j],
              y = rownames(dist_matrix)[i],
              text = sprintf("%.2f", dist_matrix[i, j]),
              xref = "x",
              yref = "y",
              showarrow = FALSE,
              font = list(
                color = if (dist_matrix[i, j] > max(dist_matrix) * 0.6) "white" else "black",
                size = 14,
                family = "Arial, sans-serif"
              )
            )
          }
        }

        p <- p %>% plotly::layout(annotations = annotations)

        cat("✓ Heatmap distances créée\n")
        return(p)
      }

      # ═══════════════════════════════════════════════════════════════
      # PLOT : MCA VARIABLES 2D (Clustering) ⭐ NOUVEAU
      # ═══════════════════════════════════════════════════════════════
      if (input$plot_type == "mca_variables_2d") {

        cat("\n→ Génération MCA VARIABLES 2D (Clustering)\n")

        # Variables actives
        X_active <- data()[, names(m$clusters), drop = FALSE]

        # MCA global
        mca_global <- FactoMineR::MCA(X_active, graph = FALSE, ncp = 5)

        # ⭐ CALCULER COORDONNÉES DES VARIABLES (pas modalités)
        var_coords <- compute_variable_coordinates(mca_global, X_active, n_dims = 2)

        # Ajouter les clusters
        var_coords$Variable <- rownames(var_coords)
        var_coords$Cluster <- factor(m$clusters[var_coords$Variable])

        # Variance expliquée
        variance_explained <- mca_global$eig[1:2, 2]

        cat("  Variables : ", nrow(var_coords), "\n", sep = "")
        cat("  Variance : Dim1=", round(variance_explained[1], 1), "%, ",
            "Dim2=", round(variance_explained[2], 1), "%\n", sep = "")

        # NÉCESSITE ggplot2 et ggrepel
        if (!requireNamespace("ggplot2", quietly = TRUE) ||
            !requireNamespace("ggrepel", quietly = TRUE)) {

          # Fallback : plotly simple
          p <- plotly::plot_ly(
            data = var_coords,
            x = ~Dim.1,
            y = ~Dim.2,
            color = ~Cluster,
            colors = rainbow(m$n_clusters),
            text = ~Variable,
            type = "scatter",
            mode = "markers+text",
            marker = list(size = 12),
            textposition = "top center",
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Cluster: %{color}<br>",
              "Dim1: %{x:.3f}<br>",
              "Dim2: %{y:.3f}<br>",
              "<extra></extra>"
            )
          ) %>%
            plotly::layout(
              title = "MCA - Clustering de Variables (packages ggplot2/ggrepel requis pour ellipses)",
              xaxis = list(title = sprintf("Dim 1 (%.1f%%)", variance_explained[1])),
              yaxis = list(title = sprintf("Dim 2 (%.1f%%)", variance_explained[2]))
            )

          cat("✓ MCA variables 2D créé (version simple)\n")
          return(p)
        }

        # Plot avec ellipses (ggplot2)
        library(ggplot2)
        library(ggrepel)

        couleurs <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C")
        couleurs <- couleurs[1:m$n_clusters]

        p_gg <- ggplot(var_coords, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
          geom_point(size = 6, alpha = 0.9) +
          stat_ellipse(level = 0.95, linewidth = 2, alpha = 0.2) +
          geom_text_repel(
            aes(label = Variable),
            size = 5,
            fontface = "bold",
            max.overlaps = 50,
            box.padding = 0.8,
            point.padding = 0.5,
            segment.color = "gray50",
            segment.alpha = 0.6,
            segment.size = 0.8
          ) +
          scale_color_manual(values = couleurs, name = "Cluster") +
          theme_minimal(base_size = 16) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
            plot.subtitle = element_text(hjust = 0.5, size = 14),
            legend.position = "right",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14, face = "bold"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA)
          ) +
          labs(
            title = "Clustering de Variables - MCA 2D",
            subtitle = sprintf(
              "Chaque point = 1 variable | Dim1: %.1f%% | Dim2: %.1f%% | Total: %.1f%%",
              variance_explained[1], variance_explained[2], sum(variance_explained)
            ),
            x = sprintf("Dimension 1 (%.1f%%)", variance_explained[1]),
            y = sprintf("Dimension 2 (%.1f%%)", variance_explained[2]),
            caption = "Note: Position = barycentre pondéré des modalités"
          )

        cat("✓ MCA variables 2D créé (avec ellipses)\n")

        # Convertir en plotly pour interactivité
        p <- plotly::ggplotly(p_gg, tooltip = "text")

        return(p)
      }

      # ═══════════════════════════════════════════════════════════════
      # PLOT : MCA VARIABLES 3D INTERACTIF ⭐⭐ NOUVEAU
      # ═══════════════════════════════════════════════════════════════
      if (input$plot_type == "mca_variables_3d") {

        cat("\n→ Génération MCA VARIABLES 3D INTERACTIF (Clustering)\n")

        # Variables actives
        X_active <- data()[, names(m$clusters), drop = FALSE]

        # MCA global
        mca_global <- FactoMineR::MCA(X_active, graph = FALSE, ncp = 5)

        # ⭐ CALCULER COORDONNÉES DES VARIABLES en 3D
        var_coords <- compute_variable_coordinates(mca_global, X_active, n_dims = 3)

        # Ajouter les clusters
        var_coords$Variable <- rownames(var_coords)
        var_coords$Cluster <- factor(m$clusters[var_coords$Variable])

        # Variance expliquée
        variance_3d <- mca_global$eig[1:3, 2]

        cat("  Variables : ", nrow(var_coords), "\n", sep = "")
        cat("  Variance : Dim1=", round(variance_3d[1], 1), "%, ",
            "Dim2=", round(variance_3d[2], 1), "%, ",
            "Dim3=", round(variance_3d[3], 1), "%\n", sep = "")

        # Couleurs
        couleurs_3d <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C")
        couleurs_3d <- couleurs_3d[1:m$n_clusters]

        # Plot 3D interactif
        p <- plotly::plot_ly(
          data = var_coords,
          x = ~Dim.1,
          y = ~Dim.2,
          z = ~Dim.3,
          color = ~Cluster,
          colors = couleurs_3d,
          text = ~paste0(
            "<b>", Variable, "</b><br>",
            "Cluster: ", Cluster, "<br>",
            "Dim1: ", round(Dim.1, 3), "<br>",
            "Dim2: ", round(Dim.2, 3), "<br>",
            "Dim3: ", round(Dim.3, 3)
          ),
          type = "scatter3d",
          mode = "markers+text",
          marker = list(
            size = 12,
            line = list(color = "white", width = 2),
            opacity = 0.95
          ),
          textfont = list(size = 12, family = "Arial Black"),
          textposition = "top center",
          hoverinfo = "text"
        ) %>%
          plotly::layout(
            title = list(
              text = sprintf(
                "<b>Clustering de Variables - MCA 3D Interactif</b><br><sub>🎯 Chaque point = 1 variable | Dim1: %.1f%% | Dim2: %.1f%% | Dim3: %.1f%% | Total: %.1f%%</sub>",
                variance_3d[1], variance_3d[2], variance_3d[3], sum(variance_3d[1:3])
              ),
              font = list(size = 18, family = "Arial, sans-serif")
            ),
            scene = list(
              xaxis = list(
                title = sprintf("Dim 1 (%.1f%%)", variance_3d[1]),
                gridcolor = "rgb(240, 240, 240)",
                showbackground = TRUE,
                backgroundcolor = "rgb(250, 250, 250)"
              ),
              yaxis = list(
                title = sprintf("Dim 2 (%.1f%%)", variance_3d[2]),
                gridcolor = "rgb(240, 240, 240)",
                showbackground = TRUE,
                backgroundcolor = "rgb(250, 250, 250)"
              ),
              zaxis = list(
                title = sprintf("Dim 3 (%.1f%%)", variance_3d[3]),
                gridcolor = "rgb(240, 240, 240)",
                showbackground = TRUE,
                backgroundcolor = "rgb(250, 250, 250)"
              ),
              camera = list(
                eye = list(x = 1.8, y = 1.8, z = 1.8)
              )
            ),
            showlegend = TRUE,
            legend = list(
              title = list(text = "<b>Clusters</b>"),
              orientation = "v",
              x = 1.05,
              y = 0.5,
              font = list(size = 14)
            ),
            annotations = list(
              list(
                text = "🎯 Rotation: Clic gauche | Zoom: Molette | Pan: Clic droit",
                xref = "paper", yref = "paper",
                x = 0.5, y = -0.1,
                showarrow = FALSE,
                font = list(size = 12, color = "gray")
              )
            )
          )

        cat("✓ MCA variables 3D créé\n")
        cat("  💡 Chaque point = 1 VARIABLE (clustering des variables !)\n")
        cat("  💡 Position = barycentre pondéré des modalités\n")

        return(p)
      }

      # ═══════════════════════════════════════════════════════════════
      # PLOT : HEATMAP DISTANCES ENTRE VARIABLES ⭐ NOUVEAU
      # ═══════════════════════════════════════════════════════════════
      if (input$plot_type == "distances_variables") {

        cat("\n→ Génération HEATMAP DISTANCES VARIABLES\n")

        # Variables actives
        X_active <- data()[, names(m$clusters), drop = FALSE]

        # MCA global
        mca_global <- FactoMineR::MCA(X_active, graph = FALSE, ncp = 5)

        # ⭐ CALCULER COORDONNÉES DES VARIABLES
        var_coords <- compute_variable_coordinates(mca_global, X_active, n_dims = 5)

        # Ajouter les clusters
        var_coords$Cluster <- m$clusters[rownames(var_coords)]

        # Matrice des distances euclidiennes entre variables
        coords_matrix <- as.matrix(var_coords[, 1:5])
        dist_matrix <- as.matrix(dist(coords_matrix))

        # Réordonner par cluster
        order_idx <- order(var_coords$Cluster)
        dist_matrix <- dist_matrix[order_idx, order_idx]

        var_names_ordered <- rownames(var_coords)[order_idx]
        clusters_ordered <- var_coords$Cluster[order_idx]

        cat("  Variables : ", nrow(var_coords), "\n", sep = "")
        cat("  Distance minimale :", round(min(dist_matrix[dist_matrix > 0]), 3), "\n")
        cat("  Distance maximale :", round(max(dist_matrix), 3), "\n")

        # Heatmap interactive avec plotly
        p <- plotly::plot_ly(
          z = dist_matrix,
          x = var_names_ordered,
          y = var_names_ordered,
          type = "heatmap",
          colorscale = list(
            c(0, "darkblue"),
            c(0.5, "white"),
            c(1, "darkred")
          ),
          colorbar = list(title = "Distance"),
          hovertemplate = paste0(
            "<b>%{y}</b> ↔ <b>%{x}</b><br>",
            "Distance: %{z:.3f}<br>",
            "<extra></extra>"
          )
        ) %>%
          plotly::layout(
            title = list(
              text = "Heatmap des Distances Entre Variables<br><sub>Variables réordonnées par cluster | Distances euclidiennes dans l'espace MCA</sub>",
              font = list(size = 16)
            ),
            xaxis = list(
              title = "",
              tickangle = 45,
              tickfont = list(size = 12)
            ),
            yaxis = list(
              title = "",
              tickfont = list(size = 12)
            ),
            margin = list(l = 120, r = 80, t = 120, b = 120)
          )

        # Ajouter les délimitations de clusters
        cluster_breaks <- which(diff(as.numeric(clusters_ordered)) != 0)

        shapes <- list()
        for (brk in cluster_breaks) {
          shapes[[length(shapes) + 1]] <- list(
            type = "line",
            x0 = brk + 0.5, x1 = brk + 0.5,
            y0 = 0, y1 = length(var_names_ordered),
            line = list(color = "black", width = 2, dash = "dash")
          )
          shapes[[length(shapes) + 1]] <- list(
            type = "line",
            x0 = 0, x1 = length(var_names_ordered),
            y0 = brk + 0.5, y1 = brk + 0.5,
            line = list(color = "black", width = 2, dash = "dash")
          )
        }

        p <- p %>% plotly::layout(shapes = shapes)

        cat("✓ Heatmap distances variables créée\n")
        cat("  💡 Lignes pointillées = délimitations entre clusters\n")
        cat("  💡 Chaque cellule = distance entre 2 VARIABLES\n")

        return(p)
      }


      cat("========================================\n\n")
    })

    # =========================================================================
    # RÉSUMÉ TEXTUEL
    # =========================================================================

    output$model_summary <- renderPrint({
      req(is_varclus_quali())
      m <- get_quali_model()
      req(m)

      m$summary()
    })

    # =========================================================================
    # TABLEAUX
    # =========================================================================

    output$assignments_table <- renderDT({
      req(is_varclus_quali())
      m <- get_quali_model()
      req(m)

      df_assign <- data.frame(
        Variable = names(m$clusters),
        Cluster = paste("Cluster", m$clusters),
        eta2 = sapply(1:length(m$clusters), function(i) {
          var <- names(m$clusters)[i]
          clust <- m$clusters[i]
          m$eta2_matrix[var, clust]
        }),
        stringsAsFactors = FALSE
      )

      datatable(
        df_assign,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(1, 'asc'), list(2, 'desc'))
        ),
        rownames = FALSE,
        colnames = c("Variable", "Cluster", "η² avec son cluster")
      ) %>%
        formatRound("eta2", 3)
    })

    output$eta2_table <- renderDT({
      req(is_varclus_quali())
      m <- get_quali_model()
      req(m)

      eta2_df <- as.data.frame(m$eta2_matrix)
      eta2_df <- tibble::rownames_to_column(eta2_df, "Variable")

      datatable(
        eta2_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        formatRound(columns = 2:ncol(eta2_df), digits = 3) %>%
        formatStyle(
          columns = 2:ncol(eta2_df),
          backgroundColor = styleInterval(
            cuts = seq(0.1, 0.9, 0.1),  # 9 cuts (CORRIGÉ: était 0 à 1 = 11 cuts)
            values = colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(10)  # 10 colors (CORRIGÉ: était 11)
          )
        )
    })

    # =========================================================================
    # TÉLÉCHARGEMENTS
    # =========================================================================

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("quali_", input$plot_type, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(is_varclus_quali())
        m <- get_quali_model()
        req(m)

        png(file, width = 1200, height = 800, res = 120)

        # Reproduire le plot actuel
        # (Code simplifié, à adapter selon le type)
        if (input$plot_type == "eta2_heatmap") {
          m$plot_eta2_heatmap_pro2(show_values = input$show_values)
        } else if (input$plot_type == "cluster_sizes") {
          m$plot_cluster_sizes_pro()
        } else if (input$plot_type == "inertia") {
          m$plot_inertia_pro()
        }

        dev.off()
      }
    )

    output$download_all <- downloadHandler(
      filename = function() {
        paste0("quali_all_plots_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(is_varclus_quali())
        m <- get_quali_model()
        req(m)

        pdf(file, width = 14, height = 10)

        tryCatch({
          # Page 1: Heatmap
          m$plot_eta2_heatmap_pro2(show_values = TRUE)

          # Page 2: Tailles
          m$plot_cluster_sizes_pro()

          # Page 3: Inertie
          m$plot_inertia_pro()

          # Page 4: Sélection k (si dispo)
          if (!is.null(m$scores_k_selection)) {
            m$plot_k_selection_pro()
          }

        }, error = function(e) {
          message("Erreur création plot:", e$message)
        })

        dev.off()
      }
    )
  })
}
