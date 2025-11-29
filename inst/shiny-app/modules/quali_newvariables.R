# ═══════════════════════════════════════════════════════════════════════
# MODULE: quali_newvariables_varclus (VARCLUSQuali - variables qualitatives)
# VERSION FINALE : Interface optimisée + Statistiques pertinentes
# ═══════════════════════════════════════════════════════════════════════

#' @title Quali New Variables UI (VARCLUSQuali)
#' @description Interface pour création / projection de nouvelles variables qualitatives
quali_newvariables_ui <- function(id){
  ns <- NS(id)

  tagList(
    h2("🔮 Projection de Nouvelles Variables & Illustratives (Qualitatives – VARCLUS)"),

    fluidRow(
      # ───────── CRÉATION / SÉLECTION ─────────
      box(
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        title = "📍 Étape 1 : Choisir la Variable",

        radioButtons(
          ns("var_source"),
          "Type de variable :",
          choices = c(
            "🆕 Créer une nouvelle variable qualitative" = "create",
            "📊 Variable illustrative existante"        = "illustrative"
          ),
          selected = "create"
        ),

        hr(),

        # ─── Si création ───
        conditionalPanel(
          condition = "input.var_source == 'create'",
          ns = ns,

          h4("🆕 Créer une Variable Qualitative"),
          textInput(ns("new_var_name"), "Nom", "NewVar1"),

          selectInput(
            ns("operation"),
            "Opération :",
            c(
              "Combinaison (A_B)"               = "combine",
              "Si A = X alors Y sinon Z"        = "ifelse",
              "Découpage quantile (Low/Med/High)" = "cut"
            )
          ),

          conditionalPanel(
            condition = "input.operation == 'combine'",
            ns = ns,
            p(class = "text-info", icon("info-circle"),
              " Combine 2 variables qualitatives : A=X & B=Y → X_Y")
          ),

          conditionalPanel(
            condition = "input.operation == 'ifelse'",
            ns = ns,
            p(class = "text-info", icon("info-circle"),
              " Condition : Si variable A = modalité X, alors Y sinon Z.")
          ),

          conditionalPanel(
            condition = "input.operation == 'cut'",
            ns = ns,
            p(class = "text-info", icon("info-circle"),
              " Découpe une variable numérique en 3 niveaux (quantiles).")
          ),

          uiOutput(ns("var_picker")),
          uiOutput(ns("operation_params")),

          actionButton(
            ns("generate_var"),
            "➕ Générer la Variable",
            icon = icon("plus"),
            class = "btn-success btn-block"
          )
        ),

        # ─── Si illustrative ───
        conditionalPanel(
          condition = "input.var_source == 'illustrative'",
          ns = ns,

          h4("📊 Variable Illustrative"),
          uiOutput(ns("illu_picker")),
          p(class = "text-muted",
            "Une variable illustrative est présente dans les données mais n'a pas servi à construire les clusters.")
        ),

        hr(),

        # ─── Bouton de prédiction ───
        actionButton(
          ns("predict_var"),
          "🔮 Prédire le Cluster & Projeter",
          icon = icon("magic"),
          class = "btn-primary btn-lg btn-block"
        )
      ),

      # ───────── INFORMATIONS GÉNÉRALES ─────────
      box(
        width = 4,
        status = "warning",
        solidHeader = TRUE,
        title = "📋 Informations Générales",

        verbatimTextOutput(ns("var_info"))
      ),

      # ───────── QUALITÉ DE REPRÉSENTATION ─────────
      box(
        width = 4,
        status = "info",
        solidHeader = TRUE,
        title = "📈 Qualité de Représentation",

        verbatimTextOutput(ns("representation_quality"))
      )
    ),

    # ───────── VISUALISATION ─────────
    fluidRow(
      box(
        width = 8,
        status = "success",
        solidHeader = TRUE,
        title = "🎯 Visualisation MCA 2D (Clustering de Variables + Nouvelle Variable)",

        plotly::plotlyOutput(ns("plot_2d"), height = "700px")
      ),

      # ───────── DISTANCES (COLLAPSIBLE) ─────────
      box(
        width = 4,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "🔍 Analyse de Proximité",

        p(class = "text-muted",
          "Distances calculées dans l'espace MCA complet (5 dimensions)"),

        uiOutput(ns("modality_selector")),

        verbatimTextOutput(ns("closest_modalities"))
      )
    )
  )
}

#' @title Quali New Variables Server (VARCLUSQuali)
#' @description Logique serveur pour nouvelles variables qualitatives avec VARCLUSQuali
quali_newvariables_server <- function(id, model, data, clusters){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # ═══════════════════════════════════════════════════════════════
    # REACTIVES & STATE
    # ═══════════════════════════════════════════════════════════════

    rv <- reactiveValues(
      var_factor   = NULL,
      var_name     = NULL,
      var_type     = NULL,
      pred         = NULL,
      mca_global   = NULL,
      supp_coords  = NULL
    )

    # Vérifier qu'on a bien un modèle VARCLUSQuali
    is_varclus_quali <- reactive({
      req(model())
      inherits(model(), "ClustQualiVarclus")
    })

    # ═══════════════════════════════════════════════════════════════
    # MCA GLOBAL + HELPERS
    # ═══════════════════════════════════════════════════════════════

    compute_mca_global <- reactive({
      req(is_varclus_quali(), model())

      m <- model()
      req(m$fitted)

      X <- m$data
      active_vars <- names(m$clusters)
      X_active <- X[, active_vars, drop = FALSE]

      if (!requireNamespace("FactoMineR", quietly = TRUE)) {
        stop("Le package 'FactoMineR' est requis pour le MCA.")
      }

      FactoMineR::MCA(X_active, graph = FALSE, ncp = 5)
    })

    # Calcul des coordonnées de TOUTES LES MODALITÉS
    compute_modality_coordinates <- function(mca, X, n_dims = 2, clusters) {
      mod_coords <- mca$var$coord[, 1:n_dims, drop = FALSE]
      var_names <- colnames(X)
      all_modal_names <- rownames(mod_coords)

      all_modalities <- list()

      for (v in var_names) {
        levs <- levels(X[[v]])

        # STRATÉGIE MULTI-MATCHING
        idx_in_mca <- match(levs, all_modal_names)
        found_count <- sum(!is.na(idx_in_mca))

        if (found_count == 0) {
          full_names <- paste0(v, ".", levs)
          idx_in_mca <- match(full_names, all_modal_names)
          found_count <- sum(!is.na(idx_in_mca))
        }

        if (found_count == 0) {
          pattern <- paste0("^", v)
          candidates <- grep(pattern, all_modal_names, value = TRUE, ignore.case = TRUE)

          if (length(candidates) > 0) {
            idx_in_mca <- match(candidates, all_modal_names)
          }
        }

        idx_in_mca <- idx_in_mca[!is.na(idx_in_mca)]

        if (length(idx_in_mca) > 0) {
          modal_names <- all_modal_names[idx_in_mca]
          coords <- mod_coords[idx_in_mca, , drop = FALSE]

          df_modal <- as.data.frame(coords)
          colnames(df_modal) <- paste0("Dim.", 1:n_dims)

          df_modal$Modalite <- modal_names
          df_modal$Variable <- v
          df_modal$Cluster <- clusters[v]

          all_modalities[[v]] <- df_modal
        }
      }

      if (length(all_modalities) == 0) {
        return(data.frame(
          Dim.1 = numeric(0),
          Dim.2 = numeric(0),
          Modalite = character(0),
          Variable = character(0),
          Cluster = integer(0)
        ))
      }

      do.call(rbind, all_modalities)
    }

    # Projection supplémentaire
    project_supplementary_variable <- function(mca, X_active, new_var, n_dims = 3) {
      if (!is.factor(new_var)) {
        new_var <- as.factor(new_var)
      }

      n_obs <- nrow(X_active)
      if (length(new_var) != n_obs) {
        stop("new_var doit avoir autant de lignes que X_active")
      }

      ind_coords <- mca$ind$coord[, 1:n_dims, drop = FALSE]
      levs <- levels(new_var)
      n_lev <- length(levs)

      modal_coords <- matrix(NA_real_, nrow = n_lev, ncol = n_dims)
      rownames(modal_coords) <- levs
      colnames(modal_coords) <- paste0("Dim.", 1:n_dims)

      for (i in seq_along(levs)) {
        idx <- which(new_var == levs[i])
        if (length(idx) > 0) {
          modal_coords[i, ] <- colMeans(ind_coords[idx, , drop = FALSE])
        }
      }

      freq_table <- table(new_var)
      w <- as.numeric(freq_table[levs]) / sum(freq_table)
      w[is.na(w)] <- 0

      var_coords <- colSums(modal_coords * w, na.rm = TRUE)
      names(var_coords) <- paste0("Dim.", 1:n_dims)

      list(
        modal_coords = modal_coords,
        var_coords   = var_coords
      )
    }

    # ═══════════════════════════════════════════════════════════════
    # UI OUTPUTS : VAR PICKER, OPERATION PARAMS, ILLU PICKER
    # ═══════════════════════════════════════════════════════════════

    output$var_picker <- renderUI({
      req(data())
      df <- data()

      if (input$operation == "combine") {
        factor_vars <- names(df)[sapply(df, is.factor)]
        if (length(factor_vars) < 2) {
          return(p("Il faut au moins 2 variables qualitatives."))
        }
        tagList(
          selectInput(ns("var1"), "Variable 1 :", factor_vars),
          selectInput(ns("var2"), "Variable 2 :", factor_vars, selected = factor_vars[min(2, length(factor_vars))])
        )
      } else if (input$operation == "ifelse") {
        factor_vars <- names(df)[sapply(df, is.factor)]
        if (length(factor_vars) == 0) {
          return(p("Aucune variable qualitative disponible."))
        }
        selectInput(ns("var1"), "Variable à tester :", factor_vars)
      } else if (input$operation == "cut") {
        num_vars <- names(df)[sapply(df, is.numeric)]
        if (length(num_vars) == 0) {
          return(p("Aucune variable numérique disponible."))
        }
        selectInput(ns("var1"), "Variable numérique :", num_vars)
      }
    })

    output$operation_params <- renderUI({
      req(input$operation, data())
      df <- data()

      if (input$operation == "ifelse") {
        req(input$var1)
        if (!(input$var1 %in% names(df))) return(NULL)
        levs <- levels(df[[input$var1]])
        tagList(
          selectInput(ns("cond_level"), "Si modalité = ", levs),
          textInput(ns("then_val"), "Alors", "Yes"),
          textInput(ns("else_val"), "Sinon", "No")
        )
      } else {
        NULL
      }
    })

    output$illu_picker <- renderUI({
      req(data(), model())
      df <- data()
      m <- model()

      active_vars <- names(m$clusters)
      available <- setdiff(names(df), active_vars)
      available <- available[sapply(df[available], is.factor)]

      if (length(available) == 0) {
        return(p("Aucune variable illustrative qualitative disponible."))
      }

      selectInput(ns("illu_var"), "Choisir une variable illustrative :", available)
    })

    # ═══════════════════════════════════════════════════════════════
    # GÉNÉRATION DE NOUVELLE VARIABLE
    # ═══════════════════════════════════════════════════════════════

    observeEvent(input$generate_var, {
      req(data(), input$new_var_name, input$operation)

      df <- data()

      new_col <- tryCatch({
        if (input$operation == "combine") {
          req(input$var1, input$var2)
          v1 <- df[[input$var1]]
          v2 <- df[[input$var2]]
          interaction(v1, v2, sep = "_", drop = TRUE)

        } else if (input$operation == "ifelse") {
          req(input$var1, input$cond_level, input$then_val, input$else_val)
          v <- df[[input$var1]]
          res <- ifelse(v == input$cond_level, input$then_val, input$else_val)
          as.factor(res)

        } else if (input$operation == "cut") {
          req(input$var1)
          num_col <- df[[input$var1]]
          if (!is.numeric(num_col)) {
            stop("Variable non numérique pour cut.")
          }
          cut(num_col, breaks = quantile(num_col, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
              labels = c("Low", "Medium", "High"), include.lowest = TRUE)
        } else {
          stop("Opération inconnue.")
        }
      }, error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error", duration = 5)
        NULL
      })

      if (!is.null(new_col)) {
        rv$var_factor <- new_col
        rv$var_name   <- input$new_var_name
        rv$var_type   <- "new"
        showNotification(
          paste("Variable", input$new_var_name, "créée avec succès !"),
          type = "message", duration = 3
        )
      }
    })

    # ═══════════════════════════════════════════════════════════════
    # PRÉDICTION + PROJECTION
    # ═══════════════════════════════════════════════════════════════

    observeEvent(input$predict_var, {
      req(model(), data())

      m <- model()
      req(m$fitted)

      if (input$var_source == "create") {
        req(rv$var_factor, rv$var_name)
        var_to_predict <- rv$var_factor
        var_name       <- rv$var_name
        var_type       <- "new"

      } else {
        req(input$illu_var)
        df <- data()
        if (!(input$illu_var %in% names(df))) {
          showNotification("Variable illustrative introuvable.", type = "error", duration = 5)
          return()
        }
        var_to_predict <- df[[input$illu_var]]
        var_name       <- input$illu_var
        var_type       <- "illustrative"
      }

      if (!is.factor(var_to_predict)) {
        var_to_predict <- as.factor(var_to_predict)
      }

      df_pred <- data.frame(X = var_to_predict)
      names(df_pred) <- var_name

      pred_result <- tryCatch({
        m$predict(df_pred)
      }, error = function(e) {
        showNotification(paste("Erreur predict :", e$message), type = "error", duration = 7)
        NULL
      })

      if (is.null(pred_result)) return()

      mca <- compute_mca_global()
      X_active <- m$data[, names(m$clusters), drop = FALSE]

      supp_coords <- project_supplementary_variable(
        mca, X_active, var_to_predict, n_dims = min(5, ncol(mca$var$coord))
      )

      rv$var_factor  <- var_to_predict
      rv$var_name    <- var_name
      rv$var_type    <- var_type
      rv$pred        <- pred_result
      rv$mca_global  <- mca
      rv$supp_coords <- supp_coords

      showNotification(
        paste0("Projection réussie pour '", var_name, "'"),
        type = "message", duration = 3
      )
    })

    # ═══════════════════════════════════════════════════════════════
    # OUTPUTS : STATISTIQUES
    # ═══════════════════════════════════════════════════════════════

    output$var_info <- renderPrint({
      req(rv$var_factor, rv$var_name)

      cat("Variable :", rv$var_name, "\n")
      cat("Type :", if(rv$var_type == "new") "Nouvelle (créée)" else "Illustrative (existante)", "\n")
      cat("Nombre de modalités :", length(levels(rv$var_factor)), "\n")
      cat("Nombre d'observations :", length(rv$var_factor), "\n\n")

      tab <- table(rv$var_factor)
      cat("Effectifs par modalité :\n")
      for (i in seq_along(tab)) {
        cat(sprintf("  %-20s : %3d (%.1f%%)\n",
                    names(tab)[i], tab[i], 100*tab[i]/sum(tab)))
      }
    })

    output$modality_selector <- renderUI({
      req(rv$supp_coords)

      modal_names <- rownames(rv$supp_coords$modal_coords)

      selectInput(
        ns("selected_modality"),
        "Sélectionner une modalité :",
        choices = modal_names,
        selected = modal_names[1]
      )
    })

    output$closest_modalities <- renderPrint({
      req(rv$supp_coords, rv$mca_global, input$selected_modality)

      mca <- rv$mca_global
      supp_coords <- rv$supp_coords$modal_coords

      n_dims_total <- min(5, ncol(mca$var$coord))

      sel_idx <- which(rownames(supp_coords) == input$selected_modality)
      if (length(sel_idx) == 0) return(cat("Modalité non trouvée.\n"))

      sel_coords <- supp_coords[sel_idx, 1:n_dims_total]

      all_coords <- mca$var$coord[, 1:n_dims_total, drop = FALSE]

      distances <- apply(all_coords, 1, function(row) {
        sqrt(sum((row - sel_coords)^2))
      })

      sorted_idx <- order(distances)
      top_10 <- sorted_idx[1:min(10, length(sorted_idx))]

      cat("Modalité sélectionnée :", input$selected_modality, "\n")
      cat("Distance calculée sur", n_dims_total, "dimensions\n\n")
      cat("Top 10 modalités les plus proches :\n")
      cat(sprintf("%-4s %-30s %8s\n", "Rang", "Modalité", "Distance"))
      cat(strrep("-", 45), "\n")

      for (i in seq_along(top_10)) {
        idx <- top_10[i]
        cat(sprintf("%-4d %-30s %8.4f\n",
                    i,
                    rownames(all_coords)[idx],
                    distances[idx]))
      }
    })

    output$representation_quality <- renderPrint({
      req(rv$mca_global, rv$supp_coords)

      mca <- rv$mca_global
      variance <- mca$eig[, 2]

      cat("Variance expliquée par dimension :\n")
      for (i in 1:min(5, length(variance))) {
        cat(sprintf("  Dim %d : %.2f%%\n", i, variance[i]))
      }

      cat(sprintf("\nCumulée (5 dims) : %.2f%%\n\n", sum(variance[1:min(5, length(variance))])))

      supp_coords <- rv$supp_coords$modal_coords
      n_dims <- min(5, ncol(supp_coords))

      cos2 <- rowSums(supp_coords[, 1:n_dims]^2)

      cat("Qualité de représentation (cos²) :\n")
      for (i in seq_len(nrow(supp_coords))) {
        cat(sprintf("  %-20s : %.3f %s\n",
                    rownames(supp_coords)[i],
                    cos2[i],
                    if(cos2[i] > 0.5) "✓" else ""))
      }

      cat("\n✓ = bien représenté (cos² > 0.5)\n")
    })

    # ═══════════════════════════════════════════════════════════════
    # PLOT 2D INTERACTIF
    # ═══════════════════════════════════════════════════════════════

    output$plot_2d <- plotly::renderPlotly({
      req(is_varclus_quali(), model(), clusters(), rv$pred, rv$mca_global, rv$supp_coords)

      m <- model()
      mca <- rv$mca_global
      X_active <- m$data[, names(m$clusters), drop = FALSE]

      # Toujours utiliser Dim 1 x Dim 2
      axes_nums <- c(1, 2)

      n_dims_available <- ncol(mca$var$coord)
      if (any(axes_nums > n_dims_available)) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              title = sprintf("Erreur : seulement %d dimensions disponibles", n_dims_available)
            )
        )
      }

      modal_coords <- compute_modality_coordinates(mca, X_active, n_dims = max(axes_nums), m$clusters)

      modal_coords_plot <- modal_coords
      modal_coords_plot$X_axis <- modal_coords[[paste0("Dim.", axes_nums[1])]]
      modal_coords_plot$Y_axis <- modal_coords[[paste0("Dim.", axes_nums[2])]]

      variance_explained <- mca$eig[axes_nums, 2]

      if (nrow(modal_coords_plot) == 0) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              title = "Erreur : aucune coordonnée de modalité"
            )
        )
      }

      p <- plotly::plot_ly()

      clusters_unique <- sort(unique(modal_coords_plot$Cluster))
      couleurs <- rainbow(length(clusters_unique))

      for (i in seq_along(clusters_unique)) {
        cluster_i <- clusters_unique[i]
        df_i <- modal_coords_plot[modal_coords_plot$Cluster == cluster_i, ]

        p <- p %>%
          plotly::add_trace(
            data = df_i,
            x = ~X_axis,
            y = ~Y_axis,
            type = "scatter",
            mode = "markers+text",
            text = ~Modalite,
            customdata = ~Variable,
            marker = list(
              size = 8,
              color = couleurs[i],
              line = list(color = "white", width = 1),
              opacity = 0.7
            ),
            textposition = "top center",
            textfont = list(size = 8),
            name = paste("Cluster", cluster_i),
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Variable: %{customdata}<br>",
              "Cluster: ", cluster_i, "<br>",
              "Dim1: %{x:.3f}<br>",
              "Dim2: %{y:.3f}<br>",
              "<extra></extra>"
            )
          )
      }

      supp_modal <- rv$supp_coords$modal_coords

      if (nrow(supp_modal) > 0 && ncol(supp_modal) >= max(axes_nums)) {

        df_supp <- data.frame(
          X_axis = supp_modal[, axes_nums[1]],
          Y_axis = supp_modal[, axes_nums[2]],
          Modalite = rownames(supp_modal)
        )

        symbol <- if (rv$var_type == "new") "diamond" else "x"
        color  <- if (rv$var_type == "new") "red" else "black"
        trace_name <- if (rv$var_type == "new") {
          paste0("🆕 ", rv$var_name)
        } else {
          paste0("📊 ", rv$var_name, " (illustrative)")
        }

        p <- p %>%
          plotly::add_trace(
            data = df_supp,
            x = ~X_axis,
            y = ~Y_axis,
            type = "scatter",
            mode = "markers+text",
            text = ~Modalite,
            marker = list(
              size   = 12,
              symbol = symbol,
              color  = color,
              line   = list(color = "white", width = 2)
            ),
            textposition = "top center",
            textfont = list(size = 10, family = "Arial Black"),
            name = trace_name,
            hovertemplate = paste0(
              "<b>%{text}</b><br>",
              "Variable: ", rv$var_name, "<br>",
              "Type: ", rv$var_type, "<br>",
              "Dim1: %{x:.3f}<br>",
              "Dim2: %{y:.3f}<br>",
              "<extra></extra>"
            ),
            inherit = FALSE
          )
      }

      p %>%
        plotly::layout(
          title = list(
            text = sprintf(
              "MCA 2D – Modalités par Cluster<br><sub>Dim1: %.1f%% | Dim2: %.1f%% | Total: %.1f%%</sub>",
              variance_explained[1], variance_explained[2],
              sum(variance_explained)
            ),
            font = list(size = 16)
          ),
          xaxis = list(
            title = sprintf("Dim 1 (%.1f%%)", variance_explained[1]),
            zeroline = TRUE,
            zerolinewidth = 1,
            zerolinecolor = "lightgray"
          ),
          yaxis = list(
            title = sprintf("Dim 2 (%.1f%%)", variance_explained[2]),
            zeroline = TRUE,
            zerolinewidth = 1,
            zerolinecolor = "lightgray"
          ),
          hovermode = "closest",
          showlegend = TRUE,
          legend = list(
            title = list(text = "<b>Groupes</b>"),
            orientation = "v"
          )
        )
    })
  })
}
