# ==========================================================
# MODULE : kmeans_newvariables
# - Création / projection de nouvelles variables (K-means)
# - Assignation au cluster par corr² moyen avec les variables du cluster
# - Soft membership = scores normalisés
# - Visualisation PCA (2D / 3D) des variables + nouvelle variable
# ==========================================================

kmeans_newvariables_ui <- function(id){
  ns <- NS(id)

  tagList(
    h2("🔮 Nouvelles variables & Projection (K-means)"),

    # ============================================================
    # LIGNE 1 : CHOIX VARIABLE + RÉSUMÉ
    # ============================================================
    fluidRow(

      # ----------------------------------------------------------
      # 1) BOX — CRÉATION / EXISTANTE
      # ----------------------------------------------------------
      box(
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        title = "📍 Étape 1 : Choisir la variable",

        radioButtons(
          ns("var_source"),
          "Type de variable :",
          choices = c(
            "🆕 Créer une nouvelle variable" = "create",
            "📊 Variable existante du dataset" = "existing"
          ),
          selected = "create"
        ),

        hr(),

        # ---- Création ----
        conditionalPanel(
          condition = "input.var_source == 'create'",
          ns = ns,

          h4("🆕 Nouvelle variable (combinaison)"),
          textInput(ns("new_var_name"), "Nom de la nouvelle variable :", "NewVar1"),

          selectInput(
            ns("operation"),
            "Transformation :",
            c(
              "Somme (x + y)"          = "sum",
              "Moyenne (x + y) / 2"    = "mean",
              "Différence (x - y)"     = "diff",
              "Produit (x × y)"        = "prod",
              "Ratio (x / y)"          = "ratio",
              "Maximum (max)"          = "max",
              "Minimum (min)"          = "min"
            )
          ),

          uiOutput(ns("var_picker")),

          actionButton(
            ns("generate_var"),
            "➕ Générer la variable",
            icon = icon("plus"),
            class = "btn-success btn-block"
          )
        ),

        # ---- Variable existante ----
        conditionalPanel(
          condition = "input.var_source == 'existing'",
          ns = ns,

          h4("📊 Variable existante"),
          uiOutput(ns("existing_var_ui")),

          p(class = "text-info",
            icon("info-circle"),
            "La variable sera projetée et assignée à un cluster K-means ",
            "sans modifier le modèle existant.")
        ),

        hr(),

        # ---- PREDICTION ----
        actionButton(
          ns("predict_var"),
          "🔮 Assigner au cluster & Visualiser",
          icon = icon("magic"),
          class = "btn-primary btn-lg btn-block"
        )
      ),

      # ----------------------------------------------------------
      # 2) BOX — RÉSUMÉ
      # ----------------------------------------------------------
      box(
        width = 4,
        status = "warning",
        solidHeader = TRUE,
        title = "📋 Résumé de la projection",

        verbatimTextOutput(ns("summary")),
        hr(),

        h4("Soft membership"),
        tableOutput(ns("soft_table"))
      ),

      # ----------------------------------------------------------
      # 3) BOX — INFOS / AIDE
      # ----------------------------------------------------------
      box(
        width = 4,
        status = "info",
        solidHeader = TRUE,
        title = "ℹ️ Détails de la méthode",

        tags$ul(
          tags$li("Chaque cluster est représenté par l'ensemble de ses variables."),
          tags$li("Pour la nouvelle variable, on calcule la corrélation avec ",
                  "les variables de chaque cluster."),
          tags$li("Score du cluster = moyenne de ", tags$code("cor(new, var)^2"),
                  " sur les variables du cluster."),
          tags$li("Hard cluster = celui avec le score le plus élevé."),
          tags$li("Soft membership = scores normalisés (somme = 1).")
        )
      )
    ),

    # ============================================================
    # LIGNE 2 : PLOTS
    # ============================================================
    fluidRow(
      box(
        width = 3,
        status = "info",
        solidHeader = TRUE,
        title = "📊 Paramètres de visualisation",

        checkboxInput(
          ns("use_scale"),
          "Standardiser les variables",
          value = TRUE
        ),

        helpText("La PCA est recalculée sur :",
                 tags$br(),
                 tags$code("variables actives + nouvelle variable")),

        hr(),

        selectInput(
          ns("pc_axes"),
          "Axes PCA à afficher :",
          choices = c(
            "PC1 × PC2" = "1-2",
            "PC1 × PC3" = "1-3",
            "PC1 × PC4" = "1-4",
            "PC2 × PC3" = "2-3",
            "PC2 × PC4" = "2-4",
            "PC3 × PC4" = "3-4"
          ),
          selected = "1-2"
        ),

        hr(),

        numericInput(ns("pt_size"),
                     "Taille des points :",
                     value = 10, min = 4, max = 30, step = 1)
      ),

      box(
        width = 9,
        status = "success",
        solidHeader = TRUE,
        title = "🎯 Projection PCA 2D des variables (K-means)",

        plotlyOutput(ns("plot_2d"), height = "700px")
      )
    )
  )
}

kmeans_newvariables_server <- function(id, model, data, clusters, method){
  moduleServer(id, function(input, output, session){

    rv <- reactiveValues(
      new_vec = NULL,        # résultat de generate_var (create)
      current_vec = NULL,    # variable effectivement projetée (create ou existing)
      var_name = NULL,
      var_type = NULL,
      pred = NULL            # list(cluster = ..., soft = named numeric)
    )

    # ============================================================
    # PICKER DES VARIABLES ACTIVES (pour création)
    # ============================================================
    output$var_picker <- renderUI({
      req(data(), clusters())
      # On travaille sur les variables actives (celles qui ont un cluster)
      var_names <- names(clusters())
      # garder que les numériques
      numeric_vars <- var_names[sapply(var_names, function(v) is.numeric(data()[[v]]))]

      if(length(numeric_vars) < 2){
        return(p("Au moins 2 variables numériques actives sont nécessaires."))
      }

      shinyWidgets::pickerInput(
        session$ns("vars"),
        "Choisir 2 variables actives :",
        choices = numeric_vars,
        multiple = TRUE,
        options = list(`max-options` = 2, `live-search` = TRUE)
      )
    })

    # ============================================================
    # PICKER DES VARIABLES EXISTANTES
    # ============================================================
    output$existing_var_ui <- renderUI({
      req(data())

      df <- data()
      numeric_vars <- names(df)[sapply(df, is.numeric)]

      if(length(numeric_vars) == 0){
        return(p("Aucune variable numérique disponible dans le dataset."))
      }

      selectInput(
        session$ns("existing_var"),
        "Choisir une variable numérique :",
        choices = numeric_vars,
        selected = numeric_vars[1]
      )
    })

    # ============================================================
    # CRÉATION NOUVELLE VARIABLE (COMBINAISON)
    # ============================================================
    observeEvent(input$generate_var, {
      req(method() == "kmeans")
      req(data(), clusters())
      req(input$vars, length(input$vars) == 2)

      df <- data()
      v1 <- input$vars[1]
      v2 <- input$vars[2]

      x <- df[[v1]]
      y <- df[[v2]]

      if(!is.numeric(x) || !is.numeric(y)){
        showNotification("Les deux variables doivent être numériques.", type = "error")
        return()
      }

      new_vec <- switch(
        input$operation,
        sum   = x + y,
        mean  = (x + y)/2,
        diff  = x - y,
        prod  = x * y,
        ratio = x / (y + 1e-8),
        max   = pmax(x, y),
        min   = pmin(x, y)
      )

      rv$new_vec   <- new_vec
      rv$current_vec <- new_vec
      rv$var_name  <- input$new_var_name
      rv$var_type  <- "new"

      showNotification(
        paste("Variable", rv$var_name, "créée !"),
        type = "message"
      )
    })

    # ============================================================
    # HELPER : CALCULER L'ASSIGNATION K-MEANS
    # ============================================================
    compute_membership <- function(v, df, cl_vec){

      # v : vecteur numeric (longueur = n obs)
      # df : data() complet
      # cl_vec : vecteur nommée Variable -> cluster (clusters())

      K <- max(cl_vec, na.rm = TRUE)
      scores <- rep(0, K)

      for(k in 1:K){
        vars_k <- names(cl_vec)[cl_vec == k]
        vars_k <- vars_k[vars_k %in% colnames(df)]

        if(length(vars_k) == 0){
          scores[k] <- 0
        } else {
          # corrélation avec chaque variable du cluster
          cors2 <- sapply(vars_k, function(nm){
            xk <- df[[nm]]
            if(!is.numeric(xk)) return(NA_real_)
            cor(v, xk, use = "pairwise.complete.obs")^2
          })

          if(all(is.na(cors2))){
            scores[k] <- 0
          } else {
            scores[k] <- mean(cors2, na.rm = TRUE)
          }
        }
      }

      # Hard cluster
      hard <- which.max(scores)

      # Soft membership
      if(sum(scores) > 0){
        soft <- scores / sum(scores)
      } else {
        soft <- rep(1/K, K)
      }

      names(soft) <- paste0("Cluster", 1:K)

      list(
        cluster = hard,
        soft    = soft
      )
    }

    # ============================================================
    # PRÉDICTION / PROJECTION
    # ============================================================
    observeEvent(input$predict_var, {
      req(method() == "kmeans")
      req(data(), clusters())

      df <- data()
      cl <- clusters()

      # ---- Choix de la source ----
      if(input$var_source == "create"){
        req(rv$new_vec)

        v <- rv$new_vec
        vname <- rv$var_name
        vtype <- "new"

      } else {
        req(input$existing_var)
        vname <- input$existing_var
        v <- df[[vname]]

        if(!is.numeric(v)){
          showNotification("La variable sélectionnée doit être numérique.", type = "error")
          return()
        }

        rv$var_name <- vname
        rv$var_type <- "existing"
        vtype <- "existing"
      }

      # Sécuriser longueur
      if(length(v) != nrow(df)){
        showNotification("Longueur de la variable incompatible avec le dataset.", type = "error")
        return()
      }

      # Calcul cluster / soft membership
      mem <- compute_membership(v, df, cl)

      rv$current_vec <- v
      rv$pred <- list(
        cluster = mem$cluster,
        soft    = mem$soft
      )
      rv$var_type <- vtype

      showNotification("Projection K-means terminée ✔", type = "message")
    })

    # ============================================================
    # RÉSUMÉ
    # ============================================================
    output$summary <- renderPrint({
      req(rv$pred, rv$var_name)

      cat("Variable :", rv$var_name, "\n")
      cat("Type     :", rv$var_type, "\n")
      cat("Cluster  :", rv$pred$cluster, "\n\n")

      cat("Soft membership (probabilités) :\n")
      print(round(rv$pred$soft, 4))
    })

    output$soft_table <- renderTable({
      req(rv$pred)

      s <- rv$pred$soft
      data.frame(
        Cluster     = names(s),
        Probabilité = round(as.numeric(s), 4),
        row.names   = NULL,
        check.names = FALSE
      )
    })

    # ============================================================
    # PLOT 2D (PCA sur variables + nouvelle variable)
    # ============================================================
    output$plot_2d <- plotly::renderPlotly({
      req(method() == "kmeans")
      req(data(), clusters())
      req(rv$current_vec, rv$var_name, rv$pred)

      df <- data()
      cl <- clusters()

      # Matrice des variables actives (numériques uniquement)
      active_vars <- names(cl)
      X_active <- df[, active_vars, drop = FALSE]
      num_cols <- sapply(X_active, is.numeric)
      X_active <- X_active[, num_cols, drop = FALSE]

      if(ncol(X_active) < 1){
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "Aucune variable numérique active disponible"))
      }

      # Ajouter la nouvelle variable
      X_ext <- cbind(X_active, rv$current_vec)
      colnames(X_ext)[ncol(X_ext)] <- rv$var_name

      # Standardisation si demandé
      if(isTRUE(input$use_scale)){
        X_ext <- scale(X_ext)
      }

      # PCA sur les données (obs x vars), coordonnées des variables = loadings
      pca <- prcomp(X_ext, center = FALSE, scale. = FALSE)

      # Parser les axes sélectionnés
      axes_choice <- if(!is.null(input$pc_axes)) input$pc_axes else "1-2"
      axes_nums <- as.integer(strsplit(axes_choice, "-")[[1]])

      # Vérifier disponibilité
      n_pc_available <- ncol(pca$rotation)
      if (any(axes_nums > n_pc_available)) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              title = sprintf("Erreur : seulement %d composantes disponibles", n_pc_available)
            )
        )
      }

      loadings <- pca$rotation[, axes_nums, drop = FALSE]
      var_exp  <- summary(pca)$importance[2, axes_nums] * 100

      coords <- data.frame(
        PC_X     = loadings[, 1],
        PC_Y     = loadings[, 2],
        Variable = rownames(loadings),
        stringsAsFactors = FALSE
      )

      coords$Type <- ifelse(coords$Variable == rv$var_name, "Nouvelle", "Active")

      # Clusters pour les variables actives
      cl_full <- rep(NA_integer_, nrow(coords))
      names(cl_full) <- coords$Variable
      cl_full[names(cl)] <- cl
      cl_full[rv$var_name] <- rv$pred$cluster
      coords$Cluster <- factor(cl_full)

      # Séparer actives / nouvelle
      df_active  <- coords[coords$Type == "Active", ]
      df_new_var <- coords[coords$Type == "Nouvelle", , drop = FALSE]

      p <- plotly::plot_ly()

      if(nrow(df_active) > 0){
        p <- p %>%
          plotly::add_markers(
            data = df_active,
            x = ~PC_X, y = ~PC_Y,
            text = ~Variable,
            color = ~Cluster,
            colors = rainbow(length(unique(na.omit(df_active$Cluster)))),
            marker = list(
              size = input$pt_size,
              line = list(color = "white", width = 1)
            ),
            name = "Variables actives",
            hoverinfo = "text",
            hovertext = ~paste0(
              "Variable: ", Variable, "<br>",
              "Cluster: ", Cluster, "<br>",
              "PC", axes_nums[1], ": ", round(PC_X, 3), "<br>",
              "PC", axes_nums[2], ": ", round(PC_Y, 3)
            )
          )
      }

      # Nouvelle variable (point rouge diamant)
      p <- p %>%
        plotly::add_markers(
          data = df_new_var,
          x = ~PC_X, y = ~PC_Y,
          text = ~Variable,
          marker = list(
            size   = input$pt_size + 4,
            color  = "red",
            symbol = "diamond",
            line   = list(color = "black", width = 2)
          ),
          name = paste0("Nouvelle variable (Cluster ", rv$pred$cluster, ")"),
          hoverinfo = "text",
          hovertext = ~paste0(
            "Nouvelle variable: ", Variable, "<br>",
            "Cluster assigné: ", rv$pred$cluster, "<br>",
            "PC", axes_nums[1], ": ", round(PC_X, 3), "<br>",
            "PC", axes_nums[2], ": ", round(PC_Y, 3)
          ),
          textposition = "top center"
        )

      p <- p %>%
        plotly::layout(
          title = list(
            text = sprintf(
              "PCA 2D des variables (K-means) avec nouvelle variable<br><sub>PC%d: %.1f%% | PC%d: %.1f%% de variance expliquée</sub>",
              axes_nums[1], var_exp[1], axes_nums[2], var_exp[2]
            ),
            font = list(size = 16)
          ),
          xaxis = list(
            title = sprintf("PC%d (%.1f%%)", axes_nums[1], var_exp[1]),
            zeroline = TRUE, showgrid = TRUE
          ),
          yaxis = list(
            title = sprintf("PC%d (%.1f%%)", axes_nums[2], var_exp[2]),
            zeroline = TRUE, showgrid = TRUE
          ),
          hovermode = "closest"
        )

      p
    })
  })
}
