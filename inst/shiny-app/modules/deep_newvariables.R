deep_newvariables_ui <- function(id){
  ns <- NS(id)

  tagList(
    h2("🔮 Projection de Nouvelles Variables & Illustratives (Deep)"),

    # ============================================================
    # LIGNE 1 : CHOIX VARIABLE + PARAMS + RÉSUMÉ
    # ============================================================
    fluidRow(

      # ------------------------------------------------------------
      # 1) BOX — CRÉATION / ILLUSTRATIVE
      # ------------------------------------------------------------
      box(
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        title = "📍 Étape 1 : Choisir la Variable",

        radioButtons(
          ns("var_source"),
          "Type de variable :",
          choices = c(
            "🆕 Créer une nouvelle variable" = "create",
            "📊 Variable illustrative existante" = "illustrative"
          ),
          selected = "create"
        ),

        hr(),

        # ---- Création ----
        conditionalPanel(
          condition = "input.var_source == 'create'",
          ns = ns,

          h4("🆕 Nouvelle Variable"),
          textInput(ns("new_var_name"), "Nom", "NewVar1"),

          selectInput(
            ns("operation"),
            "Transformation :",
            c(
              "Somme (x + y)" = "sum",
              "Moyenne (x + y) / 2" = "mean",
              "Différence (x - y)" = "diff",
              "Produit (x × y)" = "prod",
              "Ratio (x / y)" = "ratio",
              "Maximum (max)" = "max",
              "Minimum (min)" = "min"
            )
          ),

          uiOutput(ns("var_picker")),

          actionButton(
            ns("generate_var"),
            "➕ Générer",
            icon = icon("plus"),
            class = "btn-success btn-block"
          )
        ),

        # ---- Illustrative ----
        conditionalPanel(
          condition = sprintf("input['%s'] == 'illustrative'", ns("var_source")),

          h4("📊 Variable Illustrative"),

          selectInput(
            ns("illu_var_predict"),
            "Choisir une variable illustrative :",
            choices = NULL
          ),

          p(class = "text-info",
            icon("info-circle"),
            "Projeter cette variable dans l'espace latent.")
        ),

        hr(),

        # ---- PREDICTION ----
        actionButton(
          ns("predict_var"),
          "🔮 Prédire & Visualiser",
          icon = icon("magic"),
          class = "btn-primary btn-lg btn-block"
        )
      ),

      # ------------------------------------------------------------
      # 3) BOX — RÉSUMÉ
      # ------------------------------------------------------------
      box(
        width = 4,
        status = "warning",
        solidHeader = TRUE,
        title = "📋 Résumé",

        verbatimTextOutput(ns("summary")),
        hr(),

        h4("Soft Membership"),
        tableOutput(ns("soft_table"))
      )
    ),

    # ============================================================
    # LIGNE 2 : PARAMS + PLOT
    # ============================================================
    fluidRow(
      box(
        width = 3,
        status = "info",
        solidHeader = TRUE,
        title = "📊 Paramètres de Visualisation",

        selectInput(
          ns("plot_type2"),
          "Type de plot :",
          choices = c("2D" = "2d", "3D" = "3d"),
          selected = "2d"
        ),

        checkboxInput(
          ns("use_pca2"),
          "Utiliser PCA",
          value = TRUE
        )
      ),

      box(
        width = 9,
        status = "success",
        solidHeader = TRUE,
        title = "🎯 Visualisation (Deep)",

        conditionalPanel(
          condition = "input.plot_type2 == '2d'",
          ns = ns,
          plotlyOutput(ns("plot_2d"), height = "700px")
        ),

        conditionalPanel(
          condition = "input.plot_type2 == '3d'",
          ns = ns,
          plotlyOutput(ns("plot_3d"), height = "700px")
        )
      )
    )
  )
}

deep_newvariables_server <- function(id,
                                     model,
                                     data,
                                     clusters,
                                     method,
                                     illu_emb,
                                     illu_vars   = NULL,
                                     illu_labels = NULL) {
  moduleServer(id, function(input, output, session){

    rv <- reactiveValues(
      new_vec  = NULL,
      pred     = NULL,
      var_name = NULL,
      var_type = NULL
    )

    # ============================================================
    # PICKER DES VARIABLES ACTIVES
    # ============================================================
    output$var_picker <- renderUI({
      req(data())

      ### DEBUG + FIX : on privilégie les colonnes du modèle Deep
      cols <- NULL
      if (!is.null(model()) && !is.null(model()$data)) {
        cols <- colnames(model()$data)
      } else {
        cols <- names(data())
      }

      cat("\n===== DEBUG var_picker =====\n")
      cat("Méthode :", tryCatch(method(), error = function(e) "N/A"), "\n")
      cat("Colonnes model()$data :",
          ifelse(is.null(model()) || is.null(model()$data),
                 "NULL",
                 paste(cols, collapse = ", ")), "\n")
      cat("================================\n")

      pickerInput(
        session$ns("vars"),
        "Choisir 2 variables :",
        choices = cols,
        multiple = TRUE,
        options  = list(`max-options` = 2, `live-search` = TRUE)
      )
    })

    # ============================================================
    # PICKER DES ILLUSTRATIVES (utilise illu_vars OU illu_emb)
    # ============================================================
    observe({
      # DEBUG
      cat("\n===== DEBUG ILLU VARS (deep_newvariables) =====\n")
      cat("method() :", method(), "\n")
      cat("illu_vars() :", paste0(tryCatch(illu_vars(), error=function(e) 'ERR'), collapse=", "), "\n")
      emb_list <- tryCatch(illu_emb(), error = function(e) NULL)
      cat("names(illu_emb()) :", paste0(names(emb_list), collapse=", "), "\n")
      cat("===============================================\n\n")

      choices <- character(0)

      # 1) Priorité aux noms sélectionnés comme illustratives
      vars_sel <- tryCatch(illu_vars(), error = function(e) NULL)
      if (!is.null(vars_sel) && length(vars_sel) > 0) {
        choices <- vars_sel
      }

      # 2) Fallback : noms présents dans illu_emb
      if (length(choices) == 0) {
        if (!is.null(emb_list) && length(emb_list) > 0) {
          choices <- names(emb_list)
        }
      }

      # 3) Mise à jour du selectInput
      updateSelectInput(
        session,
        "illu_var_predict",
        choices = choices,
        selected = if (length(choices) > 0) choices[1] else NULL
      )
    })


    # ============================================================
    # CRÉATION NOUVELLE VARIABLE
    # ============================================================
    observeEvent(input$generate_var, {
      req(model(), method() == "deep", input$vars, length(input$vars) == 2)

      cat("\n===== DEBUG generate_var (AVANT) =====\n")
      cat("input$vars :", paste(input$vars, collapse = ", "), "\n")

      Xs <- model()$data
      cat("dim(model()$data) :",
          paste(dim(Xs), collapse = " x "), "\n")
      cat("colnames(model()$data) :",
          paste(colnames(Xs), collapse = ", "), "\n")

      # FIX : vérifier que les variables existent bien dans model()$data
      if (!all(input$vars %in% colnames(Xs))) {
        cat("⚠ Certaines variables sélectionnées ne sont PAS dans model()$data !\n")
        cat("Variables demandées :", paste(input$vars, collapse = ", "), "\n")
        showNotification(
          "Les variables choisies ne font pas partie des variables actives utilisées par le modèle Deep.",
          type = "error"
        )
        return()
      }

      x  <- Xs[, input$vars[1]]
      y  <- Xs[, input$vars[2]]

      cat("Longueur x :", length(x), " | Longueur y :", length(y), "\n")
      cat("Premier x[1:5] :", paste(head(x, 5), collapse = ", "), "\n")
      cat("Premier y[1:5] :", paste(head(y, 5), collapse = ", "), "\n")
      cat("======================================\n")

      rv$new_vec <- switch(
        input$operation,
        sum   = x + y,
        mean  = (x + y)/2,
        diff  = x - y,
        prod  = x * y,
        ratio = x / (y + 1e-8),
        max   = pmax(x, y),
        min   = pmin(x, y)
      )

      rv$var_name <- input$new_var_name
      rv$var_type <- "new"

      showNotification(
        paste("Variable", rv$var_name, "créée !"),
        type = "message"
      )
    })

    # ============================================================
    # PRÉDICTION
    # ============================================================
    observeEvent(input$predict_var, {
      req(model(), method() == "deep")

      m <- model()

      cat("\n===== DEBUG predict_var =====\n")
      cat("Source :", input$var_source, "\n")
      cat("rv$var_name :", rv$var_name, "\n")
      cat("================================\n")

      if (input$var_source == "create") {
        # ---------- CAS 1 : nouvelle variable ----------
        req(rv$new_vec)

        df_new <- data.frame(rv$new_vec)
        colnames(df_new) <- rv$var_name

        pred <- m$predict_new(df_new, mode = "flex")

        # Donner un nom de ligne propre (sinon rownames = NULL)
        if (!is.null(pred$soft_membership) && nrow(pred$soft_membership) == 1L) {
          rownames(pred$soft_membership) <- rv$var_name
        }
        if (!is.null(pred$embeddings) && nrow(pred$embeddings) == 1L) {
          rownames(pred$embeddings) <- rv$var_name
        }

        rv$pred     <- pred
        rv$var_type <- "new"

      } else {
        # ---------- CAS 2 : variable illustrative ----------
        req(input$illu_var_predict)
        vname <- input$illu_var_predict

        cat("Variable illustrative choisie :", vname, "\n")

        v <- data()[[vname]]

        if (is.numeric(v)) {

          raw_emb <- illu_emb()[[vname]]

          cat("\n===== DEBUG EMBEDDING ILLUSTRATIVE =====\n")
          print(raw_emb)
          cat("class:", class(raw_emb), "\n")
          cat("typeof:", typeof(raw_emb), "\n")
          cat("==========================================\n\n")

          # ---- NORMALISATION EMBEDDING ----
          emb <- NULL

          # cas 1 : déjà vecteur
          if (is.numeric(raw_emb) && is.null(dim(raw_emb))) {
            emb <- raw_emb
          }

          # cas 2 : matrice 1 × latent_dim
          else if (is.matrix(raw_emb)) {
            if (nrow(raw_emb) != 1)
              stop("❌ embedding matrice avec plusieurs lignes pour ", vname)
            emb <- as.numeric(raw_emb[1, ])
          }

          # cas 3 : data.frame 1 ligne
          else if (is.data.frame(raw_emb)) {
            if (nrow(raw_emb) != 1)
              stop("❌ embedding data.frame avec plusieurs lignes pour ", vname)
            emb <- as.numeric(raw_emb[1, ])
          }

          # cas 4 : list → essayer d’extraire quelque chose dedans
          else if (is.list(raw_emb)) {
            # prendre le premier élément numérique/matrice
            candidate <- NULL
            for (x in raw_emb) {
              if (is.numeric(x) || is.matrix(x)) {candidate <- x; break}
            }
            if (is.null(candidate))
              stop("❌ embedding illustratif pour ", vname, " ne contient rien d'utilisable")
            if (is.matrix(candidate)) {
              emb <- as.numeric(candidate[1, ])
            } else {
              emb <- as.numeric(candidate)
            }
          }

          # impossible à extraire
          else {
            stop("❌ embedding illustratif inconnu pour ", vname)
          }

          # ---- VALIDATION FINALE ----
          if (!is.numeric(emb) || any(!is.finite(emb))) {
            stop("❌ embedding final non numérique ou NaN pour ", vname)
          }

          if (length(emb) != ncol(m$cluster_centers)) {
            stop(
              "❌ Dimension mismatch : embedding=", length(emb),
              " centers=", ncol(m$cluster_centers),
              " (variable=", vname, ")"
            )
          }

          cat("===== EMBEDDING FINAL =====\n")
          print(emb)
          cat("latent_dim:", length(emb), "\n")
          cat("======================================\n\n")

          # ---- COMPUTE DISTANCES ----
          centers <- m$cluster_centers
          dists <- apply(centers, 1, function(c) sum((c - emb)^2))

          soft <- exp(-dists)
          soft <- soft / sum(soft)

          pred <- list(
            embeddings = matrix(emb, nrow = 1,
                                dimnames = list(vname, paste0("Dim", seq_along(emb)))),
            clusters = setNames(which.min(dists), vname),
            soft_membership = matrix(soft, nrow = 1,
                                     dimnames = list(vname, paste0("Cluster", seq_along(soft))))
          )

          rv$pred     <- pred
          rv$var_name <- vname
          rv$var_type <- "illustrative"
          return()
        }else {
          # ----- CAS FACTOR -----
          f   <- factor(v)
          lev <- levels(f)

          Xnew <- sapply(lev, function(l) as.numeric(f == l))
          colnames(Xnew) <- paste0(vname, "_", lev)
          Xnew <- as.data.frame(Xnew)

          pred <- m$predict_new(Xnew, mode = "flex")

          if (!is.null(pred$soft_membership)) {
            rownames(pred$soft_membership) <- lev
          }
          if (!is.null(pred$embeddings)) {
            rownames(pred$embeddings) <- lev
          }

          rv$pred     <- pred
          rv$var_name <- vname
          rv$var_type <- "illustrative"
        }
      }   # <-- FIN DU else GLOBAL


      # DEBUG (tu peux les enlever après)
      print("===== DEBUG: pred =====")
      print(str(rv$pred))
      print("soft =")
      print(rv$pred$soft_membership)

      showNotification("Projection terminée ✔", type = "message")
    })

    # ============================================================
    # RÉSUMÉ
    # ============================================================
    output$summary <- renderPrint({
      req(rv$pred)

      cat("Variable :", rv$var_name, "\n")
      cat("Type     :", rv$var_type, "\n\n")

      # -----------------------------
      # CLUSTERS
      # -----------------------------
      cl <- rv$pred$clusters
      if (length(cl) > 1) {
        cat("Clusters (par niveau) :\n")
        print(cl)
      } else {
        cat("Cluster :", cl, "\n")
      }
      cat("\n")

      # -----------------------------
      # EMBEDDINGS UTILISÉS
      # -----------------------------
      emb <- rv$pred$embeddings
      if (!is.null(emb)) {
        cat("Embeddings utilisés (projection dans l’espace latent) :\n")
        print(round(emb, 4))
      } else {
        cat("⚠ Aucun embedding disponible.\n")
      }
      cat("\n")

      # -----------------------------
      # SOFT MEMBERSHIP
      # -----------------------------
      S <- rv$pred$soft_membership

      if (!is.null(S)) {
        cat("Soft membership (probabilités d'appartenance aux clusters) :\n")
        print(round(S, 4))
      } else {
        cat("Soft membership non disponible.\n")
      }
    })


    # ============================================================
    # TABLEAU SOFT MEMBERSHIP
    # ============================================================
    output$soft_table <- renderTable({
      req(rv$pred)
      S <- rv$pred$soft_membership
      if (is.null(S)) return(NULL)

      S <- as.matrix(S)
      nK <- ncol(S)

      level_names <- rownames(S)
      if (is.null(level_names)) {
        # Aucun rowname → on en crée
        if (!is.null(rv$var_name) && nrow(S) == 1L) {
          level_names <- rv$var_name
        } else {
          level_names <- paste0("Item_", seq_len(nrow(S)))
        }
      }

      df <- data.frame(Level = level_names, round(S, 4), check.names = FALSE)
      colnames(df)[-1] <- paste0("Cluster_", seq_len(nK))
      df
    }, rownames = FALSE)

    # ============================================================
    # PLOT 2D
    # ============================================================
    output$plot_2d <- renderPlotly({
      req(model(), rv$pred)

      emb     <- model()$embeddings
      cl      <- clusters()
      new_emb <- rv$pred$embeddings

      # PCA
      if (input$use_pca2 && ncol(emb) > 2) {
        pca        <- prcomp(emb)
        coords     <- pca$x[, 1:2, drop = FALSE]
        new_coords <- predict(pca, newdata = new_emb)[, 1:2, drop = FALSE]
      } else {
        coords     <- emb[, 1:2, drop = FALSE]
        new_coords <- new_emb[, 1:2, drop = FALSE]
      }

      base_colors <- scales::hue_pal()(model()$n_clusters)

      # --- Variables actives (fond) ---
      p <- plot_ly(
        x     = coords[, 1],
        y     = coords[, 2],
        type  = "scatter",
        mode  = "markers",
        marker = list(size = 8),
        text   = rownames(emb),
        color  = factor(cl, levels = 1:model()$n_clusters),
        colors = base_colors,
        name   = "Variables actives"
      )

      # --- Variable/Niveaux projetés (un marqueur noir) ---
      p <- p %>% add_trace(
        x     = new_coords[, 1],
        y     = new_coords[, 2],
        type  = "scatter",
        mode  = "markers+text",
        marker = list(size = 14, color = "black", symbol = "diamond"),
        text   = rownames(new_coords),
        textposition = "top center",
        name   = rv$var_name,
        inherit = FALSE    # très important pour ne pas récupérer color = cl
      )

      p %>% layout(title = "Projection 2D")
    })

    # ============================================================
    # PLOT 3D
    # ============================================================
    output$plot_3d <- renderPlotly({
      req(model(), rv$pred)

      emb     <- model()$embeddings
      cl      <- clusters()
      new_emb <- rv$pred$embeddings

      if (input$use_pca2 && ncol(emb) > 3) {
        pca        <- prcomp(emb)
        coords     <- pca$x[, 1:3, drop = FALSE]
        new_coords <- predict(pca, newdata = new_emb)[, 1:3, drop = FALSE]
      } else {
        coords     <- emb[, 1:3, drop = FALSE]
        new_coords <- new_emb[, 1:3, drop = FALSE]
      }

      base_colors <- scales::hue_pal()(model()$n_clusters)

      # Construire les hovertexts AVANT plot_ly (pas de formule ~)
      hover_active <- paste0(
        "<b>", rownames(emb), "</b><br>",
        "Cluster: ", cl, "<br>",
        "Dim1: ", round(coords[, 1], 3), "<br>",
        "Dim2: ", round(coords[, 2], 3), "<br>",
        "Dim3: ", round(coords[, 3], 3)
      )

      hover_new <- paste0(
        "<b>", rownames(new_coords), "</b><br>",
        "Variable: ", rv$var_name, "<br>",
        "Cluster: ", rv$pred$cluster, "<br>",
        "Dim1: ", round(new_coords[, 1], 3), "<br>",
        "Dim2: ", round(new_coords[, 2], 3), "<br>",
        "Dim3: ", round(new_coords[, 3], 3)
      )

      # --- Variables actives ---
      p <- plot_ly() %>%
        add_trace(
          x    = coords[, 1],
          y    = coords[, 2],
          z    = coords[, 3],
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 6),
          color  = factor(cl, levels = 1:model()$n_clusters),
          colors = base_colors,
          name   = "Variables actives",
          text = hover_active,
          hoverinfo = "text"
        )

      # --- Variable/Niveaux projetés ---
      p <- p %>% add_trace(
        x    = new_coords[, 1],
        y    = new_coords[, 2],
        z    = new_coords[, 3],
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = 12,
          color = "red",
          symbol = "diamond",
          line = list(color = "black", width = 2)
        ),
        name   = rv$var_name,
        text = hover_new,
        hoverinfo = "text",
        showlegend = TRUE
      )

      p %>% layout(
        title = "Projection 3D (Deep Embeddings)",
        scene = list(
          xaxis = list(title = "Dim 1"),
          yaxis = list(title = "Dim 2"),
          zaxis = list(title = "Dim 3"),
          camera = list(
            eye = list(x = 1.5, y = 1.5, z = 1.5)
          )
        ),
        dragmode = "orbit",
        hovermode = "closest"
      )
    })

  })
}
