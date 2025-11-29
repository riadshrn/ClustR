# ==========================================================
# MODULE : deep_visualization  (VERSION PRO - FINAL CORRECTED)
# - PCA partout où nécessaire
# - Contrôle utilisateur sur nombre de dimensions affichées
# - Section de résultats détaillés (collapsible)
# - Visualisations Deep 2D/3D avec PCA
# - Top variables proches avec PCA
# - Support MULTI illustratives
# - Interprétation LLM (Mistral)
# ==========================================================

deep_visualization_ui <- function(id){
  ns <- NS(id)

  tagList(

    # ---------- CSS ----------
    tags$style(HTML("
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

      .interpret-box h4 {
         color: #3949ab;
         font-size: 18px;
         font-weight: 600;
         margin-top: 18px;
         margin-bottom: 8px;
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

      .interpret-box tbody tr:last-child td {
         border-bottom: none;
      }

      .interpret-box strong {
         color: #1a237e;
         font-weight: 600;
      }

      .interpret-box ul, .interpret-box ol {
         margin: 15px 0;
         padding-left: 25px;
      }

      .interpret-box li {
         margin: 8px 0;
         line-height: 1.6;
      }

      .interpret-box p {
         margin: 12px 0;
         text-align: justify;
      }

      .interpret-box code {
         background: #f0f2f8;
         padding: 2px 6px;
         border-radius: 3px;
         font-family: 'Courier New', monospace;
         font-size: 14px;
         color: #c7254e;
      }

      .interpret-box pre {
         background: #f8f9fa;
         padding: 15px;
         border-radius: 6px;
         border-left: 4px solid #3f51b5;
         overflow-x: auto;
         margin: 15px 0;
      }

      .interpret-box hr {
         border: none;
         border-top: 2px solid #e0e0e0;
         margin: 25px 0;
      }

      .prompt-box {
        background: #0b1020;
        color: #e6e6e6;
        padding: 16px;
        border-radius: 8px;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
        font-size: 13px;
        white-space: pre-wrap;
        max-height: 400px;
        overflow-y: auto;
        border: 1px solid #333;
        margin-top: 12px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.3);
      }

      .spinner-line {
        display: flex;
        align-items: center;
        gap: 12px;
        font-weight: 600;
        color: #2c3e50;
        font-size: 18px;
      }

      .spinner-line i {
        color: #3f51b5;
      }

      .before-after-info {
        background: #e8f4f8;
        border-left: 4px solid #2196F3;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }

      .before-after-info h4 {
        margin-top: 0;
        color: #1976D2;
      }

      .results-table {
        width: 100%;
        border-collapse: collapse;
        margin: 15px 0;
        background: white;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }

      .results-table thead {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
      }

      .results-table th {
        padding: 12px;
        text-align: left;
        font-weight: 600;
      }

      .results-table td {
        padding: 10px 12px;
        border-bottom: 1px solid #e0e0e0;
      }

      .results-table tbody tr:hover {
        background-color: #f8f9fa;
      }

      .metric-badge {
        display: inline-block;
        padding: 4px 10px;
        border-radius: 12px;
        font-size: 13px;
        font-weight: 600;
      }

      .metric-good {
        background: #d4edda;
        color: #155724;
      }

      .metric-medium {
        background: #fff3cd;
        color: #856404;
      }

      .metric-bad {
        background: #f8d7da;
        color: #721c24;
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

    fluidRow(
      # ---------- LEFT : options ----------
      box(
        width=3, status="primary", solidHeader=TRUE, title="Options",

        selectInput(
          ns("plot_type"), "Type de plot :",
          choices = c(
            "⚖️ AVANT/APRÈS Clustering" = "before_after",
            "Embeddings 2D – Animation PCA" = "emb2d_pca_anim",
            "Embeddings 2D interactif" = "emb2d_plotly",
            "Embeddings 2D (statique)" = "emb2d_static",
            "Embeddings 3D interactif" = "emb3d_plotly",
            "Reconstruction error"     = "recon",
            "Importance variables"     = "imp"
          ),
          selected = "before_after"
        ),

        hr(),
        uiOutput(ns("illu_show_ui")),
        hr(),

        # Option PCA pour emb2d_static uniquement
        conditionalPanel(
          condition = sprintf("input['%s'] == 'emb2d_static'", ns("plot_type")),
          checkboxInput(
            ns("use_pca_2d"),
            "🔍 Réduction de dim avec PCA",
            value = FALSE
          ),
          div(
            class = "text-muted",
            style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
            "Active PCA si latent_dim > 2"
          )
        ),

        hr(),

        numericInput(ns("plot_width"),  "Largeur (px)", 1200, min=400, max=1800, step=100),
        numericInput(ns("plot_height"), "Hauteur (px)", 550, min=300, max=1200, step=50),

        downloadButton(ns("download_plot"), "Télécharger plot",
                       class="btn-sm btn-info", style="width:100%;")
      ),

      # ---------- RIGHT : visualization ----------
      box(
        width=9, status="info", solidHeader=TRUE, title="Visualisation",
        uiOutput(ns("algo_check")),
        uiOutput(ns("plot_ui"))
      )
    ),

    # ========================================================
    # NOUVELLE SECTION : RÉSULTATS DÉTAILLÉS (COLLAPSIBLE)
    # ========================================================
    fluidRow(
      box(
        width=12, status="success", solidHeader=TRUE,
        title="📊 Résultats Détaillés du Clustering",
        collapsible=TRUE, collapsed=FALSE,

        tabsetPanel(
          id = ns("results_tabs"),

          # --- TAB 1: Métriques de Qualité ---
          tabPanel(
            "Métriques de Qualité",
            br(),
            uiOutput(ns("quality_metrics_ui"))
          ),

          # --- TAB 2: Composition des Clusters ---
          tabPanel(
            "Composition des Clusters",
            br(),
            # ⭐ NOUVEAU : Contrôle nombre de dimensions
            fluidRow(
              column(
                4,
                numericInput(
                  ns("n_dims_composition"),
                  "Nombre de dimensions à afficher :",
                  value = 5,
                  min = 2,
                  max = 20,
                  step = 1
                )
              )
            ),
            DTOutput(ns("cluster_composition_table"))
          ),

          # --- TAB 3: Dimensions et Embeddings ---
          tabPanel(
            "Dimensions & Embeddings",
            br(),
            uiOutput(ns("dimensions_info_ui")),
            br(),
            h4("📋 Aperçu des Embeddings (5 premières variables)"),
            DTOutput(ns("embeddings_preview_table"))
          ),

          # --- TAB 4: Variables Illustratives ---
          tabPanel(
            "Variables Illustratives",
            br(),
            uiOutput(ns("illustratives_summary_ui"))
          ),

          # --- TAB 5: Modalités par Variable ---
          tabPanel(
            "Modalités des Illustratives",
            br(),

            # ⭐ NOUVEAU : Sélecteur de modalités
            uiOutput(ns("modality_selector_container")),

            uiOutput(ns("modalities_ui"))
          )
        )
      )
    ),

    # ========================================================
    # TOP VARIABLES PROCHES (AMÉLIORÉ)
    # ========================================================
    fluidRow(
      box(
        width=12, status="warning", solidHeader=TRUE,
        title="🎯 Variables les Plus Proches d'une Illustrative",
        collapsible=TRUE,

        fluidRow(
          column(
            6,
            selectInput(
              ns("nearest_var_type"),
              "Type d'illustrative :",
              choices = c("Catégorielle" = "factor", "Numérique" = "numeric")
            )
          ),
          column(
            6,
            numericInput(
              ns("nearest_k"),
              "Nombre de variables à afficher :",
              value = 10,
              min = 3,
              max = 20,
              step = 1
            )
          )
        ),

        uiOutput(ns("nearest_controls_ui")),

        hr(),

        actionButton(
          ns("compute_nearest"),
          "🔍 Calculer les Plus Proches",
          class = "btn btn-primary btn-lg",
          icon = icon("search"),
          style = "width: 100%;"
        ),

        br(), br(),

        uiOutput(ns("nearest_results_ui"))
      )
    ),

    # ========================================================
    # INTERPRÉTATION AUTOMATIQUE
    # ========================================================
    fluidRow(
      box(
        width = 12, status="primary", solidHeader=TRUE,
        title="🤖 Interprétation Automatique (IA)",

        fluidRow(
          column(
            6,
            actionButton(ns("btn_interpret"), "✨ Générer l'Interprétation",
                         class="btn btn-success btn-lg", icon=icon("magic"),
                         style="width:100%;")
          ),
          column(
            6,
            actionButton(ns("btn_show_prompt"), "👁️ Voir le prompt (debug)",
                         class="btn btn-default", icon=icon("eye"),
                         style="width:100%;")
          )
        ),

        uiOutput(ns("prompt_ui")),
        div(class="interpret-box", uiOutput(ns("interpretation")))
      )
    )
  )
}


# ==========================================================
# SERVER
# ==========================================================
deep_visualization_server <- function(id,
                                      model,
                                      data,
                                      clusters,
                                      method,
                                      illustrative_names,
                                      illustrative_labels = reactive(NULL),
                                      deep_quality = reactive(NULL)){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    rv <- reactiveValues(
      illu_proj = NULL,
      ready = FALSE,
      last_prompt = NULL,
      show_prompt = FALSE,
      nearest_results = NULL
    )

    is_deep <- reactive({
      req(method())
      method() == "deep"
    })

    # ---------------- Guard UI ----------------
    output$algo_check <- renderUI({
      if(!is_deep() || is.null(model())){
        div(
          class="alert alert-warning",
          h4(icon("exclamation-triangle"), " Visualisation Deep uniquement"),
          p("Veuillez lancer Deep Clustering pour activer ces visualisations.")
        )
      }
    })

    # ========================================================
    # Project illustratives after fit / selection
    # ========================================================
    observeEvent(list(model(), illustrative_names(), data()), {

      req(is_deep(), model(), data())
      illu <- illustrative_names()

      if(is.null(illu) || length(illu)==0){
        rv$illu_proj <- NULL
        rv$ready <- TRUE
        return()
      }

      df <- data()
      illu_list <- list()

      for(vn in illu){
        if(!vn %in% names(df)) next
        v <- df[[vn]]

        # factor-like?
        if(is.factor(v) || (is.numeric(v) && length(unique(v)) <= 10)){

          v_fac <- factor(v)

          labs_all <- illustrative_labels()
          if(!is.null(labs_all) && !is.null(labs_all[[vn]])){
            labs_map <- labs_all[[vn]][levels(v_fac)]
          } else {
            labs_map <- levels(v_fac)
          }

          res <- tryCatch({
            model()$project_illustrative(v_fac, labels=unname(labs_map))
          }, error=function(e) NULL)
          if(is.null(res)) next

          emb_res <- if(is.matrix(res)) res else res$embeddings

          illu_list[[vn]] <- list(
            type="factor",
            embeddings=emb_res,
            clusters=rep(1, nrow(emb_res)),
            labels=rownames(emb_res),
            original_levels = levels(v_fac)
          )

        } else {

          Xillu <- data.frame(tmp=as.numeric(v))
          colnames(Xillu) <- vn

          res <- tryCatch({
            model()$project_illustrative(Xillu)
          }, error=function(e) NULL)
          if(is.null(res)) next

          if(is.atomic(res)){
            emb <- matrix(res, nrow=1)
            rownames(emb) <- vn
            clusters_ <- 1
            labels_ <- vn
          } else {
            emb <- res$embeddings
            clusters_ <- res$clusters
            labels_ <- rownames(res$embeddings)
          }

          illu_list[[vn]] <- list(
            type="numeric",
            embeddings=emb,
            clusters=clusters_,
            labels=labels_
          )
        }
      }

      rv$illu_proj <- if(length(illu_list)>0) illu_list else NULL
      rv$ready <- TRUE
    })


    # ========================================================
    # UI: choose illustratives to show
    # ========================================================
    output$illu_show_ui <- renderUI({
      req(rv$ready)
      if(is.null(rv$illu_proj)){
        return(div(class="text-muted","Aucune illustrative sélectionnée."))
      }
      choices <- names(rv$illu_proj)
      checkboxGroupInput(
        ns("illu_show"),
        "Afficher illustratives :",
        choices=choices, selected=choices
      )
    })

    # ========================================================
    # Plot UI switch
    # ========================================================
    output$plot_ui <- renderUI({
      if(!is_deep() || is.null(model())) return(NULL)

      if(input$plot_type %in% c("before_after","emb2d_plotly","emb3d_plotly","emb2d_pca_anim")){
        plotlyOutput(ns("plotly_plot"),
                     width=paste0(input$plot_width,"px"),
                     height=paste0(input$plot_height,"px"))
      } else {
        plotOutput(ns("static_plot"),
                   width=paste0(input$plot_width,"px"),
                   height=paste0(input$plot_height,"px"))
      }
    })


    # ========================================================
    # RÉSULTATS DÉTAILLÉS - TAB 1: Métriques de Qualité
    # ========================================================
    output$quality_metrics_ui <- renderUI({
      req(model(), deep_quality())

      qual <- deep_quality()

      # Fonction pour formater les métriques avec badge coloré
      format_metric <- function(value, good_threshold, bad_threshold, higher_better = TRUE) {
        if(is.null(value) || is.na(value)) return(span(class="metric-badge metric-medium", "N/A"))

        value_rounded <- round(value, 3)

        if(higher_better) {
          if(value >= good_threshold) {
            badge_class <- "metric-good"
          } else if(value >= bad_threshold) {
            badge_class <- "metric-medium"
          } else {
            badge_class <- "metric-bad"
          }
        } else {
          if(value <= good_threshold) {
            badge_class <- "metric-good"
          } else if(value <= bad_threshold) {
            badge_class <- "metric-medium"
          } else {
            badge_class <- "metric-bad"
          }
        }

        span(class=paste("metric-badge", badge_class), value_rounded)
      }

      tagList(
        div(class="section-card",
            h4("📈 Métriques de Séparation des Clusters"),
            tags$table(class="results-table",
                       tags$thead(
                         tags$tr(
                           tags$th("Métrique"),
                           tags$th("Valeur"),
                           tags$th("Interprétation"),
                           tags$th("Qualité")
                         )
                       ),
                       tags$tbody(
                         tags$tr(
                           tags$td(strong("Silhouette Moyenne")),
                           tags$td(format_metric(qual$sil_mean, 0.5, 0.25, TRUE)),
                           tags$td("Mesure la cohésion et la séparation des clusters"),
                           tags$td(if(!is.null(qual$sil_mean) && qual$sil_mean >= 0.5) "✅ Excellente"
                                   else if(!is.null(qual$sil_mean) && qual$sil_mean >= 0.25) "⚠️ Acceptable"
                                   else "❌ Faible")
                         ),
                         tags$tr(
                           tags$td(strong("Dunn Index")),
                           tags$td(format_metric(qual$dunn, 0.3, 0.1, TRUE)),
                           tags$td("Ratio distance inter/intra clusters (plus élevé = mieux)"),
                           tags$td(if(!is.null(qual$dunn) && qual$dunn >= 0.3) "✅ Bon"
                                   else if(!is.null(qual$dunn) && qual$dunn >= 0.1) "⚠️ Moyen"
                                   else "❌ Faible")
                         ),
                         tags$tr(
                           tags$td(strong("Davies-Bouldin Index")),
                           tags$td(format_metric(qual$dbi, 1.0, 2.0, FALSE)),
                           tags$td("Mesure de similarité inter-clusters (plus bas = mieux)"),
                           tags$td(if(!is.null(qual$dbi) && qual$dbi <= 1.0) "✅ Bon"
                                   else if(!is.null(qual$dbi) && qual$dbi <= 2.0) "⚠️ Moyen"
                                   else "❌ Élevé")
                         )
                       )
            )
        ),

        div(class="section-card",
            h4("🎯 Score Global de Qualité"),
            div(
              style="text-align: center; padding: 20px;",
              div(
                style="font-size: 48px; font-weight: bold; color: #667eea;",
                if(!is.null(qual$quality)) paste0(round(qual$quality * 100, 1), "%") else "N/A"
              ),
              div(
                style="font-size: 18px; color: #666; margin-top: 10px;",
                if(!is.null(qual$quality) && qual$quality >= 0.7) "🌟 Clustering de très bonne qualité"
                else if(!is.null(qual$quality) && qual$quality >= 0.5) "👍 Clustering de qualité acceptable"
                else if(!is.null(qual$quality)) "⚠️ Clustering à améliorer"
                else "Données non disponibles"
              )
            )
        ),

        div(class="section-card",
            h4("🔧 Paramètres du Modèle"),
            tags$table(class="results-table",
                       tags$thead(
                         tags$tr(
                           tags$th("Paramètre"),
                           tags$th("Valeur")
                         )
                       ),
                       tags$tbody(
                         tags$tr(
                           tags$td("Nombre de clusters"),
                           tags$td(strong(model()$n_clusters))
                         ),
                         tags$tr(
                           tags$td("Dimension latente"),
                           tags$td(strong(model()$latent_dim))
                         ),
                         tags$tr(
                           tags$td("Erreur de reconstruction (MSE)"),
                           tags$td(if(!is.null(model()$recon_mse)) round(model()$recon_mse, 6) else "N/A")
                         ),
                         tags$tr(
                           tags$td("Nombre de variables actives"),
                           tags$td(strong(nrow(model()$embeddings)))
                         )
                       )
            )
        )
      )
    })


    # ========================================================
    # RÉSULTATS DÉTAILLÉS - TAB 2: Composition des Clusters
    # ⭐ CORRIGÉ : Afficher TOUTES les dimensions avec contrôle utilisateur
    # ========================================================
    output$cluster_composition_table <- renderDT({
      req(model(), clusters())

      m <- model()
      cl <- clusters()
      emb <- m$embeddings

      # ⭐ NOUVEAU : Utiliser le nombre de dimensions choisi par l'utilisateur
      n_dims_show <- min(input$n_dims_composition, ncol(emb))

      # Créer un dataframe avec toutes les variables et leur cluster
      df_comp <- data.frame(
        Variable = names(cl),
        Cluster = as.integer(cl),
        stringsAsFactors = FALSE
      )

      # Ajouter les embeddings (toutes ou n premières dimensions)
      for(i in 1:n_dims_show) {
        df_comp[[paste0("Dim", i)]] <- round(emb[, i], 4)
      }

      # Calculer la distance au centre du cluster
      centers <- by(emb, cl, colMeans) |> do.call(rbind, args=_)

      df_comp$Distance_Centre <- sapply(seq_len(nrow(emb)), function(i) {
        k <- cl[i]
        round(sqrt(sum((emb[i,] - centers[k,])^2)), 4)
      })

      # ⭐ CORRIGÉ : Utiliser |> au lieu de %>%
      dt <- datatable(
        df_comp,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(1, 'asc'), list(ncol(df_comp) - 1, 'asc')),  # Tri par cluster puis distance
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        filter = 'top',
        caption = sprintf("Composition détaillée des clusters (%d dimensions affichées sur %d)",
                          n_dims_show, ncol(emb))
      )

      dt <- dt |>
        formatStyle(
          'Cluster',
          backgroundColor = styleEqual(
            sort(unique(cl)),
            rainbow(length(unique(cl)), alpha = 0.3)
          )
        )

      dt <- dt |>
        formatStyle(
          'Distance_Centre',
          background = styleColorBar(range(df_comp$Distance_Centre), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )

      dt
    })


    # ========================================================
    # RÉSULTATS DÉTAILLÉS - TAB 3: Dimensions & Embeddings
    # ========================================================
    output$dimensions_info_ui <- renderUI({
      req(model())

      m <- model()
      emb <- m$embeddings

      # Calculer la variance expliquée par chaque dimension
      var_per_dim <- apply(emb, 2, var)
      total_var <- sum(var_per_dim)
      var_explained <- var_per_dim / total_var * 100

      tagList(
        div(class="section-card",
            h4("📊 Variance Expliquée par Dimension"),
            plotOutput(ns("variance_plot"), height = "300px")
        ),

        div(class="section-card",
            h4("📉 Statistiques des Dimensions"),
            tags$table(class="results-table",
                       tags$thead(
                         tags$tr(
                           tags$th("Dimension"),
                           tags$th("Variance"),
                           tags$th("% Variance Expliquée"),
                           tags$th("% Cumulée")
                         )
                       ),
                       tags$tbody(
                         lapply(1:ncol(emb), function(i) {
                           tags$tr(
                             tags$td(paste0("Dim ", i)),
                             tags$td(round(var_per_dim[i], 4)),
                             tags$td(paste0(round(var_explained[i], 2), "%")),
                             tags$td(paste0(round(sum(var_explained[1:i]), 2), "%"))
                           )
                         })
                       )
            )
        )
      )
    })

    output$variance_plot <- renderPlot({
      req(model())

      emb <- model()$embeddings
      var_per_dim <- apply(emb, 2, var)
      total_var <- sum(var_per_dim)
      var_explained <- var_per_dim / total_var * 100

      par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

      # Barplot variance par dimension
      barplot(
        var_explained,
        names.arg = paste0("Dim", 1:length(var_explained)),
        col = colorRampPalette(c("#667eea", "#764ba2"))(length(var_explained)),
        main = "Variance Expliquée par Dimension",
        ylab = "% Variance",
        xlab = "Dimension",
        las = 2
      )
      grid()

      # Scree plot (variance cumulée)
      plot(
        1:length(var_explained),
        cumsum(var_explained),
        type = "b",
        pch = 19,
        col = "#667eea",
        lwd = 2,
        main = "Variance Cumulée",
        xlab = "Nombre de Dimensions",
        ylab = "% Variance Cumulée",
        ylim = c(0, 100)
      )
      abline(h = c(80, 90), lty = 2, col = c("orange", "red"))
      text(length(var_explained) * 0.7, 85, "80%", col = "orange")
      text(length(var_explained) * 0.7, 95, "90%", col = "red")
      grid()
    })

    output$embeddings_preview_table <- renderDT({
      req(model())

      emb <- model()$embeddings
      cl <- model()$clusters

      # Prendre les 5 premières variables
      n_show <- min(5, nrow(emb))
      emb_show <- emb[1:n_show, ]

      df_preview <- data.frame(
        Variable = rownames(emb_show),
        Cluster = cl[1:n_show],
        stringsAsFactors = FALSE
      )

      # Ajouter toutes les dimensions
      for(i in 1:ncol(emb_show)) {
        df_preview[[paste0("Dim", i)]] <- round(emb_show[, i], 4)
      }

      datatable(
        df_preview,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE
      )
    })


    # ========================================================
    # RÉSULTATS DÉTAILLÉS - TAB 4: Variables Illustratives
    # ⭐ CORRIGÉ : Utiliser PCA pour projection 2D
    # ========================================================
    output$illustratives_summary_ui <- renderUI({
      req(rv$ready)

      if(is.null(rv$illu_proj)) {
        return(div(
          class="alert alert-info",
          h4(icon("info-circle"), " Aucune variable illustrative"),
          p("Vous n'avez pas sélectionné de variables illustratives pour ce clustering.")
        ))
      }

      illu_proj <- rv$illu_proj

      tagList(
        div(class="section-card",
            h4("📋 Résumé des Variables Illustratives"),
            tags$table(class="results-table",
                       tags$thead(
                         tags$tr(
                           tags$th("Variable"),
                           tags$th("Type"),
                           tags$th("Nombre de Niveaux/Points"),
                           tags$th("Dimensions Embeddings")
                         )
                       ),
                       tags$tbody(
                         lapply(names(illu_proj), function(vn) {
                           obj <- illu_proj[[vn]]
                           tags$tr(
                             tags$td(strong(vn)),
                             tags$td(
                               if(obj$type == "factor")
                                 span(class="badge badge-info", "Catégorielle")
                               else
                                 span(class="badge badge-success", "Numérique")
                             ),
                             tags$td(nrow(obj$embeddings)),
                             tags$td(paste(dim(obj$embeddings), collapse = " × "))
                           )
                         })
                       )
            )
        ),

        div(class="section-card",
            h4("🎨 Aperçu des Embeddings Illustratifs"),
            lapply(names(illu_proj), function(vn) {
              obj <- illu_proj[[vn]]

              tagList(
                h5(paste("Variable:", vn)),
                if(obj$type == "factor") {
                  div(
                    p(strong("Modalités:"), paste(obj$labels, collapse = ", ")),
                    renderPlot({
                      # ⭐ CORRIGÉ : Utiliser PCA si > 2 dimensions
                      emb_illu <- obj$embeddings

                      if(ncol(emb_illu) > 2) {
                        pca <- prcomp(emb_illu, center = TRUE, scale. = FALSE)
                        emb_2d <- pca$x[, 1:2]
                        var_exp <- summary(pca)$importance[2, 1:2] * 100
                        xlab <- sprintf("PC1 (%.1f%%)", var_exp[1])
                        ylab <- sprintf("PC2 (%.1f%%)", var_exp[2])
                        title <- sprintf("Projection PCA 2D de %s\n(Total: %.1f%%)",
                                         vn, sum(var_exp))
                      } else {
                        emb_2d <- emb_illu[, 1:2]
                        xlab <- "Dim 1"
                        ylab <- "Dim 2"
                        title <- paste("Projection 2D de", vn)
                      }

                      plot(
                        emb_2d,
                        pch = 19,
                        col = rainbow(nrow(emb_2d)),
                        cex = 2,
                        main = title,
                        xlab = xlab,
                        ylab = ylab
                      )
                      text(emb_2d, labels = obj$labels, pos = 3, cex = 0.8)
                      grid()
                    }, height = 300)
                  )
                } else {
                  div(
                    p(strong("Coordonnées:"), paste(round(obj$embeddings[1, ], 4), collapse = ", "))
                  )
                },
                hr()
              )
            })
        )
      )
    })


    # ========================================================
    # RÉSULTATS DÉTAILLÉS - TAB 5: Modalités des Illustratives
    # ⭐ CORRIGÉ : Afficher TOUTES les dimensions + PCA pour plot 2D
    # ========================================================
    # ========================================================
    # SÉLECTEUR DE MODALITÉS - CONTAINER COMPLET
    # ========================================================

    output$modality_selector_container <- renderUI({
      req(rv$ready, rv$illu_proj)

      illu_proj <- rv$illu_proj
      factor_vars <- names(illu_proj)[sapply(illu_proj, function(x) x$type == "factor")]

      # Si pas de variables factor, ne rien afficher
      if(length(factor_vars) == 0) {
        return(NULL)
      }

      # Construire la liste de toutes les modalités
      # FIX ULTIME : Shiny a un bug avec checkboxGroupInput qui retourne
      # les LABELS au lieu des VALUES. Solution : rendre labels = IDs

      all_modalities <- c()
      for(vn in factor_vars) {
        obj <- illu_proj[[vn]]
        for(lbl in obj$labels) {
          mod_id <- paste0(vn, "___", lbl)  # L'ID unique
          # ASTUCE : Utiliser l'ID comme label aussi pour éviter le bug Shiny
          all_modalities[mod_id] <- mod_id  # ID = label
        }
      }

      # DEBUG : Vérifier le format
      cat("\n=== DEBUG SELECTOR ===\n")
      cat("Type:", class(all_modalities), "\n")
      cat("Premiers IDs (names):", paste(head(names(all_modalities), 3), collapse = " | "), "\n")
      cat("Premiers labels (values):", paste(head(all_modalities, 3), collapse = " | "), "\n")
      cat("IDs == labels?", identical(names(all_modalities), as.character(all_modalities)), "\n")
      cat("======================\n\n")

      # Sélection initiale (top 10)
      if(is.null(input$selected_mods)) {
        default_selection <- names(all_modalities)[1:min(10, length(all_modalities))]
      } else {
        default_selection <- input$selected_mods
      }

      # Nombre de modalités sélectionnées
      n_selected <- length(default_selection)

      div(
        style = "background: #f0f4f8; border: 2px solid #667eea; border-radius: 8px; padding: 15px; margin-bottom: 20px;",

        h4(
          style = "margin-top: 0; color: #667eea;",
          icon("filter"),
          " Filtrer les modalités à afficher ",
          span(
            style = "background: #667eea; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px; margin-left: 10px;",
            paste(n_selected, "modalités")
          )
        ),

        # Container scrollable pour les checkboxes
        div(
          style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: white; border-radius: 4px;",
          checkboxGroupInput(
            ns("selected_mods"),
            NULL,
            choices = all_modalities,
            selected = default_selection
          )
        ),

        hr(),

        fluidRow(
          column(4, actionButton(ns("select_all_mod"), "Tout sélectionner", icon = icon("check-square"), class = "btn-sm btn-primary")),
          column(4, actionButton(ns("deselect_all_mod"), "Tout désélectionner", icon = icon("square"), class = "btn-sm btn-default")),
          column(4, actionButton(ns("select_top10_mod"), "Top 10", icon = icon("star"), class = "btn-sm btn-info"))
        ),

        p(
          class = "help-block",
          style = "margin-top: 10px; font-size: 12px;",
          icon("info-circle"),
          " Conseil : Sélectionnez uniquement les modalités pertinentes pour alléger l'affichage."
        )
      )
    })

    # ========================================================
    # BOUTONS DE CONTRÔLE DU SÉLECTEUR
    # ========================================================

    # Liste de toutes les modalités (helper)
    all_modalities_list <- reactive({
      req(rv$illu_proj)

      illu_proj <- rv$illu_proj
      factor_vars <- names(illu_proj)[sapply(illu_proj, function(x) x$type == "factor")]

      if(length(factor_vars) == 0) return(list())

      modalities <- list()
      for(vn in factor_vars) {
        obj <- illu_proj[[vn]]
        for(lbl in obj$labels) {
          mod_id <- paste0(vn, "___", lbl)
          mod_label <- paste0(vn, " = ", lbl)
          modalities[[mod_id]] <- mod_label
        }
      }
      modalities
    })

    # Bouton : Tout sélectionner
    observeEvent(input$select_all_mod, {
      req(all_modalities_list())
      updateCheckboxGroupInput(
        session,
        "selected_mods",
        selected = names(all_modalities_list())
      )
    })

    # Bouton : Tout désélectionner
    observeEvent(input$deselect_all_mod, {
      updateCheckboxGroupInput(
        session,
        "selected_mods",
        selected = character(0)
      )
    })

    # Bouton : Top 10
    observeEvent(input$select_top10_mod, {
      req(all_modalities_list())
      all_mods <- names(all_modalities_list())
      updateCheckboxGroupInput(
        session,
        "selected_mods",
        selected = all_mods[1:min(10, length(all_mods))]
      )
    })

    # ========================================================
    # AFFICHAGE DES MODALITÉS (FILTRÉ)
    # ========================================================
    output$modalities_ui <- renderUI({
      req(rv$ready, rv$illu_proj)

      illu_proj <- rv$illu_proj

      factor_vars <- names(illu_proj)[sapply(illu_proj, function(x) x$type == "factor")]

      if(length(factor_vars) == 0) {
        return(div(
          class="alert alert-info",
          h4(icon("info-circle"), " Aucune variable catégorielle"),
          p("Les modalités ne sont disponibles que pour les variables illustratives catégorielles.")
        ))
      }

      # ========== DEBUG START ==========
      cat("\n====================================\n")
      cat("DEBUG: modalities_ui renderUI\n")
      cat("====================================\n")

      # Récupérer les modalités sélectionnées
      sel_mods <- input$selected_mods

      cat("1. Type de sel_mods:", class(sel_mods), "\n")
      cat("2. Is NULL?", is.null(sel_mods), "\n")
      cat("3. Length:", length(sel_mods), "\n")

      if(!is.null(sel_mods) && length(sel_mods) > 0) {
        cat("4. Premières sélections:", paste(head(sel_mods, 3), collapse = " | "), "\n")
      }

      # Si aucune sélection, afficher un message
      if(is.null(sel_mods) || length(sel_mods) == 0) {
        cat("⚠️ AUCUNE SÉLECTION -> Affichage message\n")
        cat("====================================\n\n")
        return(div(
          class="alert alert-warning",
          style="background: #fff3cd; border-left: 4px solid #ffc107; padding: 15px;",
          h4(icon("exclamation-triangle"), " Aucune modalité sélectionnée"),
          p("Utilisez le sélecteur ci-dessus pour choisir les modalités à afficher."),
          p(strong("Astuce :"), " Cliquez sur \"Top 10\" pour afficher les 10 premières modalités.")
        ))
      }

      cat("5. Nombre de variables factor:", length(factor_vars), "\n")
      cat("6. Variables factor:", paste(factor_vars, collapse = ", "), "\n")

      tagList(
        lapply(factor_vars, function(vn) {
          cat("\n--- Variable:", vn, "---\n")

          obj <- illu_proj[[vn]]
          emb <- obj$embeddings

          cat("  Nombre total de modalités pour", vn, ":", length(obj$labels), "\n")
          cat("  Modalités disponibles:", paste(head(obj$labels, 5), collapse = ", "), "...\n")

          # ⭐ FILTRER : garder seulement les modalités sélectionnées
          selected_labels <- c()
          for(i in seq_along(obj$labels)) {
            lbl <- obj$labels[i]
            mod_id <- paste0(vn, "___", lbl)

            if(mod_id %in% sel_mods) {
              selected_labels <- c(selected_labels, lbl)
              cat("  ✓ Match trouvé:", mod_id, "\n")
            }
          }

          cat("  → Nombre de modalités sélectionnées:", length(selected_labels), "\n")

          # Si aucune modalité sélectionnée pour cette variable, skip
          if(length(selected_labels) == 0) {
            cat("  ⚠️ SKIP (aucune sélection)\n")
            return(NULL)
          }

          # Filtrer les embeddings
          keep_idx <- obj$labels %in% selected_labels
          emb <- emb[keep_idx, , drop = FALSE]
          labels_filtered <- obj$labels[keep_idx]

          cat("  → Embeddings filtrés: dim =", paste(dim(emb), collapse = "x"), "\n")

          # ⭐ NOUVEAU : Vérifier si les embeddings sont normalisés
          distances <- sqrt(rowSums(emb^2))
          is_normalized <- all(abs(distances - 1) < 1e-6)

          tagList(
            div(class="section-card",
                h4(paste("📊", vn)),

                # ⭐ AVERTISSEMENT si normalisé
                if(is_normalized) {
                  div(
                    class = "alert alert-warning",
                    style = "background: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin-bottom: 15px;",
                    HTML(
                      "<strong><i class='fa fa-exclamation-triangle'></i> Note :</strong>
                  Les embeddings sont <strong>normalisés L2</strong> (distance à l'origine = 1).<br>
                  Ceci est normal pour la cohérence du clustering.
                  Les distances <em>relatives</em> entre modalités restent significatives."
                    )
                  )
                },

                # Tableau avec toutes les dimensions
                tags$table(class="results-table",
                           tags$thead(
                             tags$tr(
                               c(
                                 list(tags$th("Modalité")),
                                 lapply(1:ncol(emb), function(i) tags$th(paste0("Dim ", i))),
                                 list(tags$th(if(is_normalized) "Norme (=1)" else "Distance Origine"))
                               )
                             )
                           ),
                           tags$tbody(
                             lapply(1:nrow(emb), function(i) {
                               coords <- emb[i, ]
                               dist_origin <- sqrt(sum(coords^2))

                               tags$tr(
                                 c(
                                   list(tags$td(strong(labels_filtered[i]))),
                                   lapply(coords, function(c) tags$td(round(c, 4))),
                                   list(tags$td(round(dist_origin, 4)))
                                 )
                               )
                             })
                           )
                ),

                # ⭐ NOUVEAU : Afficher les distances INTER-MODALITÉS
                if(nrow(emb) > 1) {
                  tagList(
                    br(),
                    h5("📏 Distances entre Modalités (Espace Latent Complet)"),
                    tags$table(class="results-table",
                               tags$thead(
                                 tags$tr(
                                   c(
                                     list(tags$th("")),
                                     lapply(labels_filtered, function(lab) tags$th(lab))
                                   )
                                 )
                               ),
                               tags$tbody(
                                 lapply(1:nrow(emb), function(i) {
                                   tags$tr(
                                     c(
                                       list(tags$th(strong(labels_filtered[i]))),
                                       lapply(1:nrow(emb), function(j) {
                                         if(i == j) {
                                           tags$td("—")
                                         } else {
                                           dist_ij <- sqrt(sum((emb[i,] - emb[j,])^2))
                                           tags$td(round(dist_ij, 4))
                                         }
                                       })
                                     )
                                   )
                                 })
                               )
                    )
                  )
                },

                # Plot 2D avec PCA
                renderPlot({
                  if(ncol(emb) > 2) {
                    pca <- prcomp(emb, center = TRUE, scale. = FALSE)
                    emb_2d <- pca$x[, 1:2]
                    var_exp <- summary(pca)$importance[2, 1:2] * 100
                    xlab <- sprintf("PC1 (%.1f%%)", var_exp[1])
                    ylab <- sprintf("PC2 (%.1f%%)", var_exp[2])
                    title <- sprintf("Espace Latent (PCA) - %s\n(Total: %.1f%%)",
                                     vn, sum(var_exp))
                  } else {
                    emb_2d <- emb[, 1:2]
                    xlab <- "Dimension 1"
                    ylab <- "Dimension 2"
                    title <- paste("Espace Latent des Modalités -", vn)
                  }

                  # ⭐ Calculer distances inter-modalités pour annotations
                  if(nrow(emb) > 1) {
                    dist_matrix <- as.matrix(dist(emb))
                  }

                  plot(
                    emb_2d,
                    pch = 19,
                    col = rainbow(nrow(emb_2d)),
                    cex = 3,
                    main = title,
                    xlab = xlab,
                    ylab = ylab,
                    xlim = range(emb_2d[,1]) * 1.3,
                    ylim = range(emb_2d[,2]) * 1.3
                  )

                  # Flèches depuis l'origine
                  arrows(0, 0, emb_2d[,1], emb_2d[,2],
                         col = rainbow(nrow(emb_2d)),
                         lwd = 2,
                         length = 0.15)

                  # ⭐ LIGNES entre modalités avec distances
                  if(nrow(emb) > 1) {
                    for(i in 1:(nrow(emb_2d)-1)) {
                      for(j in (i+1):nrow(emb_2d)) {
                        segments(
                          emb_2d[i, 1], emb_2d[i, 2],
                          emb_2d[j, 1], emb_2d[j, 2],
                          col = "gray60",
                          lty = 2,
                          lwd = 1
                        )

                        # Annotation distance
                        mid_x <- (emb_2d[i, 1] + emb_2d[j, 1]) / 2
                        mid_y <- (emb_2d[i, 2] + emb_2d[j, 2]) / 2

                        text(
                          mid_x, mid_y,
                          labels = sprintf("%.2f", dist_matrix[i, j]),
                          cex = 0.65,
                          col = "gray30",
                          font = 2,
                          bg = "white"
                        )
                      }
                    }
                  }

                  text(emb_2d, labels = labels_filtered, pos = 3, cex = 1, font = 2)

                  # Origine
                  points(0, 0, pch = 4, cex = 3, lwd = 3, col = "black")
                  text(0, 0, "Origine", pos = 1, cex = 0.8)

                  grid()

                  if(ncol(emb) > 2) {
                    mtext(
                      sprintf(
                        "⚠ Projection PCA 2D (latent_dim = %d) | Distances annotées = distances %dD réelles",
                        ncol(emb), ncol(emb)
                      ),
                      side = 1,
                      line = 4.5,
                      cex = 0.75,
                      col = "blue",
                      font = 2
                    )
                  }
                }, height = 450)
            ),
            hr()
          )
        })
      )
    })


    # ========================================================
    # TOP VARIABLES PROCHES - UI CONTROLS (AMÉLIORÉ)
    # ========================================================
    output$nearest_controls_ui <- renderUI({
      req(rv$ready)

      if(is.null(rv$illu_proj)) {
        return(div(
          class="alert alert-warning",
          "Aucune variable illustrative disponible."
        ))
      }

      illu_proj <- rv$illu_proj

      if(input$nearest_var_type == "factor") {
        # Variables catégorielles
        factor_vars <- names(illu_proj)[sapply(illu_proj, function(x) x$type == "factor")]

        if(length(factor_vars) == 0) {
          return(div(class="alert alert-info", "Aucune variable catégorielle illustrative."))
        }

        tagList(
          fluidRow(
            column(
              6,
              selectInput(
                ns("nearest_factor_var"),
                "Variable catégorielle :",
                choices = factor_vars
              )
            ),
            column(
              6,
              uiOutput(ns("nearest_factor_level_ui"))
            )
          )
        )

      } else {
        # Variables numériques
        numeric_vars <- names(illu_proj)[sapply(illu_proj, function(x) x$type == "numeric")]

        if(length(numeric_vars) == 0) {
          return(div(class="alert alert-info", "Aucune variable numérique illustrative."))
        }

        selectInput(
          ns("nearest_numeric_var"),
          "Variable numérique :",
          choices = numeric_vars
        )
      }
    })

    output$nearest_factor_level_ui <- renderUI({
      req(input$nearest_factor_var, rv$illu_proj)

      obj <- rv$illu_proj[[input$nearest_factor_var]]

      selectInput(
        ns("nearest_factor_level"),
        "Modalité :",
        choices = obj$labels
      )
    })


    # ========================================================
    # TOP VARIABLES PROCHES - COMPUTE (AMÉLIORÉ)
    # ========================================================
    observeEvent(input$compute_nearest, {
      req(model(), rv$illu_proj)

      withProgress(message = "Calcul des variables les plus proches...", value = 0, {

        m <- model()
        emb_model <- m$embeddings

        if(input$nearest_var_type == "factor") {
          req(input$nearest_factor_var, input$nearest_factor_level)

          obj <- rv$illu_proj[[input$nearest_factor_var]]
          level_idx <- which(obj$labels == input$nearest_factor_level)

          if(length(level_idx) == 0) {
            showNotification("Modalité introuvable", type = "error")
            return()
          }

          illu_point <- obj$embeddings[level_idx, ]

          incProgress(0.5)

          # Calcul des distances
          distances <- apply(emb_model, 1, function(var_emb) {
            sqrt(sum((var_emb - illu_point)^2))
          })

          # Tri et top k
          top_indices <- order(distances)[1:min(input$nearest_k, length(distances))]

          results_df <- data.frame(
            Rang = 1:length(top_indices),
            Variable = rownames(emb_model)[top_indices],
            Cluster = m$clusters[top_indices],
            Distance = round(distances[top_indices], 4),
            stringsAsFactors = FALSE
          )

          # Ajouter les premières dimensions pour contexte
          n_dims_show <- min(3, ncol(emb_model))
          for(i in 1:n_dims_show) {
            results_df[[paste0("Dim", i)]] <- round(emb_model[top_indices, i], 4)
          }

          rv$nearest_results <- list(
            type = "factor",
            var_name = input$nearest_factor_var,
            level = input$nearest_factor_level,
            results = results_df,
            illu_coords = illu_point
          )

        } else {
          # Numérique
          req(input$nearest_numeric_var)

          obj <- rv$illu_proj[[input$nearest_numeric_var]]
          illu_point <- obj$embeddings[1, ]  # Un seul point pour numérique

          incProgress(0.5)

          # Calcul des distances
          distances <- apply(emb_model, 1, function(var_emb) {
            sqrt(sum((var_emb - illu_point)^2))
          })

          # Tri et top k
          top_indices <- order(distances)[1:min(input$nearest_k, length(distances))]

          results_df <- data.frame(
            Rang = 1:length(top_indices),
            Variable = rownames(emb_model)[top_indices],
            Cluster = m$clusters[top_indices],
            Distance = round(distances[top_indices], 4),
            stringsAsFactors = FALSE
          )

          # Ajouter dimensions
          n_dims_show <- min(3, ncol(emb_model))
          for(i in 1:n_dims_show) {
            results_df[[paste0("Dim", i)]] <- round(emb_model[top_indices, i], 4)
          }

          rv$nearest_results <- list(
            type = "numeric",
            var_name = input$nearest_numeric_var,
            results = results_df,
            illu_coords = illu_point
          )
        }

        incProgress(1)
      })

      showNotification("✅ Calcul terminé !", type = "message", duration = 3)
    })


    # ========================================================
    # TOP VARIABLES PROCHES - RESULTS UI (AMÉLIORÉ)
    # ⭐ CORRIGÉ : Bandeau informatif si latent_dim > 2
    # ========================================================
    output$nearest_results_ui <- renderUI({
      req(rv$nearest_results)

      res <- rv$nearest_results
      m <- model()

      tagList(
        # ⭐ CORRIGÉ : sprintf() avec UN SEUL string
        if(ncol(m$embeddings) > 2) {
          variance_pca <- sum(summary(prcomp(m$embeddings, center=TRUE, scale.=FALSE))$importance[2, 1:2]) * 100

          div(
            class = "alert alert-info",
            style = "background: #e3f2fd; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;",
            HTML(
              sprintf(
                "<strong><i class='fa fa-info-circle'></i> Information importante :</strong><br>
            Les <strong>distances</strong> affichées dans le tableau sont calculées dans l'espace latent <strong>complet (%dD)</strong>.<br>
            La visualisation 2D utilise une <strong>projection PCA</strong> pour représenter fidèlement ces distances.<br>
            <span style='color: #1976D2;'><strong>Variance expliquée par PC1+PC2 : %.1f%%</strong></span>",
                ncol(m$embeddings),
                variance_pca
              )
            )
          )
        },

        div(class="section-card",
            if(res$type == "factor") {
              h4(paste0("🎯 Variables les plus proches de : ", res$var_name, " = ", res$level))
            } else {
              h4(paste0("🎯 Variables les plus proches de : ", res$var_name))
            },

            p(
              class = "text-muted",
              sprintf("Coordonnées illustrative : (%s)",
                      paste(round(res$illu_coords, 4), collapse = ", "))
            ),

            br(),

            # Tableau interactif
            DTOutput(ns("nearest_table")),

            br(),

            # Visualisation 2D
            h5("📊 Visualisation dans l'Espace Latent (2D)"),
            plotOutput(ns("nearest_plot"), height = "500px")
        )
      )
    })

    output$nearest_table <- renderDT({
      req(rv$nearest_results)

      res <- rv$nearest_results
      m <- model()
      emb <- m$embeddings

      results_df <- res$results

      # ⭐ CORRIGÉ : Ajouter TOUTES les dimensions latentes
      top_indices <- match(results_df$Variable, rownames(emb))

      # Supprimer les anciennes colonnes Dim si présentes
      results_df <- results_df[, !grepl("^Dim", names(results_df))]

      # Ajouter TOUTES les dimensions
      for(i in 1:ncol(emb)) {
        results_df[[paste0("Dim", i)]] <- round(emb[top_indices, i], 4)
      }

      # ⭐ Réorganiser les colonnes : Rang, Variable, Cluster, Distance, puis toutes les Dim
      col_order <- c("Rang", "Variable", "Cluster", "Distance",
                     paste0("Dim", 1:ncol(emb)))
      results_df <- results_df[, col_order]

      dt <- datatable(
        results_df,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        caption = sprintf("Variables actives les plus proches dans l'espace latent (%dD)", ncol(emb))
      )

      dt <- dt |>
        formatStyle(
          'Cluster',
          backgroundColor = styleEqual(
            sort(unique(results_df$Cluster)),
            rainbow(length(unique(results_df$Cluster)), alpha = 0.3)
          )
        )

      dt <- dt |>
        formatStyle(
          'Distance',
          background = styleColorBar(range(results_df$Distance), 'lightcoral'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )

      dt <- dt |>
        formatStyle(
          'Rang',
          fontWeight = 'bold'
        )

      dt
    })

    # ⭐ CORRIGÉ : Plot avec PCA automatique si latent_dim > 2
    output$nearest_plot <- renderPlot({
      req(rv$nearest_results, model())

      res <- rv$nearest_results
      m <- model()
      emb <- m$embeddings
      cl <- m$clusters

      # ========================================
      # DÉCISION AUTOMATIQUE : PCA si latent_dim > 2
      # ========================================
      use_pca_for_nearest <- ncol(emb) > 2

      if(use_pca_for_nearest) {
        # Projection PCA pour visualisation fidèle
        pca <- prcomp(emb, center = TRUE, scale. = FALSE)
        emb_2d <- pca$x[, 1:2]
        rownames(emb_2d) <- rownames(emb)

        variance_explained <- summary(pca)$importance[2, 1:2] * 100

        # Projeter le point illustratif dans l'espace PCA
        illu_full <- res$illu_coords
        illu_2d <- predict(pca, newdata = matrix(illu_full, nrow = 1))[1, 1:2]

        xlab <- sprintf("PC1 (%.1f%% variance)", variance_explained[1])
        ylab <- sprintf("PC2 (%.1f%% variance)", variance_explained[2])
        main_title <- sprintf(
          "Variables Proches (Projection PCA)\nVariance totale: %.1f%% | latent_dim = %d",
          sum(variance_explained), ncol(emb)
        )

      } else {
        # Pas besoin de PCA si latent_dim == 2
        emb_2d <- emb[, 1:2]
        illu_2d <- res$illu_coords[1:2]

        xlab <- "Latent Dim 1"
        ylab <- "Latent Dim 2"
        main_title <- "Variables Proches (Espace Latent 2D)"
      }

      # Variables du top k
      top_vars <- res$results$Variable

      # Couleurs
      colors_all <- rep("gray70", nrow(emb_2d))
      colors_all[rownames(emb_2d) %in% top_vars] <- rainbow(length(unique(cl)))[cl[rownames(emb_2d) %in% top_vars]]

      # Tailles
      sizes_all <- rep(1, nrow(emb_2d))
      sizes_all[rownames(emb_2d) %in% top_vars] <- 2.5

      # ========================================
      # ⭐ NOUVEAU : CALCUL LIMITES POUR CENTRER L'ILLUSTRATIVE
      # ========================================

      # Extraire les coordonnées des top variables
      top_coords <- emb_2d[rownames(emb_2d) %in% top_vars, ]

      # Calculer le centre de gravité (illustrative + top vars)
      all_x <- c(illu_2d[1], top_coords[, 1])
      all_y <- c(illu_2d[2], top_coords[, 2])

      center_x <- mean(all_x)
      center_y <- mean(all_y)

      # Calculer la portée (range) nécessaire
      range_x <- diff(range(all_x))
      range_y <- diff(range(all_y))

      # Élargir de 40% pour avoir de la marge
      margin_factor <- 1.4

      xlim_centered <- c(
        center_x - range_x * margin_factor / 2,
        center_x + range_x * margin_factor / 2
      )

      ylim_centered <- c(
        center_y - range_y * margin_factor / 2,
        center_y + range_y * margin_factor / 2
      )

      # ========================================
      # PLOT PRINCIPAL AVEC LIMITES CENTRÉES
      # ========================================
      plot(
        emb_2d,
        col = colors_all,
        pch = 19,
        cex = sizes_all,
        main = main_title,
        xlab = xlab,
        ylab = ylab,
        xlim = xlim_centered,
        ylim = ylim_centered,
        font.main = 2,
        cex.main = 1
      )

      # Labels uniquement pour le top k
      text(
        emb_2d[rownames(emb_2d) %in% top_vars, ],
        labels = rownames(emb_2d)[rownames(emb_2d) %in% top_vars],
        pos = 3,
        cex = 0.8,
        font = 2
      )

      # ========================================
      # POINT ILLUSTRATIF (étoile rouge)
      # ========================================
      points(illu_2d[1], illu_2d[2],
             pch = 8,
             cex = 4,
             lwd = 4,
             col = "red")

      text(illu_2d[1], illu_2d[2],
           labels = if(res$type == "factor") res$level else res$var_name,
           pos = 1,
           cex = 1.2,
           font = 2,
           col = "red")

      # ========================================
      # FLÈCHES vers TOP 5 + ANNOTATIONS DISTANCE
      # ========================================
      for(i in 1:min(5, nrow(res$results))) {
        var_name <- res$results$Variable[i]
        var_coords <- emb_2d[var_name, ]
        dist_value <- res$results$Distance[i]

        # Flèche
        arrows(
          illu_2d[1], illu_2d[2],
          var_coords[1], var_coords[2],
          col = "red",
          lwd = 2,
          length = 0.1,
          lty = 2
        )

        # ⭐ Annotation de distance sur la flèche
        mid_x <- (illu_2d[1] + var_coords[1]) / 2
        mid_y <- (illu_2d[2] + var_coords[2]) / 2

        text(
          mid_x, mid_y,
          labels = sprintf("d=%.2f", dist_value),
          cex = 0.7,
          col = "darkred",
          font = 2,
          bg = "white"
        )
      }

      grid()

      # ========================================
      # LÉGENDE
      # ========================================
      legend(
        "topright",
        legend = c(
          paste("Cluster", sort(unique(cl))),
          "Illustrative",
          "Top K",
          "Autres"
        ),
        col = c(
          rainbow(length(unique(cl))),
          "red",
          "black",
          "gray70"
        ),
        pch = c(rep(19, length(unique(cl))), 8, 19, 19),
        cex = 0.8,
        bg = "white"
      )

      # ========================================
      # AVERTISSEMENT si PCA utilisée
      # ========================================
      if(use_pca_for_nearest) {
        mtext(
          sprintf(
            "⚠ Projection PCA utilisée (latent_dim = %d → 2D) pour préserver les distances réelles",
            ncol(emb)
          ),
          side = 1,
          line = 4.5,
          cex = 0.85,
          col = "blue",
          font = 2
        )
      }
    })


    # ========================================================
    # FONCTION: Créer visualisation AVANT (corrélation)
    # ========================================================
    compute_before_coords <- function(X) {
      cor_mat <- cor(X, use = "pairwise.complete.obs")
      dist_mat <- as.dist(1 - abs(cor_mat))
      mds <- cmdscale(dist_mat, k = 2)
      rownames(mds) <- colnames(X)
      colnames(mds) <- c("Dim1", "Dim2")
      return(mds)
    }


    # ========================================================
    # FONCTION: Créer visualisation APRÈS (embeddings)
    # ========================================================
    compute_after_coords <- function(embeddings) {
      if(ncol(embeddings) == 2){
        return(embeddings[, 1:2])
      }
      pca <- prcomp(embeddings, center = TRUE, scale. = FALSE)
      coords <- pca$x[, 1:2]
      rownames(coords) <- rownames(embeddings)
      colnames(coords) <- c("PC1", "PC2")
      return(coords)
    }


    # ========================================================
    # Static plots
    # ========================================================
    output$static_plot <- renderPlot({
      req(is_deep(), model())
      m <- model()

      tryCatch({

        if(input$plot_type == "before_after"){

          req(data())
          X_orig <- data()

          vars_model <- rownames(m$embeddings)
          vars_in_data <- vars_model[vars_model %in% names(X_orig)]

          X_use <- X_orig[, vars_in_data, drop = FALSE]
          X_use <- X_use[, sapply(X_use, is.numeric), drop = FALSE]

          vars_use <- colnames(X_use)

          if(length(vars_use) < 3){
            plot.new()
            text(0.5,0.5,"Pas assez de variables numériques communes\nentre data et modèle Deep (min 3).")
            return()
          }

          emb_use <- m$embeddings[vars_use, , drop=FALSE]
          cl_use  <- m$clusters[match(vars_use, vars_model)]

          colors <- rainbow(length(unique(cl_use)))[as.integer(cl_use)]

          coords_before <- compute_before_coords(X_use)
          coords_after  <- compute_after_coords(emb_use)

          coords_before <- coords_before[vars_use, , drop=FALSE]
          coords_after  <- coords_after[vars_use, , drop=FALSE]

          par(mfrow = c(1,2), mar=c(4,4,3,1))

          plot(coords_before[,1], coords_before[,2],
               col="gray50", pch=19, cex=1.5,
               xlab="Dim 1 (MDS)", ylab="Dim 2 (MDS)",
               main="AVANT clustering\n(Corrélation)", font.main=2)

          text(coords_before[,1], coords_before[,2],
               labels=vars_use, pos=3, cex=0.7)

          grid()

          plot(coords_after[,1], coords_after[,2],
               col=colors, pch=19, cex=1.5,
               xlab=ifelse(ncol(emb_use)==2,"Latent Dim 1","PC1"),
               ylab=ifelse(ncol(emb_use)==2,"Latent Dim 2","PC2"),
               main="APRÈS clustering Deep\n(Espace latent)", font.main=2)

          text(coords_after[,1], coords_after[,2],
               labels=vars_use, pos=3, cex=0.7)

          grid()

          legend("topright",
                 legend=paste("Cluster", sort(unique(cl_use))),
                 col=rainbow(length(unique(cl_use))),
                 pch=19, cex=0.8, bg="white")

          mtext(paste0("Comparaison AVANT/APRÈS Deep (",
                       length(vars_use)," variables communes)"),
                side=3, line=-2, outer=TRUE, font=2, cex=1.2)

          return()
        }


        if(input$plot_type=="recon"){
          m$plot_reconstruction()
          return()
        }

        if(input$plot_type=="imp"){

          emb <- m$embeddings
          cl  <- m$clusters

          centers <- by(emb, cl, colMeans) |> do.call(rbind, args=_)

          dists <- sapply(seq_len(nrow(emb)), function(i){
            k <- cl[i]
            sqrt(sum((emb[i,]-centers[k,])^2))
          })

          imp_dist <- 1/(1+dists)
          imp_dist <- imp_dist/max(imp_dist)

          imp_var <- sapply(seq_len(nrow(emb)), function(i){
            k <- cl[i]
            idx_k <- which(cl == k)

            var_with <- sum(apply(emb[idx_k, , drop=FALSE], 2, var))

            if(length(idx_k) > 1){
              idx_without <- idx_k[idx_k != i]
              var_without <- sum(apply(emb[idx_without, , drop=FALSE], 2, var))
              contrib <- abs(var_with - var_without)
            } else {
              contrib <- var_with
            }

            contrib
          })

          imp_var <- imp_var/max(imp_var)

          imp_cor <- sapply(seq_len(nrow(emb)), function(i){
            k <- cl[i]
            cor_val <- cor(emb[i,], centers[k,])
            abs(cor_val)
          })

          imp_cor[!is.finite(imp_cor)] <- 0
          imp_cor <- imp_cor/max(imp_cor)

          importance <- 0.4*imp_dist + 0.3*imp_var + 0.3*imp_cor
          names(importance) <- rownames(emb)
          importance <- sort(importance, decreasing=TRUE)

          top_n <- min(20, length(importance))
          imp_top <- head(importance, top_n)

          cl_top <- cl[names(imp_top)]
          colors <- rainbow(length(unique(cl)))[cl_top]

          par(mar=c(5,10,4,2))

          bp <- barplot(
            imp_top,
            horiz=TRUE,
            las=1,
            col=colors,
            border=NA,
            xlim=c(0, max(imp_top)*1.15),
            main="Importance des variables (Top 20)\nScore composite : distance + variance + cohésion",
            xlab="Score d'importance",
            cex.names=0.85
          )

          text(
            x = imp_top + max(imp_top)*0.02,
            y = bp,
            labels = round(imp_top, 3),
            cex = 0.75,
            pos = 4
          )

          legend(
            "bottomright",
            legend = paste("Cluster", sort(unique(cl))),
            fill = rainbow(length(unique(cl))),
            border = NA,
            cex = 0.8,
            bg = "white"
          )

          abline(v = median(imp_top), lty=2, col="gray40", lwd=1.5)

          return()
        }

        if(input$plot_type=="emb2d_static"){

          emb <- m$embeddings
          cl <- m$clusters
          colors <- rainbow(length(unique(cl)))[cl]

          illu_emb <- NULL
          illu_lab <- NULL

          if(!is.null(rv$illu_proj) && length(input$illu_show)>0){
            emb_list <- lapply(input$illu_show, function(vn) isolate(rv$illu_proj[[vn]]$embeddings))
            illu_emb <- do.call(rbind, emb_list)
            illu_lab <- rownames(illu_emb)
          }

          use_pca <- isTRUE(input$use_pca_2d) && ncol(emb) > 2

          if(use_pca){

            pca <- prcomp(emb, center = TRUE, scale. = FALSE)
            coords <- pca$x[, 1:2]
            rownames(coords) <- rownames(emb)

            variance_explained <- summary(pca)$importance[2, 1:2] * 100

            if(!is.null(illu_emb)){
              illu_coords <- predict(pca, newdata = illu_emb)[, 1:2]
              rownames(illu_coords) <- illu_lab
            }

            plot(coords[, 1], coords[, 2],
                 col = colors,
                 pch = 19,
                 cex = 1.5,
                 xlab = sprintf("PC1 (%.1f%% variance)", variance_explained[1]),
                 ylab = sprintf("PC2 (%.1f%% variance)", variance_explained[2]),
                 main = sprintf("Embeddings 2D avec PCA\nlatent_dim = %d → Total variance: %.1f%%",
                                ncol(emb), sum(variance_explained)),
                 cex.main = 1.1,
                 font.main = 2)

            text(coords[, 1], coords[, 2],
                 labels = rownames(coords),
                 pos = 3,
                 cex = 0.7)

            grid()

            if(!is.null(illu_emb)){
              points(illu_coords[, 1], illu_coords[, 2],
                     pch = 8, cex = 2.2, lwd = 2, col = "black")
              text(illu_coords[, 1], illu_coords[, 2],
                   labels = illu_lab,
                   pos = 4, cex = 0.9)
            }

            legend_items <- paste("Cluster", sort(unique(cl)))
            legend_cols <- rainbow(length(unique(cl)))
            legend_pch <- rep(19, length(unique(cl)))

            if(!is.null(illu_emb)){
              legend_items <- c(legend_items, "Illustrative")
              legend_cols <- c(legend_cols, "black")
              legend_pch <- c(legend_pch, 8)
            }

            legend("topright",
                   legend = legend_items,
                   col = legend_cols,
                   pch = legend_pch,
                   cex = 0.8,
                   bg = "white")

          } else {

            coords <- emb[, 1:2]

            if(!is.null(illu_emb)){
              illu_coords <- illu_emb[, 1:2]
            }

            plot(coords[, 1], coords[, 2],
                 col = colors,
                 pch = 19,
                 cex = 1.5,
                 xlab = "Latent Dim 1",
                 ylab = "Latent Dim 2",
                 main = sprintf("Embeddings 2D (Sélection Dim1-Dim2)\nlatent_dim = %d",
                                ncol(emb)),
                 cex.main = 1.1,
                 font.main = 2)

            text(coords[, 1], coords[, 2],
                 labels = rownames(emb),
                 pos = 3,
                 cex = 0.7)

            grid()

            if(!is.null(illu_emb)){
              points(illu_coords[, 1], illu_coords[, 2],
                     pch = 8, cex = 2.2, lwd = 2, col = "black")
              text(illu_coords[, 1], illu_coords[, 2],
                   labels = illu_lab,
                   pos = 4, cex = 0.9)
            }

            legend_items <- paste("Cluster", sort(unique(cl)))
            legend_cols <- rainbow(length(unique(cl)))
            legend_pch <- rep(19, length(unique(cl)))

            if(!is.null(illu_emb)){
              legend_items <- c(legend_items, "Illustrative")
              legend_cols <- c(legend_cols, "black")
              legend_pch <- c(legend_pch, 8)
            }

            legend("topright",
                   legend = legend_items,
                   col = legend_cols,
                   pch = legend_pch,
                   cex = 0.8,
                   bg = "white")
          }

          return()
        }

      }, error=function(e){
        plot.new(); text(0.5,0.5,paste("Erreur:",e$message))
      })
    })


    # ========================================================
    # Plotly plots (2D/3D)
    # ⭐ CORRIGÉ : emb2d_plotly AVEC PCA
    # ⭐ CORRIGÉ : emb3d_plotly AVEC PCA
    # ========================================================
    output$plotly_plot <- renderPlotly({
      req(is_deep(), model(), clusters())
      m  <- model()
      emb <- m$embeddings
      cl  <- factor(clusters())

      shown <- input$illu_show
      illu_proj <- isolate(rv$illu_proj)

      tryCatch({


        if(input$plot_type == "emb2d_pca_anim"){

          if(is.null(rownames(emb))){
            rownames(emb) <- paste0("Var", seq_len(nrow(emb)))
          }

          if(ncol(emb) < 2){
            return(plotly::plot_ly() |> plotly::layout(
              title="latent_dim >= 2 requis pour animation PCA"
            ))
          }

          coords_before <- emb[,1:2, drop=FALSE]

          pca <- prcomp(emb, center=TRUE, scale.=FALSE)
          coords_after <- pca$x[,1:2, drop=FALSE]

          normalize_coords <- function(coords){
            m <- scale(coords, center=TRUE, scale=TRUE)
            as.matrix(m)
          }

          coords_before_norm <- normalize_coords(coords_before)
          coords_after_norm  <- normalize_coords(coords_after)

          n_frames <- 30

          all_data <- lapply(0:n_frames, function(i){
            t <- i/n_frames
            x <- (1-t)*coords_before_norm[,1] + t*coords_after_norm[,1]
            y <- (1-t)*coords_before_norm[,2] + t*coords_after_norm[,2]
            data.frame(
              x=x,
              y=y,
              variable=rownames(emb),
              cluster=as.integer(cl),
              frame=i,
              stage=sprintf("frame %d",i),
              stringsAsFactors=FALSE
            )
          })

          df_anim <- do.call(rbind, all_data)

          unique_id <- paste0("Run", sample(1e9,1))
          df_anim$uid <- paste0(df_anim$variable, "_", unique_id)

          p <- plotly::plot_ly(
            data=df_anim,
            x=~x, y=~y,
            text=~variable,
            color=~as.factor(cluster),
            frame=~frame,
            ids=~uid,
            type="scatter",
            mode="markers+text",
            marker=list(size=12, line=list(color="black", width=1)),
            textposition="top center"
          ) |>
            plotly::layout(title="Animation PCA embeddings") |>
            plotly::animation_opts(frame=60, transition=30, redraw=TRUE)

          return(p)
        }




        #---

        if(input$plot_type == "before_after"){

          req(data())
          X_orig <- data()

          vars_model <- rownames(m$embeddings)
          vars_in_data <- vars_model[vars_model %in% names(X_orig)]

          X_use <- X_orig[, vars_in_data, drop=FALSE]
          X_use <- X_use[, sapply(X_use, is.numeric), drop=FALSE]

          vars_use <- colnames(X_use)

          if(length(vars_use) < 3){
            return(plotly::plot_ly() |> plotly::layout(
              title="Pas assez de variables numériques communes (min 3)"
            ))
          }

          emb_use <- m$embeddings[vars_use, , drop=FALSE]
          cl_use  <- m$clusters[match(vars_use, vars_model)]
          cl_fac  <- factor(cl_use)

          coords_before <- compute_before_coords(X_use)
          coords_after  <- compute_after_coords(emb_use)

          coords_before <- coords_before[vars_use, , drop=FALSE]
          coords_after  <- coords_after[vars_use, , drop=FALSE]

          normalize_coords <- function(coords){
            scale(coords, center=TRUE, scale=TRUE)
          }
          coords_before_norm <- normalize_coords(coords_before)
          coords_after_norm  <- normalize_coords(coords_after)

          n_frames <- 30

          all_data <- lapply(0:n_frames, function(i){
            t <- i/n_frames
            x <- (1-t)*coords_before_norm[,1] + t*coords_after_norm[,1]
            y <- (1-t)*coords_before_norm[,2] + t*coords_after_norm[,2]

            data.frame(
              x=x, y=y,
              variable=vars_use,
              cluster=as.integer(cl_fac),
              frame=i,
              stage=ifelse(i==0,"AVANT (corr)",
                           ifelse(i==n_frames,"APRÈS (deep)",
                                  paste0("t=",round(t,2)))),
              stringsAsFactors=FALSE
            )
          })

          plot_data <- do.call(rbind, all_data)

          colors_palette <- rainbow(length(unique(cl_fac)))

          p <- plotly::plot_ly(
            data=plot_data,
            x=~x, y=~y,
            text=~variable,
            color=~as.factor(cluster),
            colors=colors_palette,
            frame=~frame,
            ids=~variable,
            type="scatter",
            mode="markers+text",
            marker=list(size=14,line=list(color="black",width=1.5)),
            textposition="top center",
            textfont=list(size=10,color="black"),
            hoverinfo="text",
            hovertext=~paste0("<b>",variable,"</b><br>Cluster: ",cluster,"<br>",stage)
          ) |>
            plotly::layout(
              title="<b>Animation AVANT → APRÈS Deep</b>",
              xaxis=list(title="Dimension 1"),
              yaxis=list(title="Dimension 2")
            ) |>
            plotly::animation_opts(frame=80,transition=40,redraw=TRUE)

          return(p)
        }


        #---

        # ========================================================
        # ⭐ CORRIGÉ : emb2d_plotly AVEC PCA si latent_dim > 2
        # ========================================================
        if(input$plot_type=="emb2d_plotly"){

          # Décision PCA
          if(ncol(emb) > 2) {
            pca <- prcomp(emb, center = TRUE, scale. = FALSE)
            coords_2d <- pca$x[, 1:2]
            var_exp <- summary(pca)$importance[2, 1:2] * 100

            xlab <- sprintf("PC1 (%.1f%%)", var_exp[1])
            ylab <- sprintf("PC2 (%.1f%%)", var_exp[2])
            title <- sprintf("Embeddings 2D (PCA) | Total: %.1f%%", sum(var_exp))
          } else {
            coords_2d <- emb[, 1:2]
            xlab <- "Latent Dim 1"
            ylab <- "Latent Dim 2"
            title <- "Embeddings 2D (Espace Latent)"
          }

          p <- plotly::plot_ly(type="scatter", mode="markers")

          # Variables actives
          p <- p |>
            plotly::add_trace(
              x=coords_2d[,1],
              y=coords_2d[,2],
              text=rownames(emb),
              type="scatter",
              mode="markers+text",
              marker=list(
                size=11,
                color=rainbow(max(as.integer(cl)))[as.integer(cl)],
                line=list(color="black", width=1)
              ),
              textposition="top center",
              name="Actives"
            )

          # Variables illustratives
          if(!is.null(illu_proj) && length(shown)>0){
            for(vn in shown){
              obj <- illu_proj[[vn]]
              if(is.null(obj)) next

              # Projeter illustratives avec même PCA
              if(ncol(emb) > 2) {
                illu_2d <- predict(pca, newdata = obj$embeddings)[, 1:2]
              } else {
                illu_2d <- obj$embeddings[, 1:2, drop = FALSE]
              }

              symb <- if(obj$type=="factor") "diamond" else "x"

              p <- p |>
                plotly::add_trace(
                  x=illu_2d[,1],
                  y=illu_2d[,2],
                  text=obj$labels,
                  type="scatter",
                  mode="markers+text",
                  marker=list(
                    size=14,
                    symbol=symb,
                    color="black",
                    line=list(color="red", width=2)
                  ),
                  textposition="bottom center",
                  name=paste0("Illu: ",vn)
                )
            }
          }

          return(p |> plotly::layout(
            title=title,
            xaxis=list(title=xlab),
            yaxis=list(title=ylab),
            hovermode="closest"
          ))
        }

        # ========================================================
        # ⭐ CORRIGÉ : emb3d_plotly AVEC PCA si latent_dim > 3
        # ========================================================
        if(input$plot_type=="emb3d_plotly"){

          if(ncol(emb)<3){
            return(plotly::plot_ly() |> plotly::layout(
              title="latent_dim >= 3 requis pour 3D"
            ))
          }

          # Décision PCA pour 3D
          if(ncol(emb) > 3) {
            pca <- prcomp(emb, center = TRUE, scale. = FALSE)
            coords_3d <- pca$x[, 1:3]
            var_exp <- summary(pca)$importance[2, 1:3] * 100

            title <- sprintf("Embeddings 3D (PCA) | Total: %.1f%%", sum(var_exp))
            xlab <- sprintf("PC1 (%.1f%%)", var_exp[1])
            ylab <- sprintf("PC2 (%.1f%%)", var_exp[2])
            zlab <- sprintf("PC3 (%.1f%%)", var_exp[3])
          } else {
            coords_3d <- emb[, 1:3]
            title <- "Embeddings 3D (Espace Latent)"
            xlab <- "Dim 1"
            ylab <- "Dim 2"
            zlab <- "Dim 3"
          }

          p <- plotly::plot_ly(type="scatter3d", mode="markers")

          p <- p |>
            plotly::add_trace(
              x=coords_3d[,1], y=coords_3d[,2], z=coords_3d[,3],
              text=rownames(emb),
              type="scatter3d", mode="markers+text",
              marker=list(size=7,
                          color=rainbow(max(as.integer(cl)))[as.integer(cl)]),
              name="Actives"
            )

          if(!is.null(illu_proj) && length(shown)>0){
            for(vn in shown){
              obj <- illu_proj[[vn]]
              if(is.null(obj)) next

              # Projeter illustratives avec même PCA
              if(ncol(emb) > 3) {
                illu_3d <- predict(pca, newdata = obj$embeddings)[, 1:3]
              } else {
                if(ncol(obj$embeddings) < 3) next
                illu_3d <- obj$embeddings[, 1:3]
              }

              symb <- if(obj$type=="factor") "diamond" else "x"

              p <- p |>
                plotly::add_trace(
                  x=illu_3d[,1], y=illu_3d[,2], z=illu_3d[,3],
                  text=obj$labels,
                  type="scatter3d", mode="markers+text",
                  marker=list(size=9, symbol=symb,
                              color="black",
                              line=list(color="red", width=2)),
                  name=paste0("Illu: ",vn)
                )
            }
          }

          return(p |> plotly::layout(
            title=title,
            scene=list(
              xaxis=list(title=xlab),
              yaxis=list(title=ylab),
              zaxis=list(title=zlab)
            )
          ))
        }

      }, error=function(e){
        plotly::plot_ly() |> plotly::layout(title=paste("Erreur:", e$message))
      })
    })


    # ========================================================
    # PROMPT UI (debug toggle)
    # ========================================================
    observeEvent(input$btn_show_prompt, {
      rv$show_prompt <- !isTRUE(rv$show_prompt)
    })

    output$prompt_ui <- renderUI({
      if(!isTRUE(rv$show_prompt) || is.null(rv$last_prompt)) return(NULL)
      tagList(
        h4("Prompt envoyé au LLM :"),
        div(class="prompt-box", rv$last_prompt)
      )
    })


    # ========================================================
    # INTERPRETATION BUTTON - ENRICHI avec résultats détaillés
    # ========================================================
    observeEvent(input$btn_interpret, {

      req(model())

      m <- isolate(model())
      illu_proj_snapshot <- isolate(rv$illu_proj)
      qual_snapshot <- isolate(deep_quality())

      output$interpretation <- renderUI({
        div(class="interpret-box",
            div(
              style="display:flex;align-items:center;gap:10px;font-weight:600;",
              tags$i(class="fa fa-spinner fa-spin", style="font-size:20px;"),
              "Génération de l'interprétation..."
            )
        )
      })

      later::later(function() {

        clusters_info <- capture.output(m$summary())

        illu_text <- ""
        nearest_text <- ""

        if (!is.null(illu_proj_snapshot)) {
          for(vn in names(illu_proj_snapshot)) {

            obj <- illu_proj_snapshot[[vn]]
            emb <- obj$embeddings
            labs <- obj$labels

            illu_text <- paste0(
              illu_text,
              "\nVariable illustrative : ", vn, "\n",
              "Type : ", obj$type, "\n",
              "Labels : ", paste(labs, collapse=", "), "\n",
              "Coords latentes :\n",
              paste(capture.output(print(emb)), collapse="\n"),
              "\n"
            )

            if (obj$type == "factor") {
              for (lvl in rownames(emb)) {

                near <- tryCatch({
                  m$nearest_to_illustrative(
                    illustrative_embeddings = emb,
                    level = lvl,
                    k = 5
                  )
                }, error=function(e) NULL)

                if (!is.null(near)) {
                  nearest_text <- paste0(
                    nearest_text,
                    "\nNearest pour ", vn, " = ", lvl, "\n",
                    paste(capture.output(print(near)), collapse="\n"),
                    "\n"
                  )
                }
              }
            }
          }
        }


        sil_val  <- if(!is.null(qual_snapshot$sil_mean)) round(qual_snapshot$sil_mean, 3) else NA
        dunn_val <- if(!is.null(qual_snapshot$dunn)) round(qual_snapshot$dunn, 3) else NA
        dbi_val  <- if(!is.null(qual_snapshot$dbi)) round(qual_snapshot$dbi, 3) else NA
        glob_val <- if(!is.null(qual_snapshot$quality)) round(qual_snapshot$quality, 3) else NA

        mse_val     <- if(!is.null(m$recon_mse)) m$recon_mse else NA
        latent_dim  <- if(!is.null(m$latent_dim)) m$latent_dim else ncol(m$embeddings)
        k_val       <- if(!is.null(m$n_clusters)) m$n_clusters else length(unique(m$clusters))

        prompt <- paste0(
          "Tu es un expert en data science.\n",
          "Interprète proprement un modèle ClustDeepVar.\n\n",
          "CONTRAINTES STRICTES :\n",
          "- NE PAS utiliser graph TD, mermaid, UML, diagrammes ASCII.\n",
          "- NE PAS utiliser de blocs ```mermaid``` ni schémas.\n",
          "- Utiliser UNIQUEMENT du Markdown propre : titres, listes, tableaux.\n\n",

          "=== INFOS MODELE ===\n",
          "- n_clusters = ", k_val, "\n",
          "- latent_dim = ", latent_dim, "\n",
          "- Reconstruction MSE = ", mse_val, "\n\n",

          "=== CLUSTERING ===\n",
          paste(clusters_info, collapse="\n"), "\n\n",

          "=== QUALITÉ DU CLUSTERING ===\n",
          "- Silhouette moyen = ", sil_val, "\n",
          "- Dunn index       = ", dunn_val, "\n",
          "- Davies-Bouldin   = ", dbi_val, "\n",
          "- Score global     = ", glob_val, "\n\n",

          "=== ILLUSTRATIVES ===\n",
          illu_text, "\n\n",

          "=== NEAREST ===\n",
          nearest_text, "\n\n",

          "Consignes :\n",
          "1. Réponds en MARKDOWN clair.\n",
          "2. Utiliser des TABLEAUX markdown (|col|col|).\n",
          "3. Expliquer l'espace latent en 2-3 phrases.\n",
          "4. Donner 3-5 insights concrets et actionnables.\n",
          "5. Aucun diagramme, aucune syntaxe mermaid.\n",
          "6. Interpréter les métriques de qualité (Silhouette, Dunn, DBI).\n",
          "7. Suggérer des améliorations si la qualité est faible.\n"
        )

        rv$last_prompt <- prompt

        url <- "https://api.mistral.ai/v1/chat/completions"
        key <- "Wy3LrP9ad5DQy0OFRHS0yT7JLqq0BODa"

        res <- httr::POST(
          url,
          httr::add_headers(
            Authorization = paste("Bearer", key),
            "Content-Type" = "application/json"
          ),
          body = jsonlite::toJSON(list(
            model = "mistral-medium",
            messages = list(list(role="user", content=prompt)),
            temperature = 0.5
          ), auto_unbox = TRUE)
        )

        raw <- httr::content(res, as="parsed")

        txt_md <- tryCatch(
          raw$choices[[1]]$message$content,
          error=function(e){
            if(is.character(raw)) return(raw)
            paste("Erreur parsing :", capture.output(str(raw)), collapse="\n")
          }
        )

        txt_html <- tryCatch({
          if (requireNamespace("commonmark", quietly = TRUE)) {
            commonmark::markdown_html(
              txt_md,
              extensions = TRUE
            )
          } else {
            txt_md_escaped <- gsub("\n", "<br>", htmltools::htmlEscape(txt_md))
            paste0("<div style='white-space: pre-wrap;'>", txt_md_escaped, "</div>")
          }
        }, error=function(e){
          paste0("<pre style='white-space: pre-wrap; font-family: inherit;'>",
                 htmltools::htmlEscape(txt_md),
                 "</pre>")
        })

        output$interpretation <- renderUI({
          div(class="interpret-box", shiny::HTML(txt_html))
        })

      }, delay = 0.25)
    })


    # ========================================================
    # Download plot - INCHANGÉ
    # ========================================================
    output$download_plot <- downloadHandler(
      filename=function(){
        paste0("deep_plot_", input$plot_type, "_", Sys.Date(), ".png")
      },
      content=function(file){
        req(is_deep(), model())

        png(file, width=input$plot_width, height=input$plot_height)

        tryCatch({

          if(input$plot_type == "before_after"){
            X_orig <- data()
            numeric_cols <- sapply(X_orig, is.numeric)
            X_numeric <- X_orig[, numeric_cols, drop = FALSE]

            cl <- model()$clusters
            colors <- rainbow(length(unique(cl)))[cl]

            coords_before <- compute_before_coords(X_numeric)
            coords_after <- compute_after_coords(model()$embeddings)

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
                 xlab = "Latent Dim 1", ylab = "Latent Dim 2",
                 main = "APRÈS clustering Deep\n(Espace latent appris)")
            text(coords_after[, 1], coords_after[, 2],
                 labels = rownames(coords_after), pos = 3, cex = 0.7)
            grid()

          } else if(input$plot_type=="emb2d_static"){

            emb <- model()$embeddings
            cl <- model()$clusters
            colors <- rainbow(length(unique(cl)))[cl]

            use_pca <- isTRUE(input$use_pca_2d) && ncol(emb) > 2

            if(use_pca){
              pca <- prcomp(emb, center = TRUE, scale. = FALSE)
              coords <- pca$x[, 1:2]
              rownames(coords) <- rownames(emb)
              variance_explained <- summary(pca)$importance[2, 1:2] * 100

              plot(coords[, 1], coords[, 2],
                   col = colors, pch = 19, cex = 1.5,
                   xlab = sprintf("PC1 (%.1f%% variance)", variance_explained[1]),
                   ylab = sprintf("PC2 (%.1f%% variance)", variance_explained[2]),
                   main = sprintf("Embeddings 2D avec PCA\nlatent_dim = %d → Total: %.1f%%",
                                  ncol(emb), sum(variance_explained)))
              text(coords[, 1], coords[, 2],
                   labels = rownames(coords), pos = 3, cex = 0.7)
              grid()

            } else {
              coords <- emb[, 1:2]
              plot(coords[, 1], coords[, 2],
                   col = colors, pch = 19, cex = 1.5,
                   xlab = "Latent Dim 1", ylab = "Latent Dim 2",
                   main = sprintf("Embeddings 2D (Dim1-Dim2)\nlatent_dim = %d", ncol(emb)))
              text(coords[, 1], coords[, 2],
                   labels = rownames(emb), pos = 3, cex = 0.7)
              grid()
            }

          } else if(input$plot_type=="recon"){
            model()$plot_reconstruction()
          } else if(input$plot_type=="imp"){
            emb <- model()$embeddings
            cl  <- model()$clusters
            centers <- by(emb, cl, colMeans) |> do.call(rbind, args=_)
            dists <- sapply(seq_len(nrow(emb)), function(i){
              k <- cl[i]
              sqrt(sum((emb[i,]-centers[k,])^2))
            })
            imp <- 1/(1+dists)
            imp <- imp/max(imp)
            imp <- sort(imp, decreasing=TRUE)
            par(mar=c(5,10,4,2))
            barplot(imp, horiz=TRUE, las=1,
                    main="Importance des variables",
                    xlab="Score d'importance")
          }
        }, error=function(e){
          plot.new(); text(0.5,0.5,paste("Erreur:", e$message))
        })

        dev.off()
      }
    )
    list(
      illu_proj = reactive(rv$illu_proj)
    )
  })
}
