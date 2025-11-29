# ==========================================================
# MODULE : algorithm_selection.R
# 3 ALGOS + OPTIMISATION DE K (MULTI-METHODES) + DOC MATHJAX
# ==========================================================

algorithm_selection_ui <- function(id){
  ns <- NS(id)

  tagList(
    # CSS pour tooltips + collapsible
    tags$style(HTML("
      .param-tooltip {
        position: relative;
        display: inline-block;
        cursor: help;
        margin-left: 5px;
      }
      .param-tooltip .tooltiptext {
        visibility: hidden;
        width: 300px;
        background-color: #555;
        color: #fff;
        text-align: left;
        border-radius: 6px;
        padding: 10px;
        position: absolute;
        z-index: 1000;
        bottom: 125%;
        left: 50%;
        margin-left: -150px;
        opacity: 0;
        transition: opacity 0.3s;
        font-size: 12px;
        line-height: 1.4;
      }
      .param-tooltip .tooltiptext::after {
        content: '';
        position: absolute;
        top: 100%;
        left: 50%;
        margin-left: -5px;
        border-width: 5px;
        border-style: solid;
        border-color: #555 transparent transparent transparent;
      }
      .param-tooltip:hover .tooltiptext {
        visibility: visible;
        opacity: 1;
      }
      .algo-summary {
        background: white;
        border: 2px solid #3c8dbc;
        border-left: 6px solid #3c8dbc;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .algo-summary h3 {
        margin-top: 0;
        font-weight: bold;
        font-size: 20px;
        color: #2c3e50;
        border-bottom: 2px solid #3c8dbc;
        padding-bottom: 10px;
      }
      .pipeline-step {
        background: #f8f9fa;
        border-left: 4px solid #3c8dbc;
        padding: 12px;
        margin: 10px 0;
        border-radius: 4px;
        color: #2c3e50;
      }
      .pipeline-step strong {
        color: #3c8dbc;
      }
      .formula-box {
        background: #f0f4f8;
        border: 1px solid #d1d8dd;
        padding: 15px;
        border-radius: 6px;
        margin: 12px 0;
        font-size: 15px;
        color: #2c3e50;
        text-align: center;
      }
      .metric-card {
        background: #f9f9f9;
        border-left: 4px solid #3c8dbc;
        padding: 15px;
        margin-bottom: 15px;
        border-radius: 4px;
      }
      .metric-card h4 {
        margin-top: 0;
        color: #3c8dbc;
      }
      .metric-formula {
        background: #fff;
        padding: 10px;
        border-radius: 4px;
        margin: 10px 0;
        text-align: center;
      }
      .metric-usage {
        background: #e8f4f8;
        padding: 10px;
        border-radius: 4px;
        margin-top: 10px;
      }
      .collapse-btn {
        width: 100%;
        text-align: left;
        background: #3c8dbc;
        color: white;
        border: none;
        padding: 12px;
        font-size: 16px;
        font-weight: bold;
        cursor: pointer;
        border-radius: 6px;
        transition: background 0.3s;
      }
      .collapse-btn:hover {
        background: #2980b9;
      }
      .collapse-content {
        max-height: 0;
        overflow: hidden;
        transition: max-height 0.3s ease-out;
      }
      .collapse-content.active {
        max-height: 2000px;
        transition: max-height 0.5s ease-in;
      }
      .k-badge {
        background: #28a745;
        color: white;
        padding: 12px 20px;
        border-radius: 25px;
        font-size: 16px;
        font-weight: bold;
        text-align: center;
        box-shadow: 0 2px 4px rgba(0,0,0,0.2);
        margin-top: 5px;
      }
      .k-display {
        background: #d4edda;
        border: 2px solid #28a745;
        padding: 20px;
        border-radius: 8px;
        text-align: center;
        margin: 15px 0;
      }
      .k-display h3 {
        color: #155724;
        margin: 0;
        font-size: 24px;
      }
    ")
    ),

    # MathJax
    tags$head(
      tags$script(src="https://polyfill.io/v3/polyfill.min.js?features=es6"),
      tags$script(src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
    ),

    # Handler MathJax ciblé
    tags$script(HTML("
      Shiny.addCustomMessageHandler('mathjax-refresh', function(message) {
        if (window.MathJax) {
          var el = document.getElementById(message.id);
          if (el) {
            MathJax.typesetPromise([el]);
          } else {
            MathJax.typesetPromise();
          }
        }
      });
    ")),

    # JavaScript pour collapsible
    tags$script(HTML("
      $(document).on('click', '.collapse-btn', function() {
        var content = $(this).next('.collapse-content');
        var icon = $(this).find('.collapse-icon');

        if (content.hasClass('active')) {
          content.removeClass('active');
          icon.text('+');
        } else {
          content.addClass('active');
          icon.text('−');
        }
      });
    ")),

    fluidRow(
      # GAUCHE : Paramètres
      box(
        width = 5,
        title = "Configuration du modèle",
        status = "primary",
        solidHeader = TRUE,

        selectInput(
          ns("algorithm"), "Méthode :",
          choices = c(
            "Deep Clustering (Autoencodeur)" = "deep",
            "K-means Réallocatif (Qannari)" = "kmeans",
            "VARCLUS Qualitatif (MCA)" = "quali_varclus"
          ),
          selected = "deep"
        ),

        div(
          numericInput(ns("n_clusters"), "Nombre de clusters (k)",
                       value = 3, min = 2, max = 12),
          span(class="param-tooltip",
               icon("question-circle"),
               span(class="tooltiptext",
                    strong("Nombre de clusters"), br(),
                    "• 2-5 : petits datasets", br(),
                    "• 3-8 : datasets moyens", br(),
                    "• Utiliser 'Multi-méthodes' pour optimiser")
          )
        ),

        hr(),

        # Paramètres dépendant de l'algorithme
        uiOutput(ns("algo_params_ui")),

        hr(),
        uiOutput(ns("algo_buttons_ui")),
        hr(),

        actionButton(ns("run_clustering"), "🚀 Lancer",
                     icon=icon("rocket"), class="btn-success btn-lg btn-block")
      ),

      # DROITE : Résumé algo + Métriques docs
      box(
        width = 7,
        title = NULL,
        status = "info",
        solidHeader = FALSE,

        div(
          id = ns("math_container"),
          uiOutput(ns("algo_summary"))
        ),

        # === MÉTRIQUES (DOC) COLLAPSIBLES ===
        tags$button(
          class = "collapse-btn",
          icon("chart-bar"), " Métriques de validation ",
          tags$span(class = "collapse-icon", "+")
        ),

        div(
          class = "collapse-content",
          style = "padding: 15px;",

          div(class="metric-card",
              h4(icon("chart-area"), " Silhouette"),
              p("Mesure la cohésion et séparation des clusters."),
              div(class="metric-formula",
                  HTML("$$ s(i) = \\frac{b(i) - a(i)}{\\max(a(i), b(i))} \\in [-1, 1] $$"),
                  tags$ul(
                    tags$li(tags$code("a(i)"), " : distance moyenne intra-cluster"),
                    tags$li(tags$code("b(i)"), " : distance au plus proche cluster voisin")
                  )
              ),
              div(class="metric-usage",
                  strong("Interprétation :"), br(),
                  "• ", tags$span(style="color:green;", ">0.7"), " : Excellent", br(),
                  "• ", tags$span(style="color:blue;", "0.5-0.7"), " : Bon", br(),
                  "• ", tags$span(style="color:orange;", "0.3-0.5"), " : Moyen", br(),
                  "• ", tags$span(style="color:red;", "<0.3"), " : Faible", br(), br(),
                  strong("Utilité :"), " Identifie variables mal assignées.", br(), br(),
                  strong("⚠️ Adaptation K-means Réallocatif :"), br(),
                  "Distance corrélative utilisée : ",
                  HTML("\\( d(j,k) = 1 - \\text{cor}(X_j, PC1_k)^2 \\)"), br(),
                  "Cette distance est plus adaptée au clustering de variables basé sur corrélation."
              )
          ),

          div(class="metric-card",
              h4(icon("compress-arrows-alt"), " Davies-Bouldin Index (DBI)"),
              p("Ratio dispersion interne / séparation entre clusters."),
              div(class="metric-formula",
                  HTML("$$ DBI = \\frac{1}{k} \\sum_{i=1}^{k} \\max_{j \\ne i} \\frac{s_i + s_j}{d(c_i, c_j)} $$"),
                  tags$ul(
                    tags$li(tags$code("s_i"), " : dispersion intra-cluster i"),
                    tags$li(tags$code("d(c_i, c_j)"), " : distance entre centres")
                  )
              ),
              div(class="metric-usage",
                  strong("Interprétation (↓ mieux) :"), br(),
                  "• ", tags$span(style="color:green;", "<0.5"), " : Excellent", br(),
                  "• ", tags$span(style="color:blue;", "0.5-1.0"), " : Bon", br(),
                  "• ", tags$span(style="color:orange;", "1.0-2.0"), " : Moyen", br(),
                  "• ", tags$span(style="color:red;", ">2.0"), " : Faible", br(), br(),
                  strong("Utilité :"), " Évalue compacité ET séparation."
              )
          ),

          div(class="metric-card",
              h4(icon("expand-arrows-alt"), " Dunn Index"),
              p("Ratio séparation minimale / diamètre maximal."),
              div(class="metric-formula",
                  HTML("$$ Dunn = \\frac{\\min_{i \\ne j} d(C_i, C_j)}{\\max_{\\ell} \\text{diam}(C_\\ell)} $$"),
                  tags$ul(
                    tags$li("Numérateur : distance inter-clusters minimale."),
                    tags$li("Dénominateur : diamètre intra-cluster maximal.")
                  )
              ),
              div(class="metric-usage",
                  strong("Interprétation (↑ mieux) :"), br(),
                  "• ", tags$span(style="color:green;", ">1.0"), " : Excellent", br(),
                  "• ", tags$span(style="color:blue;", "0.5-1.0"), " : Bon", br(),
                  "• ", tags$span(style="color:orange;", "0.2-0.5"), " : Moyen", br(),
                  "• ", tags$span(style="color:red;", "<0.2"), " : Faible", br(), br(),
                  strong("Utilité :"), " Détecte clusters mal séparés.", br(),
                  strong("⚠ Sensible aux outliers.")
              )
          ),

          div(style="background:#e8f4f8;padding:15px;border-radius:6px;margin-top:15px;",
              h4(icon("star"), " Score Qualité Global (Deep)"),
              p("Agrégation normalisée des 3 métriques :"),
              div(class="metric-formula",
                  HTML("$$ Q = \\frac{1}{3} \\left( \\text{Sil}_{norm} + \\text{Dunn}_{norm} + (1 - \\text{DBI}_{norm}) \\right) $$")
              ),
              tags$ul(
                tags$li("Combine cohésion, séparation, robustesse."),
                tags$li("Score unique dans [0, 1]."),
                tags$li(tags$strong("Q > 0.7"), " : clustering haute qualité.")
              )
          ),

          div(style="background:#fff3cd;padding:15px;border-radius:6px;border-left:4px solid #ffc107;margin-top:15px;",
              h4(icon("info-circle"), " Guide d'Interprétation Rapide"),
              tags$table(
                style="width:100%; border-collapse: collapse;",
                tags$tr(
                  tags$th(style="padding:8px; border:1px solid #ddd; background:#f8f9fa;", "Métrique"),
                  tags$th(style="padding:8px; border:1px solid #ddd; background:#f8f9fa;", "Objectif"),
                  tags$th(style="padding:8px; border:1px solid #ddd; background:#f8f9fa;", "Excellent"),
                  tags$th(style="padding:8px; border:1px solid #ddd; background:#f8f9fa;", "Faible")
                ),
                tags$tr(
                  tags$td(style="padding:8px; border:1px solid #ddd;", strong("Silhouette")),
                  tags$td(style="padding:8px; border:1px solid #ddd;", "↑ maximiser"),
                  tags$td(style="padding:8px; border:1px solid #ddd;", tags$span(style="color:green; font-weight:bold;", "> 0.7")),
                  tags$td(style="padding:8px; border:1px solid #ddd;", tags$span(style="color:red; font-weight:bold;", "< 0.3"))
                ),
                tags$tr(
                  tags$td(style="padding:8px; border:1px solid #ddd;", strong("Davies-Bouldin")),
                  tags$td(style="padding:8px; border:1px solid #ddd;", "↓ minimiser"),
                  tags$td(style="padding:8px; border:1px solid #ddd;", tags$span(style="color:green; font-weight:bold;", "< 0.5")),
                  tags$td(style="padding:8px; border:1px solid #ddd;", tags$span(style="color:red; font-weight:bold;", "> 2.0"))
                ),
                tags$tr(
                  tags$td(style="padding:8px; border:1px solid #ddd;", strong("Dunn")),
                  tags$td(style="padding:8px; border:1px solid #ddd;", "↑ maximiser"),
                  tags$td(style="padding:8px; border:1px solid #ddd;", tags$span(style="color:green; font-weight:bold;", "> 1.0")),
                  tags$td(style="padding:8px; border:1px solid #ddd;", tags$span(style="color:red; font-weight:bold;", "< 0.2"))
                ),
                tags$tr(
                  tags$td(style="padding:8px; border:1px solid #ddd;", strong("Qualité (Deep)")),
                  tags$td(style="padding:8px; border:1px solid #ddd;", "↑ maximiser"),
                  tags$td(style="padding:8px; border:1px solid #ddd;", tags$span(style="color:green; font-weight:bold;", "> 0.7")),
                  tags$td(style="padding:8px; border:1px solid #ddd;", tags$span(style="color:red; font-weight:bold;", "< 0.3"))
                )
              )
          )
        )
      )
    ),

    # ============================================================
    # CHOIX OPTIMAL DE K (MULTI-MÉTHODES)
    # ============================================================
    fluidRow(
      box(
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        title = "Choix optimal de k (toutes méthodes)",

        fluidRow(
          column(
            width = 3,
            actionButton(ns("compute_elbow"),
                         "📈 Elbow Method",
                         icon = icon("chart-line"),
                         class = "btn-primary btn-block")
          ),
          column(
            width = 3,
            actionButton(ns("compute_optimal_k"),
                         "🎯 Multi-méthodes",
                         icon = icon("calculator"),
                         class = "btn-info btn-block")
          ),
          column(
            width = 3,
            actionButton(ns("apply_suggested_k"),
                         "✅ Appliquer k suggéré",
                         icon = icon("check"),
                         class = "btn-success btn-block")
          ),
          column(
            width = 3,
            uiOutput(ns("suggested_k_badge"))
          )
        ),

        hr(),
        uiOutput(ns("suggested_k_display")),
        hr(),
        plotOutput(ns("optimal_k_plot"), height = "450px"),
        hr(),
        DTOutput(ns("optimal_k_table"))
      )
    ),

    # Value boxes adaptatifs
    fluidRow(
      uiOutput(ns("metrics_boxes"))
    ),

    # Plots / Qualité — affichés uniquement lorsque Deep est sélectionné
    conditionalPanel(
      condition = sprintf("input['%s'] == 'deep'", ns("algorithm")),

      fluidRow(
        box(width=6, status="info", solidHeader=TRUE,
            title="Silhouette plot (Deep uniquement)",
            plotOutput(ns("silhouette_plot"), height="350px")),
        box(width=6, status="primary", solidHeader=TRUE,
            title="Qualité globale (Deep uniquement)",
            uiOutput(ns("quality_ui")))
      )
    ),


    # Résumé
    fluidRow(
      box(width=12, status="success", solidHeader=TRUE,
          title="Résumé du clustering",
          verbatimTextOutput(ns("cluster_summary")))
    )
  )
}

# ==================== SERVER ====================

algorithm_selection_server <- function(id, data, active_vars){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    rv <- reactiveValues(
      model=NULL, clusters=NULL, method=NULL,
      # Deep
      sil_obj=NULL, sil_mean=NA, dbi=NA, dunn=NA, quality=NA,
      best_params=NULL,
      # K optimisation
      suggested_k=NULL, k_optimization=NULL,
      # K-means
      W_total=NA, B_total=NA, Q=NA,
      # VARCLUS quali
      eta2_matrix=NULL, selection_results=NULL
    )

    # ============================================================
    # UI PARAMS PAR ALGO
    # ============================================================
    output$algo_params_ui <- renderUI({
      req(input$algorithm)

      if (input$algorithm == "deep") {
        tagList(
          h4("Paramètres Deep Clustering"),

          div(
            numericInput(ns("latent_dim"), "Latent dimension",
                         value=NA, min=2),
            span(class="param-tooltip",
                 icon("question-circle"),
                 span(class="tooltiptext",
                      strong("Dimension latente"), br(),
                      "Compression de l'espace", br(),
                      "• NA = auto (adapté à p et k)", br(),
                      "• Petit (2–3k) : simple", br(),
                      "• Grand (>5k) : complexe", br(),
                      "Recommandé : utiliser 'Params optimaux'")
            )
          ),

          div(
            textInput(ns("hidden_layers"), "Hidden layers",
                      value="64,32"),
            span(class="param-tooltip",
                 icon("question-circle"),
                 span(class="tooltiptext",
                      strong("Couches cachées"), br(),
                      "Architecture encoder/decoder", br(),
                      "• p≤10 : [32,16]", br(),
                      "• p≤30 : [64,32]", br(),
                      "• p>200 : [1024,768,512,256,128]", br(),
                      "Format : nombres séparés par virgule")
            )
          ),

          div(
            numericInput(ns("epochs"), "Epochs",
                         value=60, min=10, max=400, step=10),
            span(class="param-tooltip",
                 icon("question-circle"),
                 span(class="tooltiptext",
                      strong("Nombre d'époques"), br(),
                      "Passages complets sur les données", br(),
                      "• n<150 : 50-80", br(),
                      "• n>1000 : 100-200", br(), br(),
                      strong("Early Stopping :"), br(),
                      "• Activé automatiquement", br(),
                      "• Patience = 10 epochs", br(),
                      "• Monitore val_loss", br(),
                      "• Restaure meilleurs poids", br(),
                      "→ Arrêt anticipé si pas d'amélioration")
            )
          ),

          div(
            numericInput(ns("batch_size"), "Batch size",
                         value=32, min=8, max=128, step=8),
            span(class="param-tooltip",
                 icon("question-circle"),
                 span(class="tooltiptext",
                      strong("Taille de batch"), br(),
                      "Données par mise à jour", br(),
                      "• 8-16 : petits datasets", br(),
                      "• 32-64 : standard", br(),
                      "• 128+ : grands datasets")
            )
          ),

          div(
            selectInput(ns("activation"), "Activation",
                        choices=c("relu","tanh","sigmoid"),
                        selected="relu"),
            span(class="param-tooltip",
                 icon("question-circle"),
                 span(class="tooltiptext",
                      strong("Fonction d'activation"), br(),
                      "• ReLU : standard, rapide", br(),
                      "• Tanh : [-1,1], centré", br(),
                      "• Sigmoid : [0,1], lent")
            )
          ),

          div(
            selectInput(ns("loss"), "Loss function",
                        choices=c("mse","mae","huber"),
                        selected="mse"),
            span(class="param-tooltip",
                 icon("question-circle"),
                 span(class="tooltiptext",
                      strong("Fonction de perte"), br(),
                      "• MSE : standard", br(),
                      "• MAE : robuste", br(),
                      "• Huber : outliers", br(),
                      "Détecte outliers automatiquement")
            )
          ),

          div(
            sliderInput(ns("dropout"), "Dropout",
                        min=0, max=0.5, value=0.05, step=0.05),
            span(class="param-tooltip",
                 icon("question-circle"),
                 span(class="tooltiptext",
                      strong("Dropout rate"), br(),
                      "% neurones désactivés aléatoirement", br(),
                      "• 0.05-0.10 : standard", br(),
                      "• 0.15-0.20 : datasets complexes (p>100)", br(),
                      "• >0.30 : risque underfitting", br(),
                      "Technique de régularisation : réduit l'overfitting", br(),
                      "Appliqué après chaque couche cachée pendant l'entraînement")
            )
          ),

          div(
            numericInput(ns("l2_reg"), "L2 regularization",
                         value=0.001, min=0, step=1e-4),
            span(class="param-tooltip",
                 icon("question-circle"),
                 span(class="tooltiptext",
                      strong("Régularisation L2"), br(),
                      "Pénalise poids élevés", br(),
                      "• 0.0001-0.01 : range", br(),
                      "• Plus grand = plus régularisé", br(),
                      "Trade-off biais/variance")
            )
          )
        )

      } else if (input$algorithm == "kmeans") {
        tagList(
          h4("Paramètres K-means Réallocatif"),

          numericInput(ns("max_iter_kmeans"), "Max iterations",
                       value=100, min=10, max=500),

          checkboxInput(ns("scale_kmeans"), "Standardiser les variables", TRUE),

          numericInput(ns("seed_kmeans"), "Seed (reproductibilité)",
                       value=42, min=1),

          helpText("• Méthode : Vigneau & Qannari (2003)"),
          helpText("• Centroïde de chaque cluster = PC1 (ACP sur les variables du cluster)"),
          helpText("• Distance : d(j,k) = 1 - cor(X_j, PC1_k)^2")
        )

      } else if (input$algorithm == "quali_varclus") {
        tagList(
          h4("Paramètres VARCLUS Qualitatif"),

          numericInput(ns("max_iter_quali"), "Max iterations",
                       value=20, min=5, max=200),

          numericInput(ns("seed_quali"), "Seed",
                       value=123, min=1),

          checkboxInput(ns("auto_k_quali"), "Sélection automatique de k (interne à VARCLUS)", FALSE),

          conditionalPanel(
            condition = "input.auto_k_quali == true",
            ns = ns,
            numericInput(ns("k_max_quali"), "k maximum à tester (VARCLUS)",
                         value=8, min=3, max=20)
          ),

          helpText("• Entrée : variables catégorielles uniquement (factor/character)."),
          helpText("• Basé sur MCA + rapport de corrélation η².")
        )
      }
    })

    output$algo_buttons_ui <- renderUI({
      req(input$algorithm)

      if (input$algorithm == "deep") {
        tagList(
          actionButton(ns("auto_params"), "✨ Params optimaux",
                       icon=icon("magic"), class="btn-info btn-block"),
          actionButton(ns("auto_tune"), "🎯 Auto-tuning",
                       icon=icon("sliders-h"), class="btn-warning btn-block")
        )
      } else {
        helpText("Auto-params & auto-tuning disponibles pour Deep uniquement.")
      }
    })

    # ============================================================
    # RÉSUMÉ ALGO (DOC + FORMULES)
    # ============================================================

    output$algo_summary <- renderUI({
      req(input$algorithm)

      if (input$algorithm == "deep") {

        withMathJax(
          tagList(
            div(class="algo-summary",

                h3(icon("brain"), " ClustDeepVar – Deep Variable Clustering"),

                p(strong("Objectif : "),
                  "clustering de variables en apprenant pour chacune un ",
                  strong("embedding latent non-linéaire"),
                  " via un autoencodeur entraîné sur ", strong("X^T"), "."),

                div(class="metric-card",
                    h4(icon("stream"), " Étape 1 — Standardisation"),
                    p("Chaque variable est centrée-réduite avant l’apprentissage."),
                    div(class="metric-formula",
                        HTML("\\( X_{std} = (X - \\mu) / \\sigma \\)")
                    ),
                    div(class="metric-usage",
                        strong("But : "), "rendre toutes les variables comparables.")
                ),

                div(class="metric-card",
                    h4(icon("exchange-alt"), " Étape 2 — Transposition (variables = observations)"),
                    p("On entraîne l’autoencodeur sur ",
                      strong("X^T"),
                      " : chaque variable devient une « observation » composée de ",
                      strong("n"),
                      " valeurs (les observations d’origine)."),
                    div(class="metric-formula",
                        HTML("\\( X^T \\in \\mathbb{R}^{p \\times n} \\)")),
                    div(class="metric-usage",
                        strong("But : "),
                        "apprendre une signature latente propre à chaque variable.")
                ),

                div(class="metric-card",
                    h4(icon("compress"), " Étape 3 — Encodeur : embeddings des variables"),
                    p("L’encodeur réduit chaque variable à un vecteur latent de dimension ",
                      strong("d = latent_dim"), "."),
                    div(class="metric-formula",
                        HTML("\\( Z = f_{enc}(X^T), \\quad Z \\in \\mathbb{R}^{p \\times d} \\)")),
                    div(class="metric-usage",
                        strong("Interprétation : "),
                        "chaque variable est représentée par un embedding latent compact.")
                ),

                div(class="metric-card",
                    h4(icon("expand"), " Étape 4 — Decoder & Reconstruction"),
                    p("L’autoencodeur reconstruit chaque variable à partir de son embedding :"),
                    div(class="metric-formula",
                        HTML("\\( \\hat{X}^T = f_{dec}(Z) \\)")),
                    div(class="metric-usage",
                        strong("But : "),
                        "forcer le réseau à capturer l’information essentielle de chaque variable.")
                ),

                div(class="metric-card",
                    h4(icon("bullseye"), " Fonction de perte"),
                    p("L’autoencodeur minimise l’erreur de reconstruction avec régularisation L2 :"),
                    div(class="metric-formula",
                        HTML("
                \\[
                \\min_{\\theta}
                \\frac{1}{p}
                \\sum_{j=1}^{p}
                \\mathcal{L}(x_j, \\hat{x}_j)
                + \\lambda \\Vert \\theta \\Vert_2^2
                \\]
            ")),
                    tags$ul(
                      tags$li(HTML("<b>\\( \\mathcal{L} \\)</b> = MSE, MAE ou Huber.")),
                      tags$li(HTML("<b>\\(\\lambda\\)</b> = régularisation L2.")),
                      tags$li("θ = ensemble des poids du réseau.")
                    ),
                    div(class="metric-usage",
                        strong("Objectif : "),
                        "obtenir un espace latent stable, régularisé et robuste.")
                ),

                div(class="metric-card",
                    h4(icon("shield-alt"), " Régularisation et Robustesse"),
                    p("Techniques de régularisation pour éviter le surapprentissage :"),
                    tags$ul(
                      tags$li(HTML("<b>Dropout</b> : désactivation aléatoire de neurones (taux = dropout)."), br(),
                              "• 0.05-0.10 : standard", br(),
                              "• >0.20 : datasets complexes", br(),
                              "• Appliqué après chaque couche cachée"),
                      tags$li(HTML("<b>Régularisation L2</b> : pénalité λ ||θ||²₂ sur les poids."), br(),
                              "• 0.001 : standard (p≤100)", br(),
                              "• 0.01-0.02 : gros datasets (p>200)", br(),
                              "• Réduit la complexité du modèle"),
                      tags$li(HTML("<b>Early Stopping</b> : arrêt automatique de l'entraînement."), br(),
                              "• Patience = 10 epochs", br(),
                              "• Monitore val_loss", br(),
                              "• Restaure les meilleurs poids")
                    ),
                    div(class="metric-usage",
                        strong("But : "),
                        "trade-off optimal biais/variance et prévention de l'overfitting.")
                ),

                div(class="metric-card",
                    h4(icon("project-diagram"), " Étape 5 — Clustering des embeddings"),
                    p("Les embeddings des variables sont clusterisés via k-means :"),
                    div(class="metric-formula",
                        HTML("\\( \\text{k-means}(Z) \\)")),
                    div(class="metric-usage",
                        strong("Interprétation : "),
                        "des variables proches dans l’espace latent sont regroupées.")
                ),

                div(class="metric-card",
                    h4(icon("layer-group"), " Étape 6 — Soft-clustering (probabilités)"),
                    p("Les distances aux centres sont converties en probabilités via un softmax :"),
                    div(class="metric-formula",
                        HTML("
                \\[
                P_{jk} =
                \\frac{ \\exp(-d_{jk}/T) }
                     { \\sum_{c} \\exp(-d_{jc}/T) }
                \\]
            ")),
                    div(class="metric-usage",
                        strong("But : "),
                        "obtenir une appartenance floue des variables aux clusters.")
                ),

                div(class="metric-card",
                    h4(icon("project-diagram"), " Projection de variables illustratives"),
                    p("Une variable illustrative (numérique ou factorielle) est projetée via :"),
                    div(class="metric-formula",
                        HTML("
                \\[
                z_{illu} =
                \\frac{ \\sum_{j=1}^p
                    cor(x_j, v) \\, z_j }
                     { \\left\\lVert
                        \\sum_{j=1}^p
                        cor(x_j, v) \\, z_j
                       \\right\\rVert }
                \\]
            ")),
                    div(class="metric-usage",
                        strong("But : "),
                        "situer graphiquement une variable illustrative parmi les clusters.")
                ),

                div(class="metric-card",
                    h4(icon("star"), " Avantages Spécifiques & Points Forts"),
                    tags$ul(
                      tags$li(strong("Relations non-linéaires : "),
                              "capture des structures complexes inaccessibles aux méthodes linéaires (PCA, corrélation)."),
                      tags$li(strong("Embeddings robustes : "),
                              "goulot d'étranglement + régularisation (L2, dropout, early stopping)."),
                      tags$li(strong("Soft-clustering : "),
                              "probabilités d'appartenance via softmax (analyse d'incertitude)."),
                      tags$li(strong("Projection illustratives : "),
                              "intégration de variables externes pour interprétation."),
                      tags$li(strong("Prédiction : "),
                              "possibilité de projeter de nouvelles variables dans l'espace latent."),
                      tags$li(strong("Scalabilité : "),
                              "adapté aux gros datasets (p>200) avec architecture profonde.")
                    )
                ),

                p(strong("Référence : "),
                  "Deep Embedded Clustering (Xie et al., 2016) adapté au ",
                  strong("clustering de variables"), ".")
            )
          )
        )

      } else if (input$algorithm == "kmeans") {

        withMathJax(
          tagList(
            div(class="algo-summary",
                h3(icon("arrows-alt"), " K-means Réallocatif – Vigneau & Qannari"),

                p("Clustering de variables numériques basé sur la corrélation avec une ",
                  strong("synthetic variable"), " (PC1) par cluster."),

                div(class="metric-card",
                    h4(icon("stream"), " Étape 1 — Standardisation (optionnelle)"),
                    p("Les variables sont éventuellement centrées-réduites :"),
                    div(class="metric-formula",
                        HTML("\\( X_{std} = (X - \\mu) / \\sigma \\)")),
                    div(class="metric-usage",
                        strong("But : "), "rendre les variables comparables en échelle.")
                ),

                div(class="metric-card",
                    h4(icon("layer-group"), " Étape 2 — Initialisation des clusters"),
                    p("On partitionne les variables en ", strong("k"), " groupes initiaux (ex : k-means classique ou tirage aléatoire).")
                ),

                div(class="metric-card",
                    h4(icon("chart-line"), " Étape 3 — Synthetic variable par ACP"),
                    p("Pour chaque cluster ", strong("g"), ", on calcule la première composante principale :"),
                    div(class="metric-formula",
                        HTML("\\( y_g = \\text{PC1}(X_{j \\in g}) \\)")),
                    div(class="metric-usage",
                        strong("Interprétation : "),
                        "y_g résume au mieux les variables du cluster g.")
                ),

                div(class="metric-card",
                    h4(icon("compress-arrows-alt"), " Étape 4 — Distance & réallocation"),
                    p("Pour chaque variable ", strong("X_j"),
                      ", la distance au cluster g est :"),
                    div(class="metric-formula",
                        HTML("
              \\[
              d(j,g) = 1 - cor(X_j, y_g)^2
              \\]
            ")),
                    div(class="metric-usage",
                        strong("Règle : "),
                        "X_j est affectée au cluster avec la distance d(j,g) minimale.")
                ),

                div(class="metric-card",
                    h4(icon("sync"), " Étape 5 — Itérations"),
                    p("On alterne :"),
                    tags$ul(
                      tags$li("Recalcul des synthetic variables y_g par ACP."),
                      tags$li("Réallocation des variables via d(j,g)."),
                      tags$li("Jusqu’à convergence de la partition ou max_iter.")
                    )
                ),

                div(class="metric-card",
                    h4(icon("percent"), " Critère Q = B/T"),
                    p("On décompose l’inertie totale T en "),
                    tags$ul(
                      tags$li("Inertie intra-cluster : W"),
                      tags$li("Inertie inter-clusters : B")
                    ),
                    div(class="metric-formula",
                        HTML("
              \\[
              Q = \\frac{B}{T}, \\quad T = B + W
              \\]
            ")),
                    div(class="metric-usage",
                        strong("Interprétation : "),
                        "plus Q est proche de 1, plus les clusters sont bien séparés.")
                ),

                div(class="metric-card",
                    h4(icon("star"), " Avantages"),
                    tags$ul(
                      tags$li("Interprétation directe via corrélation avec la synthetic variable."),
                      tags$li("Adapté aux variables fortement corrélées."),
                      tags$li("Mise en œuvre efficace sur p modéré.")
                    )
                )
            )
          )
        )

      } else if (input$algorithm == "quali_varclus") {

        withMathJax(
          tagList(
            div(class="algo-summary",
                h3(icon("list-alt"), " VARCLUS Qualitatif – MCA & η²"),

                p("Clustering de variables catégorielles basé sur l’",
                  strong("Analyse des Correspondances Multiples (MCA)"),
                  " et le ", strong("rapport de corrélation η²"), "."),

                div(class="metric-card",
                    h4(icon("table"), " Étape 1 — Encodage disjonctif complet (implicite)"),
                    p("Chaque variable catégorielle est transformée en indicatrices (one-hot) :"),
                    div(class="metric-formula",
                        HTML("\\( X \\Rightarrow Z \\in \\{0,1\\}^{n \\times m} \\)")),
                    div(class="metric-usage",
                        strong("But : "),
                        "représenter chaque modalité comme variable binaire.", br(), br(),
                        strong("⚙️ Note technique : "),
                        "L'encodage one-hot est réalisé ",
                        strong("automatiquement"),
                        " par FactoMineR::MCA. ",
                        "Pas besoin de coder explicitement les indicatrices.")
                ),

                div(class="metric-card",
                    h4(icon("project-diagram"), " Étape 2 — MCA par cluster"),
                    p("Pour un cluster de variables, on réalise une MCA sur les colonnes associées."),
                    div(class="metric-formula",
                        HTML("L’axe principal de la MCA synthétise le cluster."))
                ),

                div(class="metric-card",
                    h4(icon("chart-area"), " Étape 3 — Rapport de corrélation η²"),
                    p("Pour une variable qualitative V et un axe factoriel Y :"),
                    div(class="metric-formula",
                        HTML("
              \\[
              \\eta^2(V, Y) = \\frac{\\text{Var}(\\mathbb{E}[Y \\mid V])}{\\text{Var}(Y)}
              \\in [0,1]
              \\]
            ")),
                    div(class="metric-usage",
                        strong("Interprétation : "),
                        "η² proche de 1 ⇒ V bien expliquée par l’axe du cluster.")
                ),

                div(class="metric-card",
                    h4(icon("expand-arrows-alt"), " Étape 4 — Distance entre variables (implicite)"),
                    p("La distance entre deux variables qualitatives peut être dérivée de η² ou de Cramér’s V."),
                    div(class="metric-formula",
                        HTML("
              \\[
              d(V_1, V_2) = 1 - \\eta^2(V_1, V_2)
              \\quad \\text{ou} \\quad
              d(V_1, V_2) = 1 - V_C(V_1, V_2)
              \\]
            ")),
                    div(class="metric-usage",
                        strong("⚙️ Note d'implémentation : "), br(),
                        "L'algorithme VARCLUS utilise ", strong("η² directement"),
                        " via l'ACM. ", br(),
                        "La distance n'est pas calculée explicitement ",
                        "(clustering indirect via axes factoriels).", br(),
                        "Cramér's V est mentionné comme ",
                        strong("alternative théorique"),
                        " mais non utilisé dans cette implémentation.")
                ),

                div(class="metric-card",
                    h4(icon("sync"), " Étape 5 — Itérations VARCLUS"),
                    tags$ul(
                      tags$li("Initialisation d’une partition en k groupes."),
                      tags$li("MCA par cluster."),
                      tags$li("Réallocation des variables selon η² / distance."),
                      tags$li("Arrêt à convergence ou max_iter.")
                    )
                ),

                div(class="metric-card",
                    h4(icon("star"), " Sélection de k"),
                    p("Deux approches :"),
                    tags$ul(
                      tags$li("Sélection interne à VARCLUS (option auto_k_quali)."),
                      tags$li("Heuristiques externes (Silhouette, Davies-Bouldin, etc.) basées sur une matrice de similarité qualitative.")
                    )
                ),

                div(class="metric-card",
                    h4(icon("star"), " Avantages"),
                    tags$ul(
                      tags$li("Spécifique aux variables catégorielles."),
                      tags$li("Utilise la structure χ² via MCA / Cramér’s V."),
                      tags$li("Partitions interprétables via les axes factoriels.")
                    )
                )
            )
          )
        )
      }
    })

    # ============================================================
    # FONCTIONS HELPER COMMUNES
    # ============================================================

    compute_dunn <- function(emb, cl){
      cl <- as.integer(cl)
      k <- sort(unique(cl))
      dist_mat <- as.matrix(dist(emb))

      max_intra <- max(sapply(k, function(kk){
        idx <- which(cl==kk)
        if(length(idx) < 2) return(0)
        max(dist_mat[idx, idx, drop=FALSE])
      }))

      min_inter <- min(sapply(k, function(ki){
        sapply(k, function(kj){
          if(ki==kj) return(Inf)
          idx_i <- which(cl==ki)
          idx_j <- which(cl==kj)
          min(dist_mat[idx_i, idx_j, drop=FALSE])
        })
      }))

      if(max_intra == 0) return(NA)
      min_inter / max_intra
    }

    compute_dbi <- function(emb, cl){
      cl <- as.integer(cl)
      k <- sort(unique(cl))

      centers <- do.call(rbind, lapply(k, function(kk){
        colMeans(emb[cl==kk, , drop=FALSE])
      }))

      S <- sapply(k, function(kk){
        idx <- which(cl==kk)
        if(length(idx)==1) return(0)
        mean(sqrt(rowSums((emb[idx,,drop=FALSE] - centers[kk,])^2)))
      })

      M <- as.matrix(dist(centers))
      DB <- mean(sapply(k, function(i){
        max(sapply(k, function(j){
          if(i==j) return(-Inf)
          if(M[i,j]==0) return(Inf)
          (S[i] + S[j]) / M[i,j]
        }))
      }))
      DB
    }

    normalize01 <- function(x){
      if(is.na(x) || is.infinite(x)) return(NA)
      pmin(pmax(x,0),1)
    }

    parse_hidden_layers <- function(x){
      hs <- gsub("\\s","",x)
      as.integer(strsplit(hs, ",")[[1]])
    }

    detect_outliers <- function(X){
      any(sapply(X, function(col){
        if(!is.numeric(col)) return(FALSE)
        q <- quantile(col, c(.25,.75), na.rm=TRUE)
        iqr <- q[2]-q[1]
        any(col < q[1]-3*iqr | col > q[2]+3*iqr, na.rm=TRUE)
      }))
    }

    # ============================================================
    # recommend_params POUR DEEP
    # ============================================================

    recommend_params <- function(X, k) {
      p <- ncol(X)
      n <- nrow(X)

      outl <- detect_outliers(X)

      # 1. LATENT DIMENSION
      if (p <= 10) {
        latent_dim <- max(2, min(3*k, p-1))
      } else if (p <= 30) {
        latent_dim <- max(2*k, min(3*k, round(p * 0.3)))
      } else if (p <= 100) {
        latent_dim <- max(3*k, min(5*k, round(p * 0.2)))
      } else if (p <= 300) {
        latent_dim <- max(5*k, min(8*k, round(p * 0.15)))
      } else {
        latent_dim <- max(8*k, min(15*k, round(p * 0.1)))
        latent_dim <- min(latent_dim, 100)
      }

      # 2. HIDDEN LAYERS
      if (p <= 10) {
        hidden_layers <- c(32, 16)
      } else if (p <= 30) {
        hidden_layers <- c(64, 32)
      } else if (p <= 50) {
        hidden_layers <- c(128, 64, 32)
      } else if (p <= 100) {
        hidden_layers <- c(256, 128, 64)
      } else if (p <= 200) {
        hidden_layers <- c(512, 256, 128, 64)
      } else if (p <= 400) {
        hidden_layers <- c(768, 512, 256, 128)
      } else {
        hidden_layers <- c(1024, 768, 512, 256, 128)
      }

      # 3. EPOCHS
      base_epochs <- if (n < 150) 60
      else if (n < 500) 80
      else if (n < 1000) 100
      else 120

      if (p <= 30) {
        epochs <- base_epochs
      } else if (p <= 100) {
        epochs <- base_epochs + 20
      } else if (p <= 200) {
        epochs <- base_epochs + 40
      } else {
        epochs <- base_epochs + 60
      }
      epochs <- min(epochs, 200)

      # 4. BATCH SIZE
      if (p <= 30) {
        batch_size <- if (n < 150) 16 else 32
      } else if (p <= 100) {
        batch_size <- if (n < 500) 16 else 24
      } else {
        batch_size <- if (n < 500) 8 else 16
      }

      # 5. DROPOUT
      if (p <= 30) {
        dropout <- if (n < 150) 0.10 else 0.05
      } else if (p <= 100) {
        dropout <- 0.10
      } else if (p <= 200) {
        dropout <- 0.15
      } else {
        dropout <- 0.20
      }

      # 6. L2 REG
      if (p <= 30) {
        l2_reg <- 0.001
      } else if (p <= 100) {
        l2_reg <- 0.005
      } else if (p <= 200) {
        l2_reg <- 0.01
      } else {
        l2_reg <- 0.02
      }

      # 7. LOSS
      loss <- if (p > 100 || outl) "huber" else "mse"

      # 8. ACTIVATION
      activation <- "relu"

      list(
        latent_dim = latent_dim,
        hidden_layers = hidden_layers,
        epochs = epochs,
        batch_size = batch_size,
        activation = activation,
        dropout = dropout,
        l2_reg = l2_reg,
        loss = loss
      )
    }

    # ============================================================
    # FONCTIONS POUR K-OPT (NUM & QUALI)
    # ============================================================

    cramers_v <- function(x, y){
      x <- factor(x); y <- factor(y)
      tbl <- table(x, y)
      if (nrow(tbl) < 2 || ncol(tbl) < 2) return(0)
      suppressWarnings({
        chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
      })
      n <- sum(tbl)
      k <- min(nrow(tbl), ncol(tbl))
      as.numeric(sqrt(chi2 / (n * (k - 1))))
    }

    assoc_mat_quali <- function(X){
      p <- ncol(X)
      if (p < 2) return(NULL)
      X[] <- lapply(X, factor)
      M <- matrix(1, nrow=p, ncol=p)
      colnames(M) <- rownames(M) <- colnames(X)
      for (i in 1:(p-1)){
        for (j in (i+1):p){
          v <- cramers_v(X[[i]], X[[j]])
          M[i,j] <- M[j,i] <- v
        }
      }
      diag(M) <- 1
      M
    }

    prepare_dist_matrix <- function(X, algo){
      if (algo %in% c("deep","kmeans")) {
        num_vars <- sapply(X, is.numeric)
        X_num <- X[, num_vars, drop = FALSE]
        if (ncol(X_num) < 3) return(NULL)
        cor_mat <- suppressWarnings(cor(X_num, use = "pairwise.complete.obs"))
        dist_mat <- as.dist(1 - abs(cor_mat))
        list(data_mat = as.matrix(dist_mat))

      } else if (algo == "quali_varclus") {
        quali_vars <- sapply(X, function(x) is.factor(x) || is.character(x))
        X_q <- X[, quali_vars, drop = FALSE]
        if (ncol(X_q) < 3) return(NULL)
        A <- assoc_mat_quali(X_q)
        if (is.null(A)) return(NULL)
        dist_mat <- as.dist(1 - A)
        list(data_mat = as.matrix(dist_mat))
      } else {
        NULL
      }
    }

    detect_elbow <- function(wss) {
      if (length(wss) < 3) return(2)
      wss_norm <- (wss - min(wss)) / (max(wss) - min(wss))
      d2 <- diff(diff(wss_norm))
      elbow_idx <- which.max(abs(d2)) + 1
      optimal_k <- elbow_idx + 1
      optimal_k
    }

    compute_elbow_optimal_k <- function(X, max_k = 10, algo = "deep") {
      prep <- prepare_dist_matrix(X, algo)
      if (is.null(prep)) return(NULL)
      data_mat <- prep$data_mat

      max_k <- min(max_k, nrow(data_mat) - 1)
      if (max_k < 2) return(NULL)

      wss <- sapply(2:max_k, function(k) {
        km <- kmeans(data_mat, centers = k, nstart = 25)
        km$tot.withinss
      })

      optimal_k <- detect_elbow(wss)

      list(
        method = "Elbow",
        k_values = 2:max_k,
        wss = wss,
        optimal_k = optimal_k,
        score = -wss[optimal_k - 1]
      )
    }

    compute_silhouette_optimal_k <- function(X, max_k = 10, algo = "deep") {
      if (!requireNamespace("cluster", quietly = TRUE)) {
        return(NULL)
      }
      prep <- prepare_dist_matrix(X, algo)
      if (is.null(prep)) return(NULL)
      data_mat <- prep$data_mat

      max_k <- min(max_k, nrow(data_mat) - 1)
      if (max_k < 2) return(NULL)

      sil_scores <- sapply(2:max_k, function(k) {
        km <- kmeans(data_mat, centers = k, nstart = 25)
        sil <- cluster::silhouette(km$cluster, dist(data_mat))
        mean(sil[, 3])
      })

      optimal_k <- which.max(sil_scores) + 1

      list(
        method = "Silhouette",
        k_values = 2:max_k,
        scores = sil_scores,
        optimal_k = optimal_k,
        score = sil_scores[optimal_k - 1]
      )
    }

    compute_calinski_harabasz <- function(data_mat, clusters) {
      n <- nrow(data_mat)
      k <- length(unique(clusters))

      grand_mean <- colMeans(data_mat)

      centers <- do.call(rbind, lapply(unique(clusters), function(cl) {
        colMeans(data_mat[clusters == cl, , drop = FALSE])
      }))

      cluster_sizes <- table(clusters)
      SSB <- sum(sapply(seq_along(unique(clusters)), function(i) {
        cluster_sizes[i] * sum((centers[i, ] - grand_mean)^2)
      }))

      SSW <- sum(sapply(unique(clusters), function(cl) {
        cluster_data <- data_mat[clusters == cl, , drop = FALSE]
        center <- colMeans(cluster_data)
        sum(apply(cluster_data, 1, function(row) sum((row - center)^2)))
      }))

      if (SSW == 0) return(0)
      CH <- (SSB / (k - 1)) / (SSW / (n - k))

      CH
    }

    compute_ch_optimal_k <- function(X, max_k = 10, algo = "deep") {
      prep <- prepare_dist_matrix(X, algo)
      if (is.null(prep)) return(NULL)
      data_mat <- prep$data_mat

      max_k <- min(max_k, nrow(data_mat) - 1)
      if (max_k < 2) return(NULL)

      ch_scores <- sapply(2:max_k, function(k) {
        km <- kmeans(data_mat, centers = k, nstart = 25)
        compute_calinski_harabasz(data_mat, km$cluster)
      })

      optimal_k <- which.max(ch_scores) + 1

      list(
        method = "Calinski-Harabasz",
        k_values = 2:max_k,
        scores = ch_scores,
        optimal_k = optimal_k,
        score = ch_scores[optimal_k - 1]
      )
    }

    compute_dbi_optimal_k <- function(X, max_k = 10, algo = "deep") {
      prep <- prepare_dist_matrix(X, algo)
      if (is.null(prep)) return(NULL)
      data_mat <- prep$data_mat

      max_k <- min(max_k, nrow(data_mat) - 1)
      if (max_k < 2) return(NULL)

      dbi_scores <- sapply(2:max_k, function(k) {
        km <- kmeans(data_mat, centers = k, nstart = 25)
        compute_dbi(data_mat, km$cluster)
      })

      optimal_k <- which.min(dbi_scores) + 1

      list(
        method = "Davies-Bouldin",
        k_values = 2:max_k,
        scores = dbi_scores,
        optimal_k = optimal_k,
        score = -dbi_scores[optimal_k - 1]
      )
    }

    compute_optimal_k_consensus <- function(X, max_k = 10, algo="deep") {
      results <- list()

      results$elbow <- compute_elbow_optimal_k(X, max_k, algo)
      results$silhouette <- compute_silhouette_optimal_k(X, max_k, algo)
      results$ch <- compute_ch_optimal_k(X, max_k, algo)
      results$dbi <- compute_dbi_optimal_k(X, max_k, algo)

      results <- Filter(Negate(is.null), results)
      if (length(results) == 0) return(NULL)

      optimal_ks <- sapply(results, function(r) r$optimal_k)
      k_table <- table(optimal_ks)
      consensus_k <- as.integer(names(k_table)[which.max(k_table)])
      confidence <- max(k_table) / length(optimal_ks) * 100

      list(
        methods = results,
        consensus_k = consensus_k,
        optimal_ks = optimal_ks,
        confidence = confidence
      )
    }

    # ============================================================
    # OBSERVERS POUR CHOIX DE K
    # ============================================================

    observeEvent(input$compute_elbow, {
      req(data(), active_vars())
      X <- data()[, active_vars(), drop = FALSE]

      withProgress(message = "Calcul Elbow Method...", value = 0, {
        result <- compute_elbow_optimal_k(X, max_k = 10, algo = input$algorithm)
        validate(need(!is.null(result), "Impossible de calculer Elbow (trop peu de variables)."))

        rv$k_optimization <- list(
          methods = list(elbow = result),
          consensus_k = result$optimal_k,
          optimal_ks = c(elbow = result$optimal_k),
          confidence = 100
        )

        rv$suggested_k <- result$optimal_k
      })

      showNotification(
        paste0("✓ Elbow calculé. k optimal suggéré: ", rv$suggested_k),
        type = "message",
        duration = 5
      )
    })

    observeEvent(input$compute_optimal_k, {
      req(data(), active_vars())
      X <- data()[, active_vars(), drop = FALSE]

      withProgress(message = "Calcul Multi-méthodes...", value = 0, {

        incProgress(0.6, detail = "Calcul des critères...")
        consensus <- compute_optimal_k_consensus(X, max_k = 10, algo = input$algorithm)
        validate(need(!is.null(consensus), "Impossible de calculer les critères (trop peu de variables)."))

        rv$k_optimization <- consensus
        rv$suggested_k <- consensus$consensus_k
      })

      showNotification(
        paste0("✓ k optimal suggéré: ", rv$suggested_k,
               " (confiance: ", round(rv$k_optimization$confidence, 0), "%)"),
        type = "message",
        duration = 5
      )
    })

    observeEvent(input$apply_suggested_k, {
      req(rv$suggested_k)
      updateNumericInput(session, "n_clusters", value = rv$suggested_k)
      showNotification(
        paste0("✓ k = ", rv$suggested_k, " appliqué"),
        type = "message",
        duration = 3
      )
    })

    # ============================================================
    # OUTPUTS K-OPT
    # ============================================================

    output$suggested_k_badge <- renderUI({
      req(rv$suggested_k)
      div(class = "k-badge",
          paste0("k = ", rv$suggested_k))
    })

    output$suggested_k_display <- renderUI({
      req(rv$suggested_k)

      confidence_text <- if (!is.null(rv$k_optimization$confidence)) {
        paste0(" (", round(rv$k_optimization$confidence, 0), "% confiance)")
      } else {
        ""
      }

      div(class = "k-display",
          h3(paste0("k optimal suggéré: ", rv$suggested_k, confidence_text)))
    })

    output$optimal_k_plot <- renderPlot({
      req(rv$k_optimization)

      methods <- rv$k_optimization$methods

      if (length(methods) == 1) {
        result <- methods[[1]]
        plot(result$k_values,
             if (!is.null(result$wss)) result$wss else result$scores,
             type = "b",
             lwd = 2, pch = 19,
             xlab = "k",
             ylab = if (!is.null(result$wss)) "Within-cluster Sum of Squares" else "Score",
             main = result$method)
        abline(v = result$optimal_k, col = "red", lty = 2, lwd = 2)
        legend("topright", legend = paste("k =", result$optimal_k),
               col = "red", lty = 2, lwd = 2, bty = "n")
        grid()

      } else {
        par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

        if (!is.null(methods$elbow)) {
          plot(methods$elbow$k_values, methods$elbow$wss, type = "b",
               lwd = 2, pch = 19,
               xlab = "k", ylab = "WSS", main = "Elbow Method")
          abline(v = methods$elbow$optimal_k, col = "red", lty = 2, lwd = 2)
          grid()
        }

        if (!is.null(methods$silhouette)) {
          plot(methods$silhouette$k_values, methods$silhouette$scores, type = "b",
               lwd = 2, pch = 19,
               xlab = "k", ylab = "Silhouette", main = "Silhouette")
          abline(v = methods$silhouette$optimal_k, col = "red", lty = 2, lwd = 2)
          grid()
        }

        if (!is.null(methods$ch)) {
          plot(methods$ch$k_values, methods$ch$scores, type = "b",
               lwd = 2, pch = 19,
               xlab = "k", ylab = "CH Index", main = "Calinski-Harabasz")
          abline(v = methods$ch$optimal_k, col = "red", lty = 2, lwd = 2)
          grid()
        }

        if (!is.null(methods$dbi)) {
          plot(methods$dbi$k_values, methods$dbi$scores, type = "b",
               lwd = 2, pch = 19,
               xlab = "k", ylab = "DBI", main = "Davies-Bouldin (↓ mieux)")
          abline(v = methods$dbi$optimal_k, col = "red", lty = 2, lwd = 2)
          grid()
        }

        mtext(paste0("CONSENSUS: k = ", rv$k_optimization$consensus_k,
                     " (", round(rv$k_optimization$confidence, 0), "% confiance)"),
              side = 3, line = -2, outer = TRUE, font = 2, cex = 1.1)
      }
    })

    output$optimal_k_table <- renderDT({
      req(rv$k_optimization)

      if (!requireNamespace("DT", quietly = TRUE)) {
        return(NULL)
      }

      results <- rv$k_optimization$methods

      df <- data.frame(
        Méthode = sapply(results, function(r) r$method),
        `k optimal` = sapply(results, function(r) r$optimal_k),
        Score = sapply(results, function(r) round(r$score, 3)),
        check.names = FALSE
      )

      df <- rbind(df, data.frame(
        Méthode = "CONSENSUS",
        `k optimal` = rv$k_optimization$consensus_k,
        Score = paste0(round(rv$k_optimization$confidence, 0), "%"),
        check.names = FALSE
      ))

      DT::datatable(df,
                    options = list(dom = 't', pageLength = 10),
                    rownames = FALSE) %>%
        DT::formatStyle(
          'Méthode',
          target = 'row',
          backgroundColor = DT::styleEqual('CONSENSUS', '#d4edda')
        )
    })

    # ============================================================
    # QUALITÉ DEEP
    # ============================================================

    evaluate_quality <- function(model){
      emb <- model$embeddings
      cl  <- model$clusters

      sil_mean <- NA
      sil_obj <- NULL

      if(requireNamespace("cluster", quietly=TRUE)){
        sil_obj <- cluster::silhouette(as.integer(cl), dist(emb))
        sil_mean <- mean(sil_obj[,3])
      }

      dbi  <- compute_dbi(emb, cl)
      dunn <- compute_dunn(emb, cl)

      sil_n  <- normalize01(sil_mean)
      dunn_n <- normalize01(dunn / (dunn+1))
      dbi_n  <- ifelse(is.na(dbi), NA, 1/(1+dbi))

      quality <- mean(c(sil_n,dunn_n,dbi_n), na.rm=TRUE)

      list(
        sil_obj = sil_obj,
        sil_mean = sil_mean,
        dbi = dbi,
        dunn = dunn,
        quality = quality
      )
    }

    # ============================================================
    # OBSERVERS : AUTO PARAMS / AUTO TUNE (DEEP)
    # ============================================================

    observeEvent(input$auto_params, {
      req(data(), active_vars(), input$algorithm == "deep")
      X <- data()[, active_vars(), drop=FALSE]

      k <- input$n_clusters
      rec <- recommend_params(X, k)
      rv$best_params <- c(rec, list(n_clusters=k))

      updateNumericInput(session, "latent_dim", value = rec$latent_dim)
      updateTextInput(session, "hidden_layers", value = paste(rec$hidden_layers, collapse=","))
      updateNumericInput(session, "epochs", value = rec$epochs)
      updateNumericInput(session, "batch_size", value = rec$batch_size)
      updateSelectInput(session, "activation", selected = rec$activation)
      updateSliderInput(session, "dropout", value = rec$dropout)
      updateNumericInput(session, "l2_reg", value = rec$l2_reg)
      updateSelectInput(session, "loss", selected = rec$loss)

      showNotification("Paramètres optimaux appliqués ✔", type="message", duration=3)
    })

    observeEvent(input$auto_tune, {
      req(data(), active_vars(), input$algorithm == "deep")
      X <- data()[, active_vars(), drop=FALSE]

      k <- input$n_clusters
      base <- recommend_params(X, k)

      grid <- list(
        list(latent_dim = base$latent_dim, hidden_layers = base$hidden_layers,
             dropout = base$dropout, epochs = base$epochs, loss = base$loss),
        list(latent_dim = max(2, base$latent_dim+1), hidden_layers = base$hidden_layers,
             dropout = base$dropout, epochs = base$epochs+20, loss = base$loss),
        list(latent_dim = max(2, base$latent_dim-1), hidden_layers = base$hidden_layers,
             dropout = min(0.2, base$dropout+0.05), epochs = base$epochs, loss = base$loss)
      )

      best_q <- -Inf; best_m <- NULL; best_p <- NULL

      withProgress(message="Auto-tuning...", value=0, {
        for(i in seq_along(grid)){
          incProgress(1/length(grid), detail=paste("Config", i))
          g <- grid[[i]]

          m_try <- ClustDeepVar$new(
            n_clusters = k,
            latent_dim = g$latent_dim,
            hidden_layers = g$hidden_layers,
            epochs = g$epochs,
            batch_size = base$batch_size,
            activation = base$activation,
            dropout = g$dropout,
            l2_reg = base$l2_reg,
            loss = g$loss
          )

          tryCatch({
            m_try$fit(X, verbose=0)
            met <- evaluate_quality(m_try)
            if(!is.na(met$quality) && met$quality > best_q){
              best_q <- met$quality
              best_m <- m_try
              best_p <- c(base, g, list(n_clusters=k))
              best_p$hidden_layers <- g$hidden_layers
            }
          }, error=function(e){})
        }
      })

      if(is.null(best_m)){
        showNotification("Auto-tuning échoué.", type="error", duration=5)
        return()
      }

      rv$best_params <- best_p
      updateNumericInput(session, "latent_dim", value = best_p$latent_dim)
      updateTextInput(session, "hidden_layers", value = paste(best_p$hidden_layers, collapse=","))
      updateNumericInput(session, "epochs", value = best_p$epochs)
      updateSliderInput(session, "dropout", value = best_p$dropout)
      updateSelectInput(session, "loss", selected = best_p$loss)

      rv$model <- best_m
      rv$clusters <- best_m$clusters
      rv$method <- "deep"

      met <- evaluate_quality(best_m)
      rv$sil_obj  <- met$sil_obj
      rv$sil_mean <- met$sil_mean
      rv$dbi      <- met$dbi
      rv$dunn     <- met$dunn
      rv$quality  <- met$quality

      # Reset autres métriques
      rv$W_total <- NA; rv$B_total <- NA; rv$Q <- NA
      rv$eta2_matrix <- NULL; rv$selection_results <- NULL

      showNotification("Auto-tuning terminé ✔", type="message", duration=4)
    })

    # ============================================================
    # RUN CLUSTERING (3 ALGOS)
    # ============================================================

    observeEvent(input$run_clustering, {
      req(data(), active_vars())
      X <- data()[, active_vars(), drop=FALSE]

      if (input$algorithm == "deep") {

        hidden <- parse_hidden_layers(input$hidden_layers)
        latdim <- if(is.na(input$latent_dim)) NULL else input$latent_dim

        num_vars <- sapply(X, is.numeric)
        if (!all(num_vars)) {
          showNotification("⚠ Colonnes non-numériques exclues automatiquement", type="warning")
          X <- X[, num_vars, drop=FALSE]
        }

        if (ncol(X) < 3) {
          showNotification("❌ Pas assez de variables numériques pour faire un clustering", type="error")
          return(NULL)
        }

        if (nrow(X) < 5) {
          showNotification("❌ Dataset trop petit après nettoyage des NA", type="error")
          return(NULL)
        }

        if (any(is.na(X))) {
          showNotification("❌ Dataset contient encore des NA après nettoyage", type="error")
          return(NULL)
        }

        if (ncol(X) > 200 && is.null(latdim)) {
          showNotification("⚠ Gros dataset détecté : utiliser 'Params optimaux' recommandé", type="warning")
        }

        m <- ClustDeepVar$new(
          n_clusters=input$n_clusters,
          latent_dim=latdim,
          hidden_layers=hidden,
          epochs=input$epochs,
          batch_size=input$batch_size,
          activation=input$activation,
          dropout=input$dropout,
          l2_reg=input$l2_reg,
          loss=input$loss
        )

        m$fit(X, verbose=0)

        rv$model <- m
        rv$clusters <- m$clusters
        rv$method <- "deep"

        met <- evaluate_quality(m)
        rv$sil_obj  <- met$sil_obj
        rv$sil_mean <- met$sil_mean
        rv$dbi      <- met$dbi
        rv$dunn     <- met$dunn
        rv$quality  <- met$quality

        rv$W_total <- NA; rv$B_total <- NA; rv$Q <- NA
        rv$eta2_matrix <- NULL; rv$selection_results <- NULL

        showNotification("ClustDeepVar terminé ✔", type="message")

      } else if (input$algorithm == "kmeans") {

        num_vars <- sapply(X, is.numeric)
        if (!all(num_vars)) {
          showNotification("❌ K-means réallocatif nécessite uniquement des variables numériques.", type="error")
          return(NULL)
        }

        if (ncol(X) < 2) {
          showNotification("❌ Pas assez de variables numériques.", type="error")
          return(NULL)
        }

        m <- ClustKMeansVar$new(
          n_clusters = input$n_clusters,
          max_iter = input$max_iter_kmeans,
          scale = input$scale_kmeans,
          seed = input$seed_kmeans
        )

        m$fit(X)

        rv$model <- m
        rv$clusters <- m$cluster_assignments
        names(rv$clusters) <- m$var_names
        rv$method <- "kmeans"

        rv$W_total <- m$W_total
        rv$B_total <- m$B_total
        rv$Q <- m$Q

        rv$sil_obj <- NULL
        rv$sil_mean <- NA
        rv$dbi <- NA
        rv$dunn <- NA
        rv$quality <- NA
        rv$eta2_matrix <- NULL
        rv$selection_results <- NULL

        showNotification("K-means Réallocatif terminé ✔", type="message")

      } else if (input$algorithm == "quali_varclus") {

        quali_vars <- sapply(X, function(x) is.factor(x) || is.character(x))
        if (!any(quali_vars)) {
          showNotification("❌ VARCLUS Quali nécessite des variables catégorielles.", type="error")
          return(NULL)
        }

        Xq <- X[, quali_vars, drop = FALSE]

        n_clust <- if (isTRUE(input$auto_k_quali)) NULL else input$n_clusters

        m <- ClustQualiVarclus$new(
          n_clusters = n_clust,
          max_iter = input$max_iter_quali,
          seed = input$seed_quali,
          verbose = TRUE,
          k_max = if (isTRUE(input$auto_k_quali)) input$k_max_quali else NULL
        )

        m$fit(Xq)

        rv$model <- m
        rv$clusters <- m$clusters
        rv$method <- "quali_varclus"

        rv$eta2_matrix <- m$eta2_matrix
        rv$selection_results <- m$scores_k_selection

        rv$sil_obj <- NULL
        rv$sil_mean <- NA
        rv$dbi <- NA
        rv$dunn <- NA
        rv$quality <- NA
        rv$W_total <- NA
        rv$B_total <- NA
        rv$Q <- NA

        showNotification("VARCLUS Qualitatif terminé ✔", type="message")
      }
    })

    # ============================================================
    # VALUE BOXES ADAPTATIVES
    # ============================================================

    output$metrics_boxes <- renderUI({
      req(rv$model, rv$method)

      if (rv$method == "deep") {
        fluidRow(
          valueBoxOutput(ns("sil_value"), width=4),
          valueBoxOutput(ns("dbi_value"), width=4),
          valueBoxOutput(ns("dunn_value"), width=4)
        )

      } else if (rv$method == "kmeans") {
        fluidRow(
          valueBoxOutput(ns("q_value"), width=4),
          valueBoxOutput(ns("w_value"), width=4),
          valueBoxOutput(ns("b_value"), width=4)
        )

      } else if (rv$method == "quali_varclus") {
        fluidRow(
          valueBoxOutput(ns("k_value"), width=6),
          valueBoxOutput(ns("iter_value"), width=6)
        )
      }
    })

    output$sil_value <- renderValueBox({
      req(rv$method == "deep")
      valueBox(ifelse(is.na(rv$sil_mean), "NA", round(rv$sil_mean,3)),
               "Silhouette", icon = icon("chart-area"),
               color = ifelse(!is.na(rv$sil_mean) && rv$sil_mean>0.3,"green","yellow"))
    })

    output$dbi_value <- renderValueBox({
      req(rv$method == "deep")
      valueBox(ifelse(is.na(rv$dbi), "NA", round(rv$dbi,3)),
               "Davies–Bouldin", icon = icon("compress-arrows-alt"),
               color = ifelse(!is.na(rv$dbi) && rv$dbi<1.5,"green","yellow"))
    })

    output$dunn_value <- renderValueBox({
      req(rv$method == "deep")
      valueBox(ifelse(is.na(rv$dunn), "NA", round(rv$dunn,3)),
               "Dunn", icon = icon("expand-arrows-alt"),
               color = ifelse(!is.na(rv$dunn) && rv$dunn>0.2,"green","yellow"))
    })

    # K-means metrics
    output$q_value <- renderValueBox({
      req(rv$method == "kmeans")
      valueBox(ifelse(is.na(rv$Q), "NA", round(rv$Q,3)),
               "Critère Q (B/T)", icon = icon("percent"),
               color = ifelse(!is.na(rv$Q) && rv$Q>0.7,"green","yellow"))
    })

    output$w_value <- renderValueBox({
      req(rv$method == "kmeans")
      valueBox(ifelse(is.na(rv$W_total), "NA", round(rv$W_total,2)),
               "Inertie W (intra)", icon = icon("compress"),
               color = "blue")
    })

    output$b_value <- renderValueBox({
      req(rv$method == "kmeans")
      valueBox(ifelse(is.na(rv$B_total), "NA", round(rv$B_total,2)),
               "Inertie B (inter)", icon = icon("expand"),
               color = "green")
    })

    # VARCLUS metrics
    output$k_value <- renderValueBox({
      req(rv$method == "quali_varclus")
      valueBox(rv$model$n_clusters,
               "Nombre de clusters", icon = icon("layer-group"),
               color = "blue")
    })

    output$iter_value <- renderValueBox({
      req(rv$method == "quali_varclus")
      iter_val <- if (!is.null(rv$model$fitted) && rv$model$fitted) {
        "Convergé"
      } else {
        "Non convergé"
      }
      valueBox(iter_val,
               "Convergence", icon = icon("check-circle"),
               color = "green")
    })

    # ============================================================
    # PLOTS DEEP
    # ============================================================

    output$silhouette_plot <- renderPlot({
      req(rv$method == "deep", rv$sil_obj)
      plot(rv$sil_obj, main="Silhouette plot", border=NA)
      abline(v=rv$sil_mean, lty=2, col="red")
    })

    output$quality_ui <- renderUI({
      req(rv$method == "deep", rv$model)
      q <- rv$quality
      txt <- if(is.na(q)) "NA"
      else if(q>0.7) "Excellente"
      else if(q>0.5) "Bonne"
      else if(q>0.3) "Moyenne"
      else "Faible"

      tagList(
        h3(paste0("Score = ", round(q,3))),
        h4(txt),
        tags$ul(
          tags$li(paste0("Silhouette : ", round(rv$sil_mean,3))),
          tags$li(paste0("Dunn : ", round(rv$dunn,3))),
          tags$li(paste0("DBI : ", round(rv$dbi,3)))
        )
      )
    })

    # ============================================================
    # RÉSUMÉ
    # ============================================================

    output$cluster_summary <- renderPrint({
      req(rv$model)
      rv$model$summary()
    })

    # ============================================================
    # RETOUR MODULE
    # ============================================================

    reactive({
      list(
        model=rv$model,
        clusters=rv$clusters,
        method=rv$method,
        # Deep
        silhouette=rv$sil_obj,
        sil_mean=rv$sil_mean,
        dbi=rv$dbi,
        dunn=rv$dunn,
        quality=rv$quality,
        best_params=rv$best_params,
        # k optimisation
        suggested_k=rv$suggested_k,
        k_optimization=rv$k_optimization,
        # K-means
        W_total=rv$W_total,
        B_total=rv$B_total,
        Q=rv$Q,
        # VARCLUS
        eta2_matrix=rv$eta2_matrix,
        selection_results=rv$selection_results
      )
    })
  })
}
