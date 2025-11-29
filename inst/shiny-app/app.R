# =======================================================================
# ClustR Shiny Application - VERSION FINALE PRODUCTION
# =======================================================================
# Supports: Deep Learning, K-means Réallocatif, VARCLUS Qualitative
# Architecture: Modular with unified visualization
# Last Update: 2025-11-27
# =======================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(ClustR)

# =======================================================================
# SOURCE ALL MODULES
# =======================================================================

source("modules/data_upload.R",           local = TRUE)
source("modules/variable_selection.R",    local = TRUE)
source("modules/algorithm_selection.R",   local = TRUE)
source("modules/deep_visualization.R",    local = TRUE)
source("modules/deep_newvariables.R",     local = TRUE)
source("modules/kmeans_visualization.R",  local = TRUE)
source("modules/quali_visualization.R",   local = TRUE)
source("modules/unified_visualization.R", local = TRUE)
source("modules/kmeans_newvariables.R",   local = TRUE)
source("modules/quali_newvariables.R", local = TRUE)


# =======================================================================
# GLOBAL OPTIONS
# =======================================================================

options(shiny.maxRequestSize = 500*1024^2)  # 500 MB max upload

# =======================================================================
# USER INTERFACE
# =======================================================================

ui <- dashboardPage(
  skin = "blue",

  # =====================================================================
  # HEADER
  # =====================================================================
  dashboardHeader(
    title = "ClustR – Variable Clustering",
    titleWidth = 320
  ),

  # =====================================================================
  # SIDEBAR
  # =====================================================================
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Accueil",
               tabName = "home",
               icon = icon("home")),
      menuItem("Données",
               tabName = "data",
               icon = icon("database")),
      menuItem("Variables",
               tabName = "variables",
               icon = icon("check-square")),
      menuItem("Clustering",
               tabName = "clustering",
               icon = icon("project-diagram")),
      menuItem("Visualisation",
               tabName = "visualization",
               icon = icon("chart-line")),
      menuItem("Nouvelles variables",
               tabName = "newvars",
               icon = icon("plus-circle")),
      menuItem("Résultats & Export",
               tabName = "results",
               icon = icon("download")),
      menuItem("Aide",
               tabName = "help",
               icon = icon("question-circle"))
    )
  ),

  # =====================================================================
  # BODY
  # =====================================================================
  dashboardBody(

    # -------------------------------------------------------------------
    # CUSTOM CSS
    # -------------------------------------------------------------------
    tags$head(
      tags$style(HTML("
        /* Global styles */
        .box-title { font-weight: bold; }
        .info-box-text { font-size: 14px; }

        /* Recommendation boxes */
        .recommendation-box {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          border-radius: 8px;
          margin: 10px 0;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }

        .recommendation-box h4 {
          color: white;
          margin-top: 0;
        }

        /* Algorithm cards */
        .algo-card {
          border: 2px solid #3c8dbc;
          border-radius: 8px;
          padding: 20px;
          margin: 10px 0;
          background: white;
          transition: all 0.3s ease;
          height: 100%;
        }

        .algo-card:hover {
          box-shadow: 0 6px 12px rgba(0,0,0,0.15);
          transform: translateY(-2px);
        }

        .algo-card h4 {
          color: #3c8dbc;
          border-bottom: 2px solid #3c8dbc;
          padding-bottom: 10px;
          margin-bottom: 15px;
        }

        /* Algorithm-specific cards */
        .algo-card.kmeans {
          border-color: #00c0ef;
        }

        .algo-card.kmeans h4 {
          color: #00c0ef;
          border-color: #00c0ef;
        }

        .algo-card.quali {
          border-color: #f39c12;
        }

        .algo-card.quali h4 {
          color: #f39c12;
          border-color: #f39c12;
        }

        .algo-card.deep {
          border-color: #9b59b6;
        }

        .algo-card.deep h4 {
          color: #9b59b6;
          border-color: #9b59b6;
        }

        /* Info highlight boxes */
        .info-highlight {
          background: #e3f2fd;
          border-left: 4px solid #2196f3;
          padding: 15px;
          margin: 15px 0;
          border-radius: 4px;
        }

        .info-highlight p {
          margin: 5px 0;
        }

        /* Success messages */
        .success-message {
          background: #d4edda;
          border: 1px solid #c3e6cb;
          color: #155724;
          padding: 12px;
          border-radius: 6px;
          margin: 10px 0;
        }

        /* Warning messages */
        .warning-message {
          background: #fff3cd;
          border: 1px solid #ffeaa7;
          color: #856404;
          padding: 12px;
          border-radius: 6px;
          margin: 10px 0;
        }

        /* Metric displays */
        .metric-container {
          display: flex;
          flex-wrap: wrap;
          gap: 15px;
          margin: 20px 0;
        }

        .metric-item {
          flex: 1;
          min-width: 200px;
          background: #f8f9fa;
          padding: 15px;
          border-radius: 8px;
          text-align: center;
          border: 2px solid #e9ecef;
        }

        .metric-label {
          font-size: 13px;
          color: #666;
          margin-bottom: 8px;
          font-weight: 500;
        }

        .metric-value {
          font-size: 28px;
          font-weight: bold;
          color: #3c8dbc;
        }

        /* Quality indicators */
        .quality-excellent { color: #28a745; }
        .quality-good { color: #17a2b8; }
        .quality-average { color: #ffc107; }
        .quality-poor { color: #dc3545; }

        /* Tables */
        .dataTables_wrapper {
          padding: 0;
        }

        table.dataTable thead th {
          background-color: #3c8dbc;
          color: white;
          font-weight: bold;
          border-bottom: none;
        }

        table.dataTable tbody tr:hover {
          background-color: #f5f5f5;
        }

        /* Buttons */
        .btn-block {
          width: 100%;
          margin: 5px 0;
        }

        /* Value boxes */
        .small-box {
          border-radius: 4px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }

        .small-box h3 {
          font-size: 36px;
          font-weight: bold;
        }

        /* Responsive adjustments */
        @media (max-width: 768px) {
          .algo-card {
            margin-bottom: 15px;
          }

          .metric-item {
            min-width: 150px;
          }
        }
      "))
    ),

    # -------------------------------------------------------------------
    # TAB ITEMS
    # -------------------------------------------------------------------
    tabItems(

      # =================================================================
      # TAB: HOME
      # =================================================================
      tabItem(
        tabName = "home",

        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "🎯 Bienvenue dans ClustR",

            h3(icon("chart-line"), " Plateforme professionnelle de clustering de variables"),

            p("ClustR propose 3 algorithmes de pointe pour l'analyse et le clustering de variables,
               adaptés à différents types de données et objectifs analytiques."),

            hr(),

            h4(icon("cogs"), " Algorithmes disponibles"),

            fluidRow(
              column(
                width = 4,
                div(
                  class = "algo-card kmeans",
                  h4(icon("arrows-alt"), " K-means Réallocatif"),
                  p(strong("Type :"), " Variables numériques"),
                  p(strong("Méthode :"), " Qannari (2003)"),
                  p(strong("Distance :"), " 1 - r²"),
                  p(strong("Centroïde :"), " PC1 du cluster"),
                  hr(),
                  tags$ul(
                    tags$li("✅ Interprétabilité maximale"),
                    tags$li("✅ Calcul rapide"),
                    tags$li("✅ Métrique Q = B/T")
                  )
                )
              ),
              column(
                width = 4,
                div(
                  class = "algo-card quali",
                  h4(icon("list-alt"), " VARCLUS Qualitative"),
                  p(strong("Type :"), " Variables catégorielles"),
                  p(strong("Méthode :"), " Réallocation par η²"),
                  p(strong("Distance :"), " 1 - η²"),
                  p(strong("Métrique :"), " Rapport de corrélation"),
                  hr(),
                  tags$ul(
                    tags$li("✅ Spécifique variables qualitatives"),
                    tags$li("✅ Association η²"),
                    tags$li("✅ Sélection K automatique")
                  )
                )
              ),
              column(
                width = 4,
                div(
                  class = "algo-card deep",
                  h4(icon("brain"), " Deep Learning"),
                  p(strong("Type :"), " Variables numériques"),
                  p(strong("Méthode :"), " Autoencoder + k-means"),
                  p(strong("Distance :"), " Euclidienne (latent)"),
                  p(strong("Avantage :"), " Relations non-linéaires"),
                  hr(),
                  tags$ul(
                    tags$li("✅ Patterns complexes"),
                    tags$li("✅ Variables illustratives"),
                    tags$li("✅ 7 méthodes sélection K")
                  )
                )
              )
            ),

            hr(),

            h4(icon("route"), " Workflow recommandé"),

            tags$ol(
              tags$li(icon("database"), strong(" Données :"),
                      " Uploader votre dataset ou utiliser un exemple intégré"),
              tags$li(icon("check-square"), strong(" Variables :"),
                      " Sélectionner les variables actives (et illustratives pour Deep)"),
              tags$li(icon("project-diagram"), strong(" Clustering :"),
                      " Choisir l'algorithme adapté à votre type de données"),
              tags$li(icon("chart-line"), strong(" Visualisation :"),
                      " Explorer les résultats avec visualisations adaptatives"),
              tags$li(icon("plus-circle"), strong(" Nouvelles variables :"),
                      " Générer des synthèses par cluster (Deep uniquement)"),
              tags$li(icon("download"), strong(" Export :"),
                      " Télécharger résultats et visualisations")
            ),

            hr(),

            div(
              class = "info-highlight",
              p(icon("info-circle"), strong(" Note importante :"),
                " Les visualisations s'adaptent automatiquement à l'algorithme choisi.
                 Chaque méthode propose des graphiques et métriques spécifiques.")
            ),

            br(),

            div(
              style = "text-align: center; margin-top: 30px;",
              actionButton(
                "start_btn",
                "🚀 Commencer l'analyse",
                icon = icon("play"),
                class = "btn-primary btn-lg",
                style = "font-size: 20px; padding: 15px 40px;"
              )
            )
          )
        ),

        # Statistics boxes
        fluidRow(
          valueBox(
            value = "3",
            subtitle = "Algorithmes disponibles",
            icon = icon("cogs"),
            color = "blue",
            width = 3
          ),
          valueBox(
            value = "Deep",
            subtitle = "Variables illustratives supportées",
            icon = icon("brain"),
            color = "purple",
            width = 3
          ),
          valueBox(
            value = "15+",
            subtitle = "Types de visualisations",
            icon = icon("chart-line"),
            color = "green",
            width = 3
          ),
          valueBox(
            value = "Multi",
            subtitle = "Formats d'export",
            icon = icon("download"),
            color = "orange",
            width = 3
          )
        )
      ),

      # =================================================================
      # TAB: DATA UPLOAD
      # =================================================================
      tabItem(
        tabName = "data",
        data_upload_ui("data_upload")
      ),

      # =================================================================
      # TAB: VARIABLE SELECTION
      # =================================================================
      tabItem(
        tabName = "variables",
        variable_selection_ui("var_select")
      ),

      # =================================================================
      # TAB: ALGORITHM SELECTION
      # =================================================================
      tabItem(
        tabName = "clustering",
        algorithm_selection_ui("algo_select")
      ),

      # =================================================================
      # TAB: VISUALIZATION (UNIFIED - Adaptatif)
      # =================================================================
      tabItem(
        tabName = "visualization",
        unified_visualization_ui("unified_viz")
      ),

      # =================================================================
      # TAB: NEW VARIABLES (Deep + K-means)
      # =================================================================
      tabItem(
        tabName = "newvars",

        # Deep
        conditionalPanel(
          condition = "output.current_method == 'deep'",
          deep_newvariables_ui("new_vars_deep")
        ),

        # K-means
        conditionalPanel(
          condition = "output.current_method == 'kmeans'",
          kmeans_newvariables_ui("new_vars_kmeans")
        ),

        # VARCLUS Quali
        conditionalPanel(
          condition = "output.current_method == 'quali_varclus'",
          quali_newvariables_ui("new_vars_quali")
        ),

        # Autres méthodes / rien
        conditionalPanel(
          condition = "!(output.current_method %in% c('deep', 'kmeans'))",
          box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = "Nouvelles variables",
            p("La création/projection de nouvelles variables est disponible pour :"),
            tags$ul(
              tags$li("Deep Variable Clustering"),
              tags$li("K-means Réallocatif")
            ),
            p("Méthode actuelle : ", strong(textOutput("current_method", inline = TRUE)))
          )
        )
      ),

      # =================================================================
      # TAB: RESULTS & EXPORT
      # =================================================================
      tabItem(
        tabName = "results",

        # Model summary
        fluidRow(
          box(
            width = 12,
            status = "success",
            solidHeader = TRUE,
            title = icon("info-circle", " Résumé du modèle"),
            verbatimTextOutput("results_summary")
          )
        ),

        # Quality metrics (adaptatif selon algorithme)
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = icon("tachometer-alt", " Métriques de qualité"),
            uiOutput("quality_metrics_display")
          )
        ),

        # Tables
        fluidRow(
          box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = icon("table", " Assignations des clusters"),
            DTOutput("cluster_table")
          ),
          box(
            width = 6,
            status = "info",
            solidHeader = TRUE,
            title = icon("chart-pie", " Statistiques par cluster"),
            DTOutput("stats_table")
          )
        ),

        # Detailed table
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            title = icon("list-alt", " Tableau détaillé complet"),
            DTOutput("detailed_table")
          )
        ),

        # Export options
        fluidRow(
          box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = icon("download", " Options d'export"),

            p("Téléchargez vos résultats dans différents formats :"),

            br(),

            fluidRow(
              column(
                width = 4,
                downloadButton(
                  "download_clusters_csv",
                  "📊 Clusters (CSV simple)",
                  class = "btn-primary btn-block"
                ),
                p(class = "help-block",
                  "Liste des variables avec leur cluster")
              ),
              column(
                width = 4,
                downloadButton(
                  "download_full_table",
                  "📋 Tableau complet (CSV)",
                  class = "btn-info btn-block"
                ),
                p(class = "help-block",
                  "Inclut toutes les informations détaillées")
              ),
              column(
                width = 4,
                downloadButton(
                  "download_report_html",
                  "📄 Rapport complet (HTML)",
                  class = "btn-success btn-block"
                ),
                p(class = "help-block",
                  "Rapport professionnel avec design")
              )
            )
          )
        )
      ),

      # =================================================================
      # TAB: HELP
      # =================================================================
      tabItem(
        tabName = "help",

        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = icon("question-circle", " Guide des algorithmes"),

            # K-means
            h3(icon("arrows-alt"), " K-means Réallocatif (Qannari)"),

            div(
              class = "info-highlight",
              h4("Principe"),
              tags$ul(
                tags$li(strong("Type de données :"), " Variables numériques uniquement"),
                tags$li(strong("Méthode :"), " Réallocation itérative par corrélation avec PC1"),
                tags$li(strong("Distance :"), " 1 - r² (coefficient de détermination)"),
                tags$li(strong("Centroïde :"), " Première composante principale de chaque cluster"),
                tags$li(strong("Convergence :"), " Itérations jusqu'à stabilité des assignations")
              )
            ),

            h4("Métriques disponibles :"),
            tags$ul(
              tags$li(strong("Critère Q = B/T :"), " Ratio inertie inter/totale (>0.7 excellent, >0.5 bon)"),
              tags$li(strong("Inertie W :"), " Somme des inerties intra-cluster"),
              tags$li(strong("Inertie B :"), " Inertie inter-clusters"),
              tags$li(strong("Silhouette :"), " Basé sur distance 1-r² (calculé sur demande)")
            ),

            h4("Visualisations :"),
            tags$ul(
              tags$li("📊 PCA globale : Variables projetées avec clusters colorés"),
              tags$li("📈 Silhouette : Graphique par variable et moyenne"),
              tags$li("🔍 PCA par cluster : Analyse intra-cluster détaillée"),
              tags$li("🌳 Dendrogramme : Hiérarchie des centroïdes"),
              tags$li("🔥 Heatmap : Matrice de corrélations annotée")
            ),

            hr(),

            # VARCLUS
            h3(icon("list-alt"), " VARCLUS Qualitative"),

            div(
              class = "info-highlight",
              h4("Principe"),
              tags$ul(
                tags$li(strong("Type de données :"), " Variables catégorielles (facteurs) uniquement"),
                tags$li(strong("Méthode :"), " Réallocation par maximisation du η²"),
                tags$li(strong("Distance :"), " 1 - η² (rapport de corrélation)"),
                tags$li(strong("Variable latente :"), " Variable synthétique représentant le cluster"),
                tags$li(strong("Convergence :"), " Itérations jusqu'à stabilité")
              )
            ),

            h4("Métriques disponibles :"),
            tags$ul(
              tags$li(strong("η² (eta carré) :"), " Mesure d'association (0 = aucune, 1 = totale)"),
              tags$li(strong("Inertie par cluster :"), " Somme des η² des variables du cluster"),
              tags$li(strong("Convergence :"), " Nombre d'itérations nécessaires")
            ),

            h4("Visualisations :"),
            tags$ul(
              tags$li("🔥 Heatmap η² : Matrice d'association variable × cluster"),
              tags$li("📊 Inertie : Graphique barres par cluster"),
              tags$li("📈 Tailles : Nombre de variables par cluster"),
              tags$li("🎯 Sélection K : Si n_clusters=NULL, graphique des métriques")
            ),

            hr(),

            # Deep Learning
            h3(icon("brain"), " Deep Learning (Autoencoder)"),

            div(
              class = "info-highlight",
              h4("Principe"),
              tags$ul(
                tags$li(strong("Type de données :"), " Variables numériques (actives)"),
                tags$li(strong("Illustratives :"), " Numériques ou catégorielles (projetées sans influence)"),
                tags$li(strong("Méthode :"), " Autoencoder sur X^T + k-means sur embeddings"),
                tags$li(strong("Architecture :"), " Encodeur → Latent → Décodeur"),
                tags$li(strong("Distance :"), " Euclidienne dans l'espace latent")
              )
            ),

            h4("Métriques disponibles :"),
            tags$ul(
              tags$li(strong("MSE reconstruction :"), " Erreur quadratique moyenne"),
              tags$li(strong("Silhouette :"), " Basé sur distance euclidienne"),
              tags$li(strong("Dunn Index :"), " Ratio distance inter/intra"),
              tags$li(strong("Davies-Bouldin :"), " Plus bas = mieux (< 1.5 bon)")
            ),

            h4("Visualisations :"),
            tags$ul(
              tags$li("🎨 Embeddings 2D/3D : Variables dans espace latent"),
              tags$li("📉 Reconstruction error : Erreur par variable"),
              tags$li("👁️ Variables illustratives : Projection dans l'espace"),
              tags$li("➕ Nouvelles variables : Génération de synthèses")
            ),

            hr(),

            # Comparison
            h3(icon("balance-scale"), " Quelle méthode choisir ?"),

            fluidRow(
              column(
                width = 4,
                div(
                  class = "algo-card kmeans",
                  h4("K-means"),
                  p(strong("✓ Utilisez si :")),
                  tags$ul(
                    tags$li("Variables numériques"),
                    tags$li("Relations linéaires"),
                    tags$li("Besoin d'interprétabilité"),
                    tags$li("Calcul rapide nécessaire")
                  ),
                  p(strong("✗ Évitez si :")),
                  tags$ul(
                    tags$li("Variables catégorielles"),
                    tags$li("Relations non-linéaires complexes")
                  )
                )
              ),
              column(
                width = 4,
                div(
                  class = "algo-card quali",
                  h4("VARCLUS Quali"),
                  p(strong("✓ Utilisez si :")),
                  tags$ul(
                    tags$li("Variables catégorielles uniquement"),
                    tags$li("Questionnaires"),
                    tags$li("Données qualitatives")
                  ),
                  p(strong("✗ Évitez si :")),
                  tags$ul(
                    tags$li("Variables continues"),
                    tags$li("Peu de modalités")
                  )
                )
              ),
              column(
                width = 4,
                div(
                  class = "algo-card deep",
                  h4("Deep Learning"),
                  p(strong("✓ Utilisez si :")),
                  tags$ul(
                    tags$li("Relations non-linéaires"),
                    tags$li("Grand nombre de variables"),
                    tags$li("Patterns complexes"),
                    tags$li("Variables illustratives utiles")
                  ),
                  p(strong("✗ Évitez si :")),
                  tags$ul(
                    tags$li("Peu de données"),
                    tags$li("Besoin interprétabilité max")
                  )
                )
              )
            ),

            hr(),

            h3(icon("lightbulb"), " Conseils pratiques"),

            tags$ul(
              tags$li(strong("Type de données :"), " Respectez les types requis pour chaque algorithme"),
              tags$li(strong("Nombre de clusters :"), " Utilisez les fonctions de sélection K automatique"),
              tags$li(strong("Valeurs manquantes :"), " Imputez ou supprimez avant upload"),
              tags$li(strong("Standardisation :"), " K-means et Deep standardisent automatiquement"),
              tags$li(strong("Interprétation :"), " Explorez toutes les visualisations disponibles"),
              tags$li(strong("Comparaison :"), " Testez plusieurs algorithmes pour comparer les résultats")
            )
          )
        )
      )
    )
  )
)

# =======================================================================
# SERVER LOGIC
# =======================================================================

server <- function(input, output, session) {

  # =====================================================================
  # REACTIVE VALUES
  # =====================================================================

  rv <- reactiveValues(
    # Data
    data = NULL,

    # Variables
    active_vars = NULL,
    illustrative_vars = NULL,
    illu_labels = NULL,

    # Model results
    model = NULL,
    clusters = NULL,
    method = NULL,

    # Quality metrics (adaptative selon algorithme)
    sil_obj = NULL,
    sil_mean = NULL,
    dunn = NULL,
    dbi = NULL,
    quality = NULL,

    # Deep-specific
    illustrative_embeddings = NULL,
    best_params = NULL,
    suggested_k = NULL
  )

  # =====================================================================
  # NAVIGATION
  # =====================================================================

  observeEvent(input$start_btn, {
    updateTabItems(session, "tabs", "data")
  })

  # =====================================================================
  # MODULE 1: DATA UPLOAD
  # =====================================================================

  uploaded_data <- data_upload_server("data_upload")

  observe({
    req(uploaded_data())
    rv$data <- uploaded_data()
  })

  # =====================================================================
  # MODULE 2: VARIABLE SELECTION
  # =====================================================================

  selected_vars <- variable_selection_server(
    "var_select",
    data = reactive(rv$data)
  )

  observe({
    req(selected_vars())
    rv$active_vars <- selected_vars()$active
    rv$illustrative_vars <- selected_vars()$illustrative
    rv$illu_labels <- selected_vars()$illu_labels
  })

  # =====================================================================
  # MODULE 3: ALGORITHM SELECTION & CLUSTERING
  # =====================================================================

  clustering_result <- algorithm_selection_server(
    "algo_select",
    data = reactive(rv$data),
    active_vars = reactive(rv$active_vars)
  )

  observe({
    req(clustering_result())

    # Core results (tous algorithmes)
    rv$model <- clustering_result()$model
    rv$clusters <- clustering_result()$clusters
    rv$method <- clustering_result()$method

    # Quality metrics (peuvent être NULL pour certains algorithmes)
    rv$silhouette <- clustering_result()$silhouette
    rv$sil_mean <- clustering_result()$sil_mean
    rv$dunn <- clustering_result()$dunn
    rv$dbi <- clustering_result()$dbi
    rv$quality <- clustering_result()$quality

    # Optimization results (Deep uniquement)
    rv$best_params <- clustering_result()$best_params
    rv$suggested_k <- clustering_result()$suggested_k

    # Illustrative embeddings (Deep uniquement)
    rv$illustrative_embeddings <- clustering_result()$illustrative_embeddings
  })


  observe({
    cat("\n========== DEBUG APP (CORE STATE) ==========\n")
    cat("rv$method                :", rv$method, "\n")
    cat("rv$active_vars           :", paste(rv$active_vars %||% character(0), collapse = ", "), "\n")
    cat("rv$illustrative_vars     :", paste(rv$illustrative_vars %||% character(0), collapse = ", "), "\n")

    emb_names <- NULL
    if (!is.null(rv$illustrative_embeddings)) {
      emb_names <- names(rv$illustrative_embeddings)
    }
    cat("names(rv$illustrative_embeddings) :", paste(emb_names %||% character(0), collapse = ", "), "\n")
    cat("============================================\n")
  })


  # =====================================================================
  # MODULE 4: UNIFIED VISUALIZATION (Adaptatif)
  # =====================================================================

  unified_vis <- unified_visualization_server(
    "unified_viz",
    model = reactive(rv$model),
    data = reactive(rv$data),
    clusters = reactive(rv$clusters),
    method = reactive(rv$method),
    illustrative_names = reactive(rv$illustrative_vars),
    illustrative_labels = reactive(rv$illu_labels),
    deep_quality = reactive({
      list(
        sil_mean = rv$sil_mean,
        dunn = rv$dunn,
        dbi = rv$dbi,
        quality = rv$quality
      )
    })
  )

  # Synchronisation automatique des variables illustratives projetées
  observe({
    req(rv$method == "deep")
    req(unified_vis$illu_proj)

    rv$illustrative_embeddings <- unified_vis$illu_proj()
  })

  # =====================================================================
  # UI DYNAMIQUE POUR ONGLET NOUVELLES VARIABLES (Deep / K-means)
  # =====================================================================

  # Exposer la méthode actuelle pour conditionalPanel JS (optionnel mais utile)
  output$current_method <- renderText({
    rv$method %||% ""
  })
  outputOptions(output, "current_method", suspendWhenHidden = FALSE)




  # =====================================================================
  # MODULE 5: NEW VARIABLES (Deep only)
  # =====================================================================

  # Deep
  deep_newvariables_server(
    "new_vars_deep",
    model    = reactive(rv$model),
    data     = reactive(rv$data),
    clusters = reactive(rv$clusters),
    method   = reactive(rv$method),
    illu_emb = reactive(rv$illustrative_embeddings),
    illu_vars   = reactive(rv$illustrative_vars),
    illu_labels = reactive(rv$illu_labels)
  )


  # K-means
  kmeans_newvariables_server(
    "new_vars_kmeans",
    model    = reactive(rv$model),
    data     = reactive(rv$data),
    clusters = reactive(rv$clusters),
    method   = reactive(rv$method)
  )

  # VARCLUS Quali
  quali_newvariables_server(
    "new_vars_quali",
    model    = reactive(rv$model),
    data     = reactive(rv$data),
    clusters = reactive(rv$clusters)
  )

  # =====================================================================
  # RESULTS TAB: OUTPUTS
  # =====================================================================

  # --- Model summary ---
  output$results_summary <- renderPrint({
    req(rv$model)
    rv$model$summary()
  })

  # --- Quality metrics display (adaptatif) ---
  output$quality_metrics_display <- renderUI({
    req(rv$model, rv$method)

    if (rv$method == "deep") {
      # Deep: Afficher Silhouette, Dunn, DBI, Quality
      qual_text <- if (is.na(rv$quality)) {
        "Non calculé"
      } else if (rv$quality > 0.7) {
        "Excellente"
      } else if (rv$quality > 0.5) {
        "Bonne"
      } else if (rv$quality > 0.3) {
        "Moyenne"
      } else {
        "Faible"
      }

      qual_class <- if (is.na(rv$quality)) {
        ""
      } else if (rv$quality > 0.7) {
        "quality-excellent"
      } else if (rv$quality > 0.5) {
        "quality-good"
      } else if (rv$quality > 0.3) {
        "quality-average"
      } else {
        "quality-poor"
      }

      tagList(
        div(
          class = "metric-container",

          div(
            class = "metric-item",
            div(class = "metric-label", "Score global"),
            div(class = paste("metric-value", qual_class),
                ifelse(is.na(rv$quality), "N/A", sprintf("%.3f", rv$quality))),
            p(style = "margin-top: 10px; font-weight: bold;", qual_text)
          ),

          div(
            class = "metric-item",
            div(class = "metric-label", "Silhouette"),
            div(class = "metric-value",
                ifelse(is.na(rv$sil_mean), "N/A", sprintf("%.3f", rv$sil_mean)))
          ),

          div(
            class = "metric-item",
            div(class = "metric-label", "Dunn Index"),
            div(class = "metric-value",
                ifelse(is.na(rv$dunn), "N/A", sprintf("%.3f", rv$dunn)))
          ),

          div(
            class = "metric-item",
            div(class = "metric-label", "Davies-Bouldin"),
            div(class = "metric-value",
                ifelse(is.na(rv$dbi), "N/A", sprintf("%.3f", rv$dbi)))
          )
        )
      )

    } else if (rv$method == "kmeans") {
      # K-means: Afficher Q, W, B, Convergence
      tagList(
        p("Les métriques K-means sont affichées dans l'onglet Visualisation."),
        p("Critère Q = B/T visible dans les graphiques PCA et métriques.")
      )

    } else if (rv$method == "quali_varclus") {
      # VARCLUS: Afficher inertie, convergence
      tagList(
        p("Les métriques VARCLUS sont affichées dans l'onglet Visualisation."),
        p("Matrice η² et inertie par cluster disponibles dans les graphiques.")
      )

    } else {
      p("Métriques de qualité non disponibles pour cette méthode.")
    }
  })

  # --- Cluster table (simple) ---
  output$cluster_table <- renderDT({
    req(rv$clusters)

    df <- data.frame(
      Variable = names(rv$clusters),
      Cluster = as.integer(rv$clusters)
    )

    datatable(
      df,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        order = list(list(1, 'asc')),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      filter = "top",
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })

  # --- Statistics table ---
  output$stats_table <- renderDT({
    req(rv$clusters)

    tab <- table(rv$clusters)
    df <- data.frame(
      Cluster = as.integer(names(tab)),
      N_Variables = as.integer(tab),
      Percentage = sprintf("%.1f%%", 100 * as.integer(tab) / sum(tab))
    )

    datatable(
      df,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'Percentage',
        background = styleColorBar(c(0, 100), 'lightblue'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  # --- Detailed table (method-specific) ---
  output$detailed_table <- renderDT({
    req(rv$model)

    df <- rv$model$to_dataframe()

    datatable(
      df,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      filter = "top",
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatRound(columns = which(sapply(df, is.numeric)), digits = 4)
  })

  # =====================================================================
  # DOWNLOADS
  # =====================================================================

  # --- Download: Clusters CSV ---
  output$download_clusters_csv <- downloadHandler(
    filename = function() {
      paste0("clusters_", rv$method, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$clusters)

      df <- data.frame(
        Variable = names(rv$clusters),
        Cluster = as.integer(rv$clusters)
      )

      write.csv(df, file, row.names = FALSE)
    }
  )

  # --- Download: Full table ---
  output$download_full_table <- downloadHandler(
    filename = function() {
      paste0("clusters_detailed_", rv$method, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$model)

      df <- rv$model$to_dataframe()
      write.csv(df, file, row.names = FALSE)
    }
  )

  # --- Download: HTML report ---
  output$download_report_html <- downloadHandler(
    filename = function() {
      paste0("report_", rv$method, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(rv$model, rv$clusters, rv$method)

      # Capture summary
      summ <- paste(capture.output(rv$model$summary()), collapse = "\n")

      # Cluster stats
      tab <- table(rv$clusters)
      cluster_stats <- data.frame(
        Cluster = as.integer(names(tab)),
        N_Variables = as.integer(tab),
        Percentage = sprintf("%.1f%%", 100 * as.integer(tab) / sum(tab))
      )

      # Method name
      method_name <- switch(
        rv$method,
        "deep" = "Deep Variable Clustering",
        "kmeans" = "K-means Réallocatif (Qannari)",
        "quali_varclus" = "VARCLUS Qualitative",
        "Clustering de Variables"
      )

      # Gradient color
      gradient <- switch(
        rv$method,
        "deep" = "linear-gradient(135deg, #667eea 0%, #764ba2 100%)",
        "kmeans" = "linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)",
        "quali_varclus" = "linear-gradient(135deg, #f093fb 0%, #f5576c 100%)",
        "linear-gradient(135deg, #3c8dbc 0%, #2980b9 100%)"
      )

      # Build HTML
      html_content <- paste0(
        "<!DOCTYPE html>",
        "<html lang='fr'>",
        "<head>",
        "<meta charset='UTF-8'>",
        "<title>ClustR Report - ", method_name, " - ", Sys.Date(), "</title>",
        "<style>",
        "body { font-family: 'Segoe UI', Arial, sans-serif; margin: 0; background: #f4f6f9; }",
        ".container { max-width: 1200px; margin: 40px auto; background: white; padding: 40px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }",
        ".header { background: ", gradient, "; color: white; padding: 30px; margin: -40px -40px 30px -40px; border-radius: 8px 8px 0 0; }",
        "h1 { margin: 0; font-size: 32px; }",
        ".header p { margin: 10px 0 0 0; opacity: 0.9; }",
        "h2 { color: #3c8dbc; border-bottom: 2px solid #3c8dbc; padding-bottom: 10px; margin-top: 40px; }",
        "pre { background: #f8f9fa; padding: 20px; border-radius: 5px; border-left: 4px solid #3c8dbc; overflow-x: auto; }",
        "table { border-collapse: collapse; width: 100%; margin: 20px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }",
        "th, td { border: 1px solid #e0e0e0; padding: 12px; text-align: left; }",
        "th { background-color: #3c8dbc; color: white; font-weight: bold; }",
        "tr:nth-child(even) { background-color: #f8f9fa; }",
        "tr:hover { background-color: #e3f2fd; }",
        ".info-box { background: #e3f2fd; border-left: 4px solid #2196f3; padding: 20px; margin: 20px 0; border-radius: 4px; }",
        ".footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #e0e0e0; text-align: center; color: #999; font-size: 12px; }",
        "</style>",
        "</head>",
        "<body>",
        "<div class='container'>",

        # Header
        "<div class='header'>",
        "<h1>", method_name, "</h1>",
        "<p>Rapport généré le ", format(Sys.time(), "%d/%m/%Y à %H:%M"), "</p>",
        "</div>",

        # Overview
        "<div class='info-box'>",
        "<h3 style='margin-top:0;'>📊 Vue d'ensemble</h3>",
        "<p><strong>Algorithme :</strong> ", method_name, "</p>",
        "<p><strong>Nombre de clusters :</strong> ", rv$model$n_clusters, "</p>",
        "<p><strong>Nombre de variables :</strong> ", length(rv$clusters), "</p>",
        "</div>",

        # Model summary
        "<h2>🔍 Résumé du modèle</h2>",
        "<pre>", summ, "</pre>",

        # Cluster statistics
        "<h2>📊 Statistiques par cluster</h2>",
        "<table>",
        "<tr><th>Cluster</th><th>Nombre de variables</th><th>Pourcentage</th></tr>"
      )

      # Add rows
      for (i in 1:nrow(cluster_stats)) {
        html_content <- paste0(
          html_content,
          "<tr>",
          "<td>", cluster_stats$Cluster[i], "</td>",
          "<td>", cluster_stats$N_Variables[i], "</td>",
          "<td>", cluster_stats$Percentage[i], "</td>",
          "</tr>"
        )
      }

      html_content <- paste0(html_content, "</table>")

      # Cluster assignments
      html_content <- paste0(
        html_content,
        "<h2>📋 Assignations des variables</h2>",
        "<table>",
        "<tr><th>Variable</th><th>Cluster</th></tr>"
      )

      for (i in seq_along(rv$clusters)) {
        html_content <- paste0(
          html_content,
          "<tr>",
          "<td>", names(rv$clusters)[i], "</td>",
          "<td>", rv$clusters[i], "</td>",
          "</tr>"
        )
      }

      # Close HTML
      html_content <- paste0(
        html_content,
        "</table>",

        "<div class='footer'>",
        "<p>Généré par ClustR - Variable Clustering Platform</p>",
        "<p>", format(Sys.time(), "%d/%m/%Y à %H:%M:%S"), "</p>",
        "</div>",

        "</div>",
        "</body>",
        "</html>"
      )

      writeLines(html_content, file)
    }
  )

}

# =======================================================================
# RUN APPLICATION
# =======================================================================

shinyApp(ui = ui, server = server)
