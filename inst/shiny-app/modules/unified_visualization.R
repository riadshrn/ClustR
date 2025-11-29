# ===========================================================================
# MODULE : unified_visualization
# - Détecte automatiquement l'algorithme (deep/kmeans/quali_varclus)
# - Affiche les visualisations appropriées
# - Interface unifiée pour tous les algorithmes
# ===========================================================================

unified_visualization_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Header dynamique selon l'algorithme
    fluidRow(
      box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("algorithm_header"))
      )
    ),

    # Contenu dynamique selon l'algorithme
    uiOutput(ns("visualization_content"))
  )
}

unified_visualization_server <- function(id, model, data, clusters, method,
                                         illustrative_names = reactive(NULL),
                                         illustrative_labels = reactive(NULL),
                                         deep_quality = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ======================================================
    # 1) Header dynamique
    # ======================================================
    output$algorithm_header <- renderUI({
      req(method())

      algo_name <- switch(
        method(),
        "deep" = "Deep Learning (Autoencoder)",
        "kmeans" = "K-means Réallocatif (Qannari)",
        "quali_varclus" = "VARCLUS Qualitative",
        "Algorithme inconnu"
      )

      algo_icon <- switch(
        method(),
        "deep" = icon("brain"),
        "kmeans" = icon("project-diagram"),
        "quali_varclus" = icon("list-alt"),
        icon("question")
      )

      tags$div(
        style = paste0("background: linear-gradient(135deg, ",
                       switch(method(),
                              "deep" = "#667eea 0%, #764ba2 100%",
                              "kmeans" = "#4facfe 0%, #00f2fe 100%",
                              "quali_varclus" = "#f093fb 0%, #f5576c 100%",
                              "#ccc 0%, #999 100%"),
                       "); color: white; padding: 20px; border-radius: 10px;"),
        tags$h2(algo_icon, " ", algo_name),
        tags$p(switch(
          method(),
          "deep" = "Clustering basé sur les embeddings d'un autoencodeur",
          "kmeans" = "Clustering par corrélation avec PC1 comme centroïde",
          "quali_varclus" = "Clustering de variables qualitatives par η²",
          ""
        ))
      )
    })

    # ======================================================
    # 2) Contenu selon l'algorithme
    # ======================================================
    output$visualization_content <- renderUI({
      req(method())

      if (method() == "deep") {
        deep_visualization_ui(ns("deep_viz_module"))

      } else if (method() == "kmeans") {
        kmeans_visualization_ui(ns("kmeans_viz_module"))

      } else if (method() == "quali_varclus") {
        quali_visualization_ui(ns("quali_viz_module"))

      } else {
        tags$div(
          class = "alert alert-warning",
          style = "margin: 20px;",
          tags$h4(icon("exclamation-triangle"), " Algorithme non reconnu"),
          tags$p("Méthode détectée: ", method()),
          tags$p("Veuillez ajuster le modèle avec un algorithme supporté (deep, kmeans, quali_varclus).")
        )
      }
    })

    # ======================================================
    # 3) Initialiser les sous-modules (une seule fois)
    #    ⚠ deep_visualization_server RENVOIE illu_proj
    # ======================================================

    deep_viz <- deep_visualization_server(
      "deep_viz_module",
      model = model,
      data = data,
      clusters = clusters,
      method = method,
      illustrative_names = illustrative_names,
      illustrative_labels = illustrative_labels,
      deep_quality = deep_quality
    )

    kmeans_visualization_server(
      "kmeans_viz_module",
      model = model,
      data = data,
      clusters = clusters,
      method = method
    )

    quali_visualization_server(
      "quali_viz_module",
      model = model,
      data = data,
      clusters = clusters
    )

    # ======================================================
    # 4) Ce que le module renvoie à l'extérieur
    # ======================================================
    list(
      illu_proj = deep_viz$illu_proj
    )

  })
}
