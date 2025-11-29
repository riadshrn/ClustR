variable_selection_ui <- function(id){
  ns <- NS(id)

  tagList(

    # =========================
    # AUTO SELECTION BOX
    # =========================
    box(width=12, status="success", solidHeader=TRUE,
        title="✨ Sélection automatique recommandée (Deep)",
        div(
          class="recommendation-box",
          uiOutput(ns("auto_reco")),
          sliderInput(ns("corr_threshold"),
                      "Seuil corrélation pour retirer variables redondantes",
                      min=0.7, max=0.99, value=0.90, step=0.01),
          actionButton(ns("apply_auto"), "Appliquer sélection auto",
                       icon=icon("magic"), class="btn-success btn-lg",
                       style="width:100%;")
        )
    ),

    fluidRow(
      # -------------------
      # ACTIVES
      # -------------------
      box(width=6, status="primary", solidHeader=TRUE,
          title="Variables actives",
          uiOutput(ns("active_ui")),
          hr(),
          actionButton(ns("select_all"), "Tout sélectionner"),
          actionButton(ns("deselect_all"), "Tout retirer")
      ),

      # -------------------
      # ILLUSTRATIVES
      # -------------------
      box(width=6, status="info", solidHeader=TRUE,
          title="Variables illustratives",
          uiOutput(ns("illu_ui")),
          uiOutput(ns("illu_label_ui")),
          helpText("Les variables illustratives sont projetées dans l’espace latent mais N'INFLUENCENT PAS le clustering.")
      )
    ),

    # =========================
    # SUMMARY
    # =========================
    box(width=12, status="warning", solidHeader=TRUE,
        title="Résumé sélection",
        verbatimTextOutput(ns("summary_sel"))
    ),

    # =========================
    # CORRELATION MATRIX
    # =========================
    box(width=12, status="info", solidHeader=TRUE,
        collapsible=TRUE,
        collapsed=TRUE,
        title="Matrice de corrélation (variables actives)",
        plotOutput(ns("corr_plot"), height="600px")
    )
  )
}

# ==========================================================
# SERVER
# ==========================================================

variable_selection_server <- function(id, data){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # stockage stable
    sel <- reactiveValues(
      active = NULL,
      illustrative = NULL,
      illu_labels = list()
    )

    # ---- AUTO RECO ----
    auto_reco <- reactive({
      req(data())
      df <- data()

      all <- names(df)
      num <- all[sapply(df, is.numeric)]

      valid <- num[sapply(num, function(v){
        vv <- var(df[[v]], na.rm=TRUE)
        vv > 1e-10 && mean(is.na(df[[v]])) < 0.5
      })]

      if(length(valid) > 1){
        cor_mat <- cor(df[,valid, drop=FALSE], use="pairwise.complete.obs")
        drop <- c()
        for(i in 1:(length(valid)-1)){
          for(j in (i+1):length(valid)){
            if(abs(cor_mat[i,j]) > input$corr_threshold)
              drop <- c(drop, valid[j])
          }
        }
        valid <- setdiff(valid, drop)
      }

      list(
        recommended = valid,
        excluded_non_numeric = setdiff(all, num),
        n = length(valid)
      )
    })

    output$auto_reco <- renderUI({
      r <- auto_reco()
      tagList(
        h4("Deep nécessite variables numériques."),
        p(paste0("Recommandées : ", r$n)),
        if(length(r$excluded_non_numeric)>0)
          p("Exclues : ", paste(r$excluded_non_numeric, collapse=", "))
      )
    })

    observeEvent(input$apply_auto, {
      r <- auto_reco()
      updateCheckboxGroupInput(session, "active_vars", selected = r$recommended)
    })

    # ---- UI ACTIVES ----
    output$active_ui <- renderUI({
      req(data())
      vars <- names(data())

      if(!is.null(input$illu_vars))
        vars <- setdiff(vars, input$illu_vars)

      # défaut si rien encore choisi
      default_sel <- sel$active
      if(is.null(default_sel) || length(default_sel) == 0){
        default_sel <- vars[1:min(10, length(vars))]
      }

      checkboxGroupInput(
        ns("active_vars"),
        "Variables actives :",
        choices = vars,
        selected = default_sel
      )
    })

    # ---- UI ILLUSTRATIVES ----
    output$illu_ui <- renderUI({
      req(data(), input$active_vars)
      vars <- setdiff(names(data()), input$active_vars)
      checkboxGroupInput(
        ns("illu_vars"),
        "Illustratives :",
        choices = vars,
        selected = sel$illustrative
      )
    })

    # ---- UI LABELS only if factor ----
    output$illu_label_ui <- renderUI({
      req(data(), input$illu_vars)

      df <- data()

      facs <- input$illu_vars[sapply(input$illu_vars, function(v){
        is.factor(df[[v]]) || (is.numeric(df[[v]]) && length(unique(df[[v]])) <= 10)
      })]

      if(length(facs)==0) return(NULL)

      vn <- facs[1]
      vals <- df[[vn]]
      vals <- factor(vals)
      levs <- levels(vals)

      # initialise si pas encore fait
      if(is.null(sel$illu_labels[[vn]])){
        sel$illu_labels[[vn]] <- levs
        names(sel$illu_labels[[vn]]) <- levs
      }

      tagList(
        hr(),
        h4(paste("Labels pour", vn)),
        lapply(seq_along(levs), function(i){
          textInput(
            ns(paste0("lvl_",vn,"_",i)),
            label = paste("Niveau", levs[i]),
            value = sel$illu_labels[[vn]][i]
          )
        })
      )
    })



    # ---- SELECT / DESELECT ALL ----
    observeEvent(input$select_all, {
      req(data())
      vars <- names(data())

      # on évite de sélectionner les illustratives
      if(!is.null(input$illu_vars))
        vars <- setdiff(vars, input$illu_vars)

      updateCheckboxGroupInput(session, "active_vars", selected = vars)
    })

    observeEvent(input$deselect_all, {
      updateCheckboxGroupInput(session, "active_vars", selected = character(0))
    })



    # ---- UPDATE sel$illu_labels WITHOUT resetting everything ----
    observeEvent(input$illu_vars, {
      sel$illustrative <- input$illu_vars
    })

    observeEvent(input$active_vars, {
      sel$active <- input$active_vars
    })


    # --- AUTO-LABEL WINE UCI ---
    observeEvent(input$illu_vars, {
      req(data())
      df <- data()

      for(vn in input$illu_vars){

        v <- df[[vn]]
        v_fac <- factor(v)
        levs <- levels(v_fac)

        # UCI Wine dataset detection
        if (vn == "Class" && identical(levs, c("1","2","3"))) {

          # Only auto-fill if user did not change anything
          if (is.null(sel$illu_labels[[vn]]) ||
              identical(unname(sel$illu_labels[[vn]]), levs)) {

            message("[AUTO] Wine UCI détecté → Auto-labels Class appliqués.")

            # Set automatic labels
            new_labels <- c(
              "1"="Barolo",
              "2"="Grignolino",
              "3"="Barbera"
            )
            sel$illu_labels[[vn]] <- new_labels

            # mettre à jour les textInput dans l’UI !!!
            for(i in seq_along(levs)) {
              updateTextInput(session, paste0("lvl_", vn, "_", i),
                              value = unname(new_labels[i]))
            }
          }
        }
      }
    })


    # mise à jour des labels
    observe({
      req(data(), input$illu_vars)
      df <- data()

      for(vn in input$illu_vars){
        v <- df[[vn]]
        if(is.factor(v) || (is.numeric(v) && length(unique(v)) <= 10)){
          v_fac <- factor(v)
          levs <- levels(v_fac)

          labs <- sapply(seq_along(levs), function(i){
            input[[paste0("lvl_",vn,"_",i)]] %||% levs[i]
          })

          names(labs) <- levs
          sel$illu_labels[[vn]] <- labs
        }
      }
    })

    # ================================
    # CORRELATION PLOT
    # ================================
    output$corr_plot <- renderPlot({
      req(data(), sel$active)

      df <- data()[, sel$active, drop=FALSE]

      num <- sapply(df, is.numeric)
      if(sum(num) < 2){
        plot.new()
        text(0.5,0.5,"Au moins 2 variables numériques sont nécessaires")
        return()
      }

      df_num <- df[,num,drop=FALSE]

      cor_mat <- cor(df_num, use="pairwise.complete.obs")

      if(requireNamespace("corrplot", quietly=TRUE)){
        corrplot::corrplot(
          cor_mat, method="color", addCoef.col="black",
          tl.col="black", tl.srt=45
        )
      } else {
        heatmap(cor_mat)
      }
    })

    # ---- RETURN ----
    reactive(list(
      active = sel$active,
      illustrative = sel$illustrative,
      illu_labels = sel$illu_labels
    ))
  })
}

