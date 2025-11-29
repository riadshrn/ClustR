#' Data Upload Module - UI
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI
#' @export
data_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        title = "📁 Charger les données",

        h4("Option 1: Dataset intégré"),
        pickerInput(
          ns("example_dataset"),
          "Choisir:",
          choices = c(
            "mtcars (32 obs, 11 vars)" = "mtcars",
            "iris (150 obs, 5 vars)" = "iris",
            "Wine UCI (178 obs, 14 vars)" = "wine_uci",
            "Communities & Crime (1994 obs, 128 vars)" = "crime",
            "HAR - Human Activity Recognition (7352 obs, 561 vars)" = "har"
          ),
          options = list(title = "Sélectionner...", size = 6)
        ),
        actionButton(
          ns("load_example"),
          "Charger",
          icon = icon("database"),
          class = "btn-success",
          style = "width:100%;"
        ),

        hr(),

        h4("Option 2: Upload fichier"),
        fileInput(
          ns("file"),
          "CSV / TSV / XLSX",
          accept = c(".csv", ".tsv", ".txt", ".xlsx")
        ),
        radioButtons(
          ns("separator"),
          "Séparateur",
          inline = TRUE,
          choices = c("Virgule" = ",", "Point-virgule" = ";", "TAB" = "\t"),
          selected = ","
        ),
        checkboxInput(ns("header"), "Header ?", TRUE),
        actionButton(
          ns("load_file"),
          "Uploader",
          icon = icon("upload"),
          class = "btn-primary",
          style = "width:100%;"
        )
      ),

      box(
        width = 6,
        status = "info",
        solidHeader = TRUE,
        title = "📊 Aperçu",
        uiOutput(ns("data_info")),
        DTOutput(ns("preview_table"))
      )
    )
  )
}


#' Data Upload Module - Server
#'
#' @param id Module namespace ID
#'
#' @return Reactive value containing the loaded dataset
#' @export
data_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    data_r <- reactiveVal(NULL)

    # ========================================================================
    # HELPER: Charger un dataset depuis system.file() avec fallback internet
    # ========================================================================
    load_dataset_safe <- function(filename, url = NULL, read_func = read.csv, ...) {
      # 1. Essayer de charger depuis inst/extdata/
      local_path <- system.file("extdata", filename, package = "ClustR")

      if (file.exists(local_path) && nchar(local_path) > 0) {
        tryCatch({
          df <- read_func(local_path, ...)
          showNotification(
            paste("✓ Dataset chargé depuis le package local"),
            type = "message",
            duration = 3
          )
          return(df)
        }, error = function(e) {
          showNotification(
            paste("⚠ Erreur lecture locale:", e$message),
            type = "warning",
            duration = 5
          )
        })
      }

      # 2. Fallback: télécharger depuis internet si URL fournie
      if (!is.null(url)) {
        showNotification(
          "⏳ Fichier local introuvable, téléchargement depuis internet...",
          type = "message",
          duration = 5
        )

        temp_file <- tempfile(fileext = tools::file_ext(filename))

        tryCatch({
          # Essayer avec download.file
          download.file(
            url,
            destfile = temp_file,
            method = "auto",
            quiet = TRUE,
            mode = "wb"
          )

          df <- read_func(temp_file, ...)
          unlink(temp_file)

          showNotification(
            "✓ Dataset téléchargé avec succès",
            type = "message",
            duration = 3
          )
          return(df)

        }, error = function(e) {
          # Essayer avec curl si disponible
          if (requireNamespace("curl", quietly = TRUE)) {
            tryCatch({
              curl::curl_download(url, temp_file, quiet = TRUE)
              df <- read_func(temp_file, ...)
              unlink(temp_file)

              showNotification(
                "✓ Dataset téléchargé avec succès (via curl)",
                type = "message",
                duration = 3
              )
              return(df)
            }, error = function(e2) {
              unlink(temp_file)
              stop(paste("Impossible de télécharger:", e2$message))
            })
          } else {
            unlink(temp_file)
            stop(paste("Téléchargement échoué:", e$message))
          }
        })
      }

      # 3. Échec total
      stop(paste(
        "Dataset introuvable:",
        filename,
        "\nVeuillez placer le fichier dans inst/extdata/ ou activer internet."
      ))
    }

    # ========================================================================
    # HELPER: Nettoyer et préparer le dataframe
    # ========================================================================
    prepare_dataframe <- function(df, remove_na = TRUE) {
      # Convertir rownames non-triviaux en colonne
      if (!is.null(rownames(df)) &&
          !identical(rownames(df), as.character(seq_len(nrow(df))))) {
        df$RowName <- rownames(df)
        rownames(df) <- NULL
      }

      # Convertir caractères en facteurs
      char_cols <- sapply(df, is.character)
      if (any(char_cols)) {
        df[char_cols] <- lapply(df[char_cols], as.factor)
      }

      # SUPPRIMER LES LIGNES AVEC NA (pour éviter erreurs de clustering)
      if (remove_na && any(is.na(df))) {
        initial_rows <- nrow(df)
        df <- na.omit(df)
        removed_rows <- initial_rows - nrow(df)

        if (removed_rows > 0) {
          showNotification(
            paste0("⚠ ", removed_rows, " ligne(s) avec NA supprimée(s). ",
                   "Dataset final: ", nrow(df), " observations."),
            type = "warning",
            duration = 5
          )
        }
      }

      return(df)
    }

    # ========================================================================
    # OBSERVER: Charger dataset intégré
    # ========================================================================
    observeEvent(input$load_example, {
      req(input$example_dataset)

      df <- tryCatch({
        switch(
          input$example_dataset,

          # ----------------------------------------------------------------
          # MTCARS (dataset R natif)
          # ----------------------------------------------------------------
          "mtcars" = {
            df <- mtcars
            df$Car_Name <- rownames(df)
            rownames(df) <- NULL
            df
          },

          # ----------------------------------------------------------------
          # IRIS (dataset R natif)
          # ----------------------------------------------------------------
          "iris" = {
            iris
          },

          # ----------------------------------------------------------------
          # WINE UCI
          # ----------------------------------------------------------------
          "wine_uci" = {
            wine <- load_dataset_safe(
              filename = "wine_uci.csv",
              url = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
              read_func = read.csv,
              header = FALSE
            )

            # Nommer les colonnes
            colnames(wine) <- c(
              "Class", "Alcohol", "Malic_Acid", "Ash", "Alcalinity",
              "Magnesium", "Phenols", "Flavanoids", "Nonflavanoid",
              "Proanthocyanins", "Color_Intensity", "Hue", "OD280", "Proline"
            )

            # Convertir Class en facteur
            wine$Class <- factor(wine$Class)
            wine
          },

          # ----------------------------------------------------------------
          # COMMUNITIES & CRIME
          # ----------------------------------------------------------------
          "crime" = {
            crime <- load_dataset_safe(
              filename = "communities_crime.csv",
              url = "https://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data",
              read_func = read.csv,
              header = FALSE,
              na.strings = "?"
            )

            # Nommer les colonnes (128 variables) - depuis communities.names
            colnames(crime) <- c(
              "state", "county", "community", "communityname", "fold",
              "population", "householdsize", "racepctblack", "racePctWhite",
              "racePctAsian", "racePctHisp", "agePct12t21", "agePct12t29",
              "agePct16t24", "agePct65up", "numbUrban", "pctUrban", "medIncome",
              "pctWWage", "pctWFarmSelf", "pctWInvInc", "pctWSocSec",
              "pctWPubAsst", "pctWRetire", "medFamInc", "perCapInc",
              "whitePerCap", "blackPerCap", "indianPerCap", "AsianPerCap",
              "OtherPerCap", "HispPerCap", "NumUnderPov", "PctPopUnderPov",
              "PctLess9thGrade", "PctNotHSGrad", "PctBSorMore", "PctUnemployed",
              "PctEmploy", "PctEmplManu", "PctEmplProfServ", "PctOccupManu",
              "PctOccupMgmtProf", "MalePctDivorce", "MalePctNevMarr",
              "FemalePctDiv", "TotalPctDiv", "PersPerFam", "PctFam2Par",
              "PctKids2Par", "PctYoungKids2Par", "PctTeen2Par",
              "PctWorkMomYoungKids", "PctWorkMom", "NumIlleg", "PctIlleg",
              "NumImmig", "PctImmigRecent", "PctImmigRec5", "PctImmigRec8",
              "PctImmigRec10", "PctRecentImmig", "PctRecImmig5", "PctRecImmig8",
              "PctRecImmig10", "PctSpeakEnglOnly", "PctNotSpeakEnglWell",
              "PctLargHouseFam", "PctLargHouseOccup", "PersPerOccupHous",
              "PersPerOwnOccHous", "PersPerRentOccHous", "PctPersOwnOccup",
              "PctPersDenseHous", "PctHousLess3BR", "MedNumBR", "HousVacant",
              "PctHousOccup", "PctHousOwnOcc", "PctVacantBoarded",
              "PctVacMore6Mos", "MedYrHousBuilt", "PctHousNoPhone",
              "PctWOFullPlumb", "OwnOccLowQuart", "OwnOccMedVal",
              "OwnOccHiQuart", "RentLowQ", "RentMedian", "RentHighQ",
              "MedRent", "MedRentPctHousInc", "MedOwnCostPctInc",
              "MedOwnCostPctIncNoMtg", "NumInShelters", "NumStreet",
              "PctForeignBorn", "PctBornSameState", "PctSameHouse85",
              "PctSameCity85", "PctSameState85", "LemasSwornFT",
              "LemasSwFTPerPop", "LemasSwFTFieldOps", "LemasSwFTFieldPerPop",
              "LemasTotalReq", "LemasTotReqPerPop", "PolicReqPerOffic",
              "PolicPerPop", "RacialMatchCommPol", "PctPolicWhite",
              "PctPolicBlack", "PctPolicHisp", "PctPolicAsian", "PctPolicMinor",
              "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", "PolicAveOTWorked",
              "LandArea", "PopDens", "PctUsePubTrans", "PolicCars",
              "PolicOperBudg", "LemasPctPolicOnPatr", "LemasGangUnitDeploy",
              "LemasPctOfficDrugUn", "PolicBudgPerPop", "ViolentCrimesPerPop"
            )

            # IMPORTANT: Ne supprimer QUE les codes techniques inutiles
            # GARDER communityname comme variable illustrative (col 4)
            # Supprimer: state (1), county (2), community (3), fold (5)
            crime <- crime[, -c(1, 2, 3, 5)]

            # Résultat: communityname + 123 variables numériques
            # L'utilisateur pourra choisir communityname comme variable illustrative
            crime
          },

          # ----------------------------------------------------------------
          # HAR (Human Activity Recognition)
          # ----------------------------------------------------------------
          "har" = {
            har <- load_dataset_safe(
              filename = "Human Activity Recognition.csv",
              url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip",
              read_func = read.table,
              header = FALSE
            )

            # Nommer les colonnes (561 features)
            colnames(har) <- paste0("Feature_", 1:ncol(har))
            har
          },

          # ----------------------------------------------------------------
          # DEFAULT
          # ----------------------------------------------------------------
          stop("Dataset non reconnu")
        )
      }, error = function(e) {
        showNotification(
          paste("❌ Erreur chargement dataset:", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      })

      # Préparer le dataframe
      if (!is.null(df)) {
        df <- prepare_dataframe(df)
        data_r(df)
        showNotification(
          paste("✔ Dataset", input$example_dataset, "chargé avec succès!"),
          type = "message",
          duration = 3
        )
      }
    })

    # ========================================================================
    # OBSERVER: Charger fichier utilisateur
    # ========================================================================
    observeEvent(input$load_file, {
      req(input$file)

      df <- tryCatch({
        ext <- tools::file_ext(input$file$name)

        if (ext %in% c("csv", "tsv", "txt")) {
          # Fichiers texte délimités
          df <- read.table(
            input$file$datapath,
            header = input$header,
            sep = input$separator,
            stringsAsFactors = FALSE,
            na.strings = c("", "NA", "N/A", "null"),
            comment.char = "",
            quote = "\""
          )
        } else if (ext == "xlsx") {
          # Fichiers Excel
          if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("Le package 'readxl' est requis pour lire les fichiers XLSX.\nInstallez-le avec: install.packages('readxl')")
          }
          df <- as.data.frame(readxl::read_excel(input$file$datapath))
        } else {
          stop(paste("Format de fichier non supporté:", ext, "\nUtilisez CSV, TSV, TXT ou XLSX"))
        }

        df
      }, error = function(e) {
        showNotification(
          paste("❌ Erreur lecture fichier:", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      })

      # Préparer le dataframe
      if (!is.null(df)) {
        df <- prepare_dataframe(df)
        data_r(df)
        showNotification(
          paste("✔ Fichier", input$file$name, "chargé avec succès!"),
          type = "message",
          duration = 3
        )
      }
    })

    # ========================================================================
    # OUTPUT: Informations dataset
    # ========================================================================
    output$data_info <- renderUI({
      req(data_r())
      df <- data_r()

      tagList(
        div(
          class = "success-message",
          style = "padding: 10px; background-color: #d4edda; border-radius: 5px;",
          tags$ul(
            style = "margin: 0; padding-left: 20px;",
            tags$li(tags$b("Observations:"), nrow(df)),
            tags$li(tags$b("Variables:"), ncol(df)),
            tags$li(tags$b("Numériques:"), sum(sapply(df, is.numeric))),
            tags$li(tags$b("Facteurs:"), sum(sapply(df, is.factor))),
            tags$li(tags$b("Valeurs manquantes:"), sum(is.na(df)),
                    sprintf("(%.1f%%)", 100 * sum(is.na(df)) / (nrow(df) * ncol(df))))
          )
        )
      )
    })

    # ========================================================================
    # OUTPUT: Aperçu tableau
    # ========================================================================
    output$preview_table <- renderDT({
      req(data_r())

      datatable(
        head(data_r(), 20),
        options = list(
          scrollX = TRUE,
          dom = "t",
          pageLength = 20,
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "display compact"
      )
    })

    # ========================================================================
    # RETURN
    # ========================================================================
    return(data_r)
  })
}
