library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(visNetwork)
library(shinycssloaders)
source("helpers.R")

fluidPage(
  theme = shinytheme("cosmo"),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(15%);
             left: calc(85%);
             }
             ")
    )
  ),
  navbarPage(
    "shiny-bsem",
    id = "main",
    tabPanel(
      "Data",
      navlistPanel(
        "Setup",
        tabPanel(
          "Data loader",
          fileInput("datafile", "Choose CSV file",
            accept = c("csv", "comma-separated-values", ".csv")
          ),
          radioButtons("std",
            label = "Standardize?",
            choices = list("No" = 0, "Yes" = 1),
            selected = 0
          ),
          dataTableOutput("table")
        ),
        "Descriptives",
        tabPanel(
          "Histogram",
          uiOutput("selectize1"),
          plotlyOutput("hist")
        ),
        tabPanel(
          "Density",
          uiOutput("selectize3"),
          plotOutput("dens")
        ),
        tabPanel(
          "Boxplot",
          uiOutput("selectize2"),
          plotlyOutput("boxp")
        ),
        tabPanel(
          "Missing",
          uiOutput("selectize4"),
          plotlyOutput("miss")
        ),
        tabPanel(
          a(img(src = "bsem.png", width = "70px"), href = "https://cran.r-project.org/web/packages/bsem/index.html")
        )
      )
    ),
    tabPanel(
      "Model",
      navlistPanel(
        "Setup model",
        tabPanel(
          "Factors (blocks)",
          numericInput(
            inputId = "K",
            "Choose the number of latent variables (factors)",
            value = 0,
            min = 0,
            max = 0,
            width = "70%"
          ),
          br(),
          verbatimTextOutput("text_blocks"),
          uiOutput("selectize_blocks")
        ),
        tabPanel(
          "Paths (paths)",
          numericInput(
            inputId = "J",
            "Choose the number of regressions between scores",
            value = 0,
            min = 0,
            max = 0,
            width = "70%"
          ),
          br(),
          verbatimTextOutput("text_paths"),
          conditionalPanel(
            "input.J > 0",
            div(
              style = "display:flex; padding: 14 px",
              uiOutput("selectize_paths"),
              uiOutput("selectize_idlamb")
            )
          )
        ),
        tabPanel(
          "Exogenous variables (exogenous)",
          numericInput(
            inputId = "L",
            "Choose the number of regressions between exogenous variables and scores",
            value = 0,
            min = 0,
            max = 0,
            width = "70%"
          ),
          br(),
          verbatimTextOutput("text_exogenous"),
          conditionalPanel(
            "input.L > 0",
            div(
              style = "display:flex; padding: 14 px",
              uiOutput("selectize_exogenous"),
              uiOutput("selectize_idex")
            )
          )
        ),
        tabPanel(
          "Run",
          shinyjs::useShinyjs(),
          withBusyIndicatorUI(
            actionButton(
              "run1",
              "Fit model",
              class = "btn-primary", icon("play"),
              style = "color: #fff; background-color: #009933;"
            )
          ),
          br(),
          div(
            downloadButton("downloadData", "Download result in .csv"),
            downloadButton("downloadFit", "Download fit in .rda")
          ),
          br(),
          br(),
          verbatimTextOutput("summary"),
        ),
        "Formulation",
        tabPanel(
          "Full model",
          includeMarkdown("www/model.md")
        ),
        tabPanel(
          a(img(src = "bsem.png", width = "70px"), href = "https://cran.r-project.org/web/packages/bsem/index.html")
        )
      )
    ),
    ## nav 3
    tabPanel(
      "Result",
      navlistPanel(
        "Outcome",
        tabPanel(
          "Posterior mean diagram",
          conditionalPanel(
            condition = "input.run1",
            visNetworkOutput("network") %>% withSpinner(color = "#009933")
          )
        ),
        tabPanel(
          "Posterior mean heatmap",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Loadings",
              plotlyOutput("loadings") %>% withSpinner(color = "#009933")
            ),
            tabPanel(
              "Scores",
              plotlyOutput("scores") %>% withSpinner(color = "#009933")
            )
          )
        ),
        tabPanel(
          "Traceplot",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Loadings",
              uiOutput("selectize_trace_loadings"),
              plotlyOutput("trace_loadings") %>% withSpinner(color = "#009933")
            ),
            tabPanel(
              "Scores",
              uiOutput("selectize_trace_scores"),
              plotlyOutput("trace_scores") %>% withSpinner(color = "#009933")
            )
          )
        ),
        tabPanel(
          "Posterior density",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Loadings",
              uiOutput("selectize_dens_loadings"),
              plotlyOutput("dens_loadings") %>% withSpinner(color = "#009933")
            ),
            tabPanel(
              "Scores",
              uiOutput("selectize_dens_scores"),
              plotlyOutput("dens_scores") %>% withSpinner(color = "#009933")
            )
          )
        ),
        tabPanel(
          "Biplot",
          div(
            style = "display:flex; padding: 14 px",
            uiOutput("selectize_biplot1"),
            uiOutput("selectize_biplot2")
          ), plotlyOutput("biplot") %>% withSpinner(color = "#009933")
        ),
        tabPanel(
          "Radar plot",
          div(
            style = "display:flex; padding: 14 px",
            uiOutput("selectize_hex1"),
            uiOutput("selectize_hex2")
          ), plotlyOutput("hex") %>% withSpinner(color = "#009933")
        ),
        tabPanel(
          a(img(src = "bsem.png", width = "70px"), href = "https://cran.r-project.org/web/packages/bsem/index.html")
        )
      )
    )
  )
)
