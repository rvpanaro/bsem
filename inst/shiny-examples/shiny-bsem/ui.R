library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)
library(visNetwork)
source("helpers.R")

fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(
    "shiny-bsem",
    tabPanel(
      "Data",
      navlistPanel(
        "Setup",
        tabPanel(
          "Data loader",
          fileInput("datafile", "Choose CSV file",
            accept = c("csv", "comma-separated-values", ".csv")
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
          "Boxplot",
          uiOutput("selectize2"),
          plotlyOutput("boxp")
        ),
        tabPanel(
          "Density",
          uiOutput("selectize3"),
          plotOutput("dens")
        ),
        tabPanel(
          "Missing",
          uiOutput("selectize4"),
          plotlyOutput("miss")
        ),
        tabPanel(
          a(img(src = "bsem.png", width = "70px"), href = "https://github.com/rvpanaro/bsem")
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
            "Number of latent variables (factors)",
            value = 1,
            min = 1,
            max = 1,
            width = "auto"
          ),
          uiOutput("selectize_blocks")
        ),
        tabPanel(
          "Paths (paths)",
          numericInput(
            inputId = "J",
            "Number of regressions between scores",
            value = 0,
            min = 0,
            max = 0,
            width = "auto"
          ),
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
            "Number of regressions between exogenous variables and scores",
            value = 0,
            min = 0,
            max = 0,
            width = "auto"
          ),
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
              "Process data",
              class = "btn-primary", icon("play"),
              style = "color: #fff; background-color: #009933;"
            )
          ),
          br(),
          verbatimTextOutput("summary"),
        ),
        "Formulation",
        tabPanel(
          "Full model",
          includeMarkdown("www/model.md")
        ),
        tabPanel(
          a(img(src = "bsem.png", width = "70px"), href = "https://github.com/rvpanaro/bsem")
        )
      )
    ),
    ## nav 3
    tabPanel(
      "Result",
      navlistPanel(
        "Outcome",
        tabPanel(
          "Diagram (mean)",
          visNetworkOutput("network")
        ),
        tabPanel(
          "Posterior mean",
          tabsetPanel(
            type = "tabs",
            tabPanel("Loadings", plotlyOutput("loadings")),
            tabPanel("Scores", plotlyOutput("scores"))
          )
        ),
        tabPanel(
          "Traceplot",
          tabsetPanel(
            type = "tabs",
            tabPanel("Loadings", plotlyOutput("trace_loadings")),
            tabPanel("Scores", plotlyOutput("trace_scores"))
          )
        ),
        tabPanel(
          "Posterior density",
          tabsetPanel(
            type = "tabs",
            tabPanel("Loadings", plotlyOutput("dens_loadings")),
            tabPanel("Scores", plotlyOutput("dens_scores"))
          )
        ),
        tabPanel(
          "Biplot",
          div(
            style = "display:flex; padding: 14 px",
            uiOutput("selectize_biplot1"),
            uiOutput("selectize_biplot2")
          ), plotlyOutput("biplot")
        ),
        tabPanel(
          "Radar plot",
          div(
            style = "display:flex; padding: 14 px",
            uiOutput("selectize_hex1"),
            uiOutput("selectize_hex2")
          ), plotlyOutput("hex")
        ),
        tabPanel(
          a(img(src = "bsem.png", width = "70px"), href = "https://github.com/rvpanaro/bsem")
        )

      )
    )
  )
)
