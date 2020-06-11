library(shiny)
library(bsem)
library(DT)
library(tidyr)
library(ggplot2)
library(ggridges)
library(dplyr)
library(plotly)
library(visdat)
library(heatmaply)
library(bayesplot)
library(fmsb)
theme_set(theme_bw())

source("biplot.R")

# Define server logic required to draw a histogram
shinyServer(
  function(input, output, session) {
    ## -----
    # Data
    ## -----

    filedata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        return(NULL)
      }
      else {
        read.csv(infile$datapath, row.names = 1)
      }
    })

    varnames <- reactive({
      if (is.null(filedata())) {
        return(NULL)
      } else {
        filedata() %>%
          colnames()
      }
    })

    vartypes <- reactive({
      if (is.null(filedata())) {
        return(NULL)
      } else {
        filedata() %>%
          sapply(class)
      }
    })

    output$selectize1 <- renderUI({
      selectizeInput(
        inputId = "invar_hist",
        "Select Input Variables",
        choices = sprintf("%s (%s)", varnames(), vartypes()),
        multiple = TRUE
      )
    })

    output$selectize2 <- renderUI({
      selectizeInput(
        inputId = "invar_boxp",
        "Select Input Variables",
        choices = sprintf("%s (%s)", varnames(), vartypes()),
        multiple = TRUE
      )
    })

    output$selectize3 <- renderUI({
      selectizeInput(
        inputId = "invar_dens",
        "Select Input Variables",
        choices = sprintf("%s (%s)", varnames(), vartypes()),
        multiple = TRUE
      )
    })

    output$selectize4 <- renderUI({
      selectizeInput(
        inputId = "invar_miss",
        "Select Input Variables",
        choices = sprintf("%s (%s)", varnames(), vartypes()),
        multiple = TRUE
      )
    })

    output$table <- renderDataTable({
      filedata()
    })

    output$hist <- renderPlotly({
      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_hist))) %>%
        gather()

      p <- ggplot(aux, aes(x = value, color = key, fill = key)) +
        geom_histogram(alpha = 0.6, bins = 20)
      # +    geom_histogram(aes(y = ..density..))

      print(ggplotly(p))
    })

    output$boxp <- renderPlotly({
      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_boxp))) %>%
        gather()
      p <- ggplot(aux, aes(x = key, y = value, fill = key)) +
        geom_boxplot(alpha = 0.6)

      print(ggplotly(p))
    })

    output$dens <- renderPlot({
      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_dens))) %>%
        gather()

      ggplot(aux, aes(x = value, y = key, fill = key)) +
        geom_density_ridges(alpha = 0.6)
      # +    geom_histogram(aes(y = ..density..))
    })

    output$miss <- renderPlotly({
      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_miss)))
      p <- vis_miss(aux)
      # +    geom_histogram(aes(y = ..density..))

      print(ggplotly(p))
    })

    ## -----
    # Model
    ## -----

    numericdata <- reactive({
      filedata() %>%
        select_if(is.numeric)
    })

    observeEvent(input$run1, {
      withBusyIndicatorServer("run1", {
        Sys.sleep(1)

        if (blocks() %>% unlist() %>% is.null()) {
          stop("load data and choose desired blocks!")
        }
        else if (exogenous() %>% unlist() %>% is.null() && paths() %>%
          unlist() %>%
          is.null()) {
          fit <- bsem::sem(
            data = numericdata(),
            blocks = blocks(),
          )
          output$summary <- renderPrint({
            summary(fit)
          })
        }
        else if (paths() %>% unlist() %>% is.null()) {
          fit <- bsem::sem(
            data = numericdata(),
            blocks = blocks(),
            exogenous = exogenous
          )
          output$summary <- renderPrint({
            summary(fit)
          })
        }
        else if (exogenous() %>% unlist() %>% is.null()) {
          fit <- bsem::sem(
            data = numericdata(),
            blocks = blocks(),
            paths = paths()
          )
          output$summary <- renderPrint({
            summary(fit)
          })
        }
        else {
          fit <- bsem::sem(
            data = numericdata(),
            blocks = blocks(),
            paths = paths(),
            exogenous = exogenous()
          )
          output$summary <- renderPrint({
            summary(fit)
          })
        }
        output$network <- renderVisNetwork({
          print(plot(fit))
        })
        output$scores <- renderPlotly({
          fit$mean_lambda %>%
            data.frame() %>%
            mutate(fact = rownames(.)) %>%
            gather(key = "key", value = "value", -fact) %>%
            ggplot(aes(key, fact, fill = value)) +
            geom_tile() +
            scale_fill_gradient2(
              limits = c(-max(abs(fit$mean_lambda)), max(abs(fit$mean_lambda))),
              low = "blue", mid = "white", high = "red"
            ) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })

        output$loadings <- renderPlotly({
          fit$mean_alpha %>%
            data.frame() %>%
            mutate(var = rownames(.)) %>%
            gather(key = "fact", value = "value", -var) %>%
            ggplot(aes(fact, var, fill = value)) +
            geom_tile() +
            scale_fill_gradient2(
              limits = c(-max(abs(fit$mean_alpha)), max(abs(fit$mean_alpha))),
              low = "blue", mid = "white", high = "red"
            )
        })

        output$trace_loadings <- renderPlotly({

          p <- mcmc_trace(fit$posterior$alpha, facet_args = list(ncol = ncol(fit$mean_alpha))) +
            theme(axis.text.x = element_blank()) +
            labs(title = "alpha")
          p %>%
            ggplotly(
              height = 200 * nrow(fit$mean_alpha),
              width = 400 * ncol(fit$mean_alpha)
            )
        })

        output$trace_scores <- renderPlotly({
          p <- mcmc_trace(fit$posterior$lambda, facet_args = list(ncol = nrow(fit$mean_lambda))) +
            theme(axis.text.x = element_blank())+
            labs(title = "lambda")

          p %>%
            ggplotly(
              height = 200 * ncol(fit$mean_lambda),
              width = 400 * nrow(fit$mean_lambda)
            )
        })


        output$dens_loadings <- renderPlotly({
          p <- mcmc_dens(fit$posterior$alpha, facet_args = list(nrow = nrow(fit$mean_alpha))) +
            theme(axis.text.x = element_blank()) +
            labs(title = "alpha")

          p %>%
            ggplotly(
              height = 200 * nrow(fit$mean_alpha),
              width = 400 * ncol(fit$mean_alpha)
            )
        })

        output$dens_scores <- renderPlotly({
          p <- mcmc_dens(fit$posterior$lambda, facet_args = list(ncol = nrow(fit$mean_lambda))) +
            theme(axis.text.x = element_blank()) +
            labs(title = "lambda")

          p %>%
            ggplotly(
              height = 200 * ncol(fit$mean_lambda),
              width = 400 * nrow(fit$mean_lambda)
            )
        })

        output$biplot <- renderPlotly({


          p <- ubiplot(scores = t(rbind(X = fit$mean_lambda[input$invar_x,],
                                  Y = fit$mean_lambda[input$invar_y,])
                                  ),
                       loadings = cbind(X = fit$mean_alpha[,input$invar_x],
                                        Y = fit$mean_alpha[,input$invar_y])
          )
          ggplotly(p)
        })

        output$hex <- renderPlotly({
          print(input$invar_h1)
          print(input$invar_h2)
          print(fit$mean_lambda[,input$invar_h1])
          print(fit$mean_lambda[,input$invar_h2])
          h1 <- input$invar_h1
          h2 <- input$invar_h2
          cnames <- fit$mean_lambda %>% colnames()
          print(cnames)
        plot_ly(
          type = 'scatterpolar',
          fill = 'toself'
        )  %>%
            add_trace(
              r = c(fit$mean_lambda[, cnames %in% h1], fit$mean_lambda[1,cnames %in% h1]),
              theta = c(rownames(fit$mean_lambda), rownames(fit$mean_lambda)[1]),
              name = input$invar_h1
            ) %>%
            add_trace(
              r = c(fit$mean_lambda[, cnames %in% h2], fit$mean_lambda[1, cnames %in% h2]),
              theta = c(rownames(fit$mean_lambda), rownames(fit$mean_lambda)[1]),
              name = input$invar_h2
            ) %>%
            layout(
              polar = list(
                radialaxis = list(
                  visible = T,
                  range = c(min(c(fit$mean_lambda[,input$invar_h1],fit$mean_lambda[,input$invar_h2]),
                                max(c(fit$mean_lambda[,input$invar_h1],fit$mean_lambda[,input$invar_h2]))
                  )
                  ),
                  showlegend = T
                )
              )
            )
        })

      })
    })

    observeEvent(input$datafile, {
      updateNumericInput(session, "K", value = 1, min = 1, max = ncol(filedata()))

      output$selectize_blocks <- renderUI({
        lapply(1:input$K, function(i) {
          selectizeInput(
            inputId = paste0("invar_block", i),
            paste("Select manifest variables in block", i),
            choices = colnames(numericdata()),
            selected = colnames(numericdata())[i],
            multiple = TRUE
          )
        })
      })
    })

    observeEvent(input$K, {
      updateNumericInput(session, "J", value = 0, min = 0, max = input$K)

      output$selectize_paths <- renderUI({
        lapply(1:input$J, function(i) {
          selectizeInput(
            inputId = paste0("invar_path", i),
            paste("Reponse score in path", i),
            choices = names(blocks()),
            selected = names(blocks())[i],
            multiple = FALSE
          )
        })
      })

      output$selectize_idlamb <- renderUI({
        lapply(1:input$J, function(i) {
          selectizeInput(
            inputId = paste0("invar_idlamb", i),
            paste("Explanatory scores in path", i),
            choices = names(blocks())[!names(blocks()) %in% names(paths())],
            multiple = FALSE
          )
        })
      })
    })

    observeEvent(input$K, {
      updateNumericInput(session, "L", value = 0, min = 0, max = input$K)

      output$selectize_exogenous <- renderUI({
        lapply(1:input$L, function(i) {
          selectizeInput(
            inputId = paste0("invar_exo", i),
            paste("Exogenous variable", i),
            choices = colnames(numericdata())[!colnames(numericdata()) %in% unlist(blocks())],
            multiple = FALSE
          )
        })
      })

      output$selectize_idex <- renderUI({
        lapply(1:input$L, function(i) {
          selectizeInput(
            inputId = paste0("invar_idex", i),
            paste("Explanatory scores for exogenous variable", i),
            choices = names(blocks()),
            multiple = FALSE
          )
        })
      })
    })

    output$selectize_biplot1 <- renderUI({
        selectizeInput(
          inputId = "invar_y",
          "y-axis",
          choices = names(blocks()),
          selected = names(blocks())[1],
          multiple = FALSE
        )
    })

    output$selectize_biplot2 <- renderUI({
        selectizeInput(
          inputId = "invar_x",
          "x-axis",
          choices = names(blocks()),
          selected = names(blocks())[2],
          multiple = FALSE
        )
    })

    output$selectize_hex1 <- renderUI({
      selectizeInput(
        inputId = "invar_h1",
        "Select the 1st observation",
        choices = rownames(numericdata()),
        selected = rownames(numericdata())[1],
        multiple = FALSE
      )
    })

    output$selectize_hex2 <- renderUI({
      selectizeInput(
        inputId = "invar_h2",
        "Select 2nd observation",
        choices = rownames(numericdata()),
        selected = rownames(numericdata())[2],
        multiple = FALSE
      )
    })



    blocks <- reactive({
      if(input$K == 0){
        NULL
      }
      else{
        B <- lapply(
          1:input$K,
          function(i) {
            if (length(input[[paste0("invar_block", i)]]) > 0) {
              input[[paste0("invar_block", i)]]
            }
          }
        )
        names(B) <- paste0("F", 1:length(B))
        B
      }
    })

# -------------------------------------------------------------------------


    paths <- reactive({
      if(input$J == 0){
          NULL
      }
      else{
        G <- lapply(
          1:input$J,
          function(i) {
            if (length(input[[paste0("invar_idlamb", i)]]) > 0) {
              input[[paste0("invar_idlamb", i)]]
            }
          }
        )
        names(G) <- sapply(1:input$J, function(i) input[[paste0("invar_path", i)]])
        G
      }
    })

    exogenous <- reactive({
      if(input$L == 0){
        NULL
      }
      else{
        Y <- lapply(
          1:input$L,
          function(i) {
            if (length(input[[paste0("invar_idex", i)]]) > 0) {
              input[[paste0("invar_idex", i)]]
            }
          }
        )
        names(Y) <- sapply(1:input$L, function(i) input[[paste0("invar_exo", i)]])
        Y
      }
    })
  }
)
