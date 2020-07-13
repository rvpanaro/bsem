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
      validate(
        need(input$datafile, "Load your data!
             Be aware that:
                1) Only numeric variables are allowed.
                2) All variables (columns) in the uploaded dataset are used to fit.
                3) Rownames are considered if the first column is of 'character' type.
             ")
      )

      if(input$std == "0"){
        filedata()
      }
      else{
        filedata() %>% scale
      }
      })

    output$hist <- renderPlotly({
      validate(
        need(input$datafile, "Please load your data in the 'Data loader' tab.")
      )
      validate(
        need(input$invar_hist, "Please select some numeric variable(s).")
      )

      if(input$std == "0"){
        aux2 <- filedata()
      }
      else{
        aux2 <- filedata() %>% scale()
      }

      aux <-  aux2 %>%
        select(matches(gsub(" .*$", "", input$invar_hist))) %>%
        gather()

      p <- ggplot(aux, aes(x = value, color = key, fill = key)) +
        geom_histogram(alpha = 0.6, bins = 20)
      # +    geom_histogram(aes(y = ..density..))

      print(ggplotly(p))
    })

    output$boxp <- renderPlotly({
      validate(
        need(input$datafile, "Please load your data in the 'Data loader' tab.")
      )

      validate(
        need(input$invar_hist, "Please select some numeric variable(s).")
      )

      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_boxp))) %>%
        gather()
      p <- ggplot(aux, aes(x = key, y = value, fill = key)) +
        geom_boxplot(alpha = 0.6)

      print(ggplotly(p))
    })

    output$dens <- renderPlot({
      validate(
        need(input$datafile, "Please load your data in the 'Data loader' tab.")
      )

      validate(
        need(input$invar_hist, "Please select some numeric variable(s).")
      )

      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_dens))) %>%
        gather()

      ggplot(aux, aes(x = value, y = key, fill = key)) +
        geom_density_ridges(alpha = 0.6)
    })

    output$miss <- renderPlotly({
      validate(
        need(input$datafile, "Please load your data in the 'Data loader' tab.")
      )

      validate(
        need(input$invar_hist, "Please select some variable(s).")
      )

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

    nonmissnumericdata <- reactive({
      filedata() %>%
        select_if(is.numeric) %>%
        select_if(Negate(is.null))
    })

    observeEvent(input$run1, {
      withBusyIndicatorServer("run1", {
        validate(
          need(input$datafile, "Please load your data in the 'Data' > 'Data loader' tab.")
        )

        Sys.sleep(1)
        print(blocks())
        print(exogenous())
        print(paths())

        if (blocks() %>% unlist() %>% is.null()) {
          stop("load data and choose desired blocks!")
        }
        else if (exogenous() %>% unlist() %>% is.null() && paths() %>%
          unlist() %>%
          is.null()) {

          fit <- bsem::sem(data = numericdata(), blocks = blocks())

          output$summary <- renderPrint({
            summary(fit)
          })
        }
        else if (paths() %>% unlist() %>% is.null()) {

          fit <- bsem::sem(data = numericdata(), blocks = blocks(), exogenous = exogenous())

          output$summary <- renderPrint({
            summary(fit)
          })
        }
        else if (exogenous() %>% unlist() %>% is.null()) {

          fit <- bsem::sem(data = numericdata(), blocks = blocks(), paths = paths())

          output$summary <- renderPrint({
            summary(fit)
          })
        }
        else {

          fit <- bsem::sem(data = numericdata(), blocks = blocks(), paths = paths(), exogenous = exogenous())
          output$summary <- renderPrint({
            summary(fit)
          })
        }
      })

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

      output$selectize_dens_loadings <- renderUI({
        selectizeInput(
          inputId = "invar_dens_loadings",
          "Select a loading",
          choices = sprintf(
            "alpha[%s,%s] (%s, %s)",
            1:nrow(fit$mean_alpha),
            1:ncol(fit$mean_alpha),
            rownames(fit$mean_alpha),
            colnames(fit$mean_alpha)
          ),
          multiple = FALSE
        )
      })

      output$selectize_dens_scores <- renderUI({
        selectizeInput(
          inputId = "invar_dens_scores",
          "Select a score",
          choices = sprintf(
            "lambda[%s,%s] (%s, %s)",
            1:nrow(fit$mean_lambda),
            1:ncol(fit$mean_lambda),
            rownames(fit$mean_lambda),
            colnames(fit$mean_lambda)
          ),
          multiple = FALSE
        )
      })

      output$selectize_trace_loadings <- renderUI({
        selectizeInput(
          inputId = "invar_trace_loadings",
          "Select a loading",
          choices = sprintf(
            "alpha[%s,%s] (%s, %s)",
            1:nrow(fit$mean_alpha),
            1:ncol(fit$mean_alpha),
            rownames(fit$mean_alpha),
            colnames(fit$mean_alpha)
          ),
          multiple = FALSE
        )
      })

      output$selectize_trace_scores <- renderUI({
        selectizeInput(
          inputId = "invar_trace_scores",
          "Select a score",
          choices = sprintf(
            "lambda[%s,%s] (%s, %s)",
            1:nrow(fit$mean_lambda),
            1:ncol(fit$mean_lambda),
            rownames(fit$mean_lambda),
            colnames(fit$mean_lambda)
          ),
          multiple = FALSE
        )
      })

      output$trace_loadings <- renderPlotly({
        p <- mcmc_trace(fit$posterior$alpha,
                        pars = gsub(" .*$", "", input$invar_trace_loadings)) +
          theme(axis.text.x = element_blank()) +
          labs(title = "alpha")

        p %>%
          ggplotly(
            height = 400,
            width = 600
          )
      })

      output$trace_scores <- renderPlotly({
        p <- mcmc_trace(fit$posterior$lambda, pars = gsub(" .*$", "", input$invar_trace_scores)) +
          theme(axis.text.x = element_blank()) +
          labs(title = "lambda")

        p %>%
          ggplotly(
            height = 400,
            width = 600
          )
      })

      output$dens_loadings <- renderPlotly({
        p <- mcmc_dens(fit$posterior$alpha, pars = gsub(" .*$", "", input$invar_dens_loadings)) +
          theme(axis.text.x = element_blank()) +
          labs(title = "alpha")

        p %>%
          ggplotly(
            height = 400,
            width = 600
          )
      })

      output$dens_scores <- renderPlotly({
        p <- mcmc_dens(fit$posterior$lambda, pars = gsub(" .*$", "", input$invar_dens_scores)) +
          theme(axis.text.x = element_blank()) +
          labs(title = "lambda")

        p %>%
          ggplotly(
            height = 400,
            width = 600
          )
      })

      output$biplot <- renderPlotly({
        p <- ubiplot(
          scores = t(rbind(
            X = fit$mean_lambda[input$invar_x, ],
            Y = fit$mean_lambda[input$invar_y, ]
          )),
          loadings = cbind(
            X = fit$mean_alpha[, input$invar_x],
            Y = fit$mean_alpha[, input$invar_y]
          )
        )
        ggplotly(p)
      })

      output$hex <- renderPlotly({
        h1 <- input$invar_h1
        h2 <- input$invar_h2
        cnames <- fit$mean_lambda %>% colnames()

        plot_ly(
          type = "scatterpolar",
          fill = "toself"
        ) %>%
          add_trace(
            r = c(fit$mean_lambda[, cnames %in% h1], fit$mean_lambda[1, cnames %in% h1]),
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
                range = c(min(
                  c(fit$mean_lambda[, input$invar_h1], fit$mean_lambda[, input$invar_h2]),
                  max(c(fit$mean_lambda[, input$invar_h1], fit$mean_lambda[, input$invar_h2]))
                )),
                showlegend = T
              )
            )
          )
      })
    })


    observeEvent(
      {
        if (any(unlist(lapply(1:input$K, function(i) {
          isTruthy(input[[paste0("invar_block", i)]])
        })))) {
          return(TRUE)
        }
      },
      {
        for (i in 1:input$K) {
          updateSelectizeInput(session,
            inputId = paste0("invar_block", i),
            choices = c(
              input[[paste0("invar_block", i)]],
              colnames(numericdata())[!colnames(numericdata()) %in% unlist(blocks())]
            ),
            selected = input[[paste0("invar_block", i)]]
          )
        }
      }
    )

    observeEvent(input$datafile, {
      updateNumericInput(session, "K", value = 1, min = 1, max = ncol(filedata()))

      output$selectize_blocks <- renderUI({
        lapply(1:input$K, function(i) {
          selectizeInput(
            inputId = paste0("invar_block", i),
            paste("Select manifest variables in block", i),
            choices = colnames(numericdata()),
            multiple = TRUE
          )
        })
      })
    })

    observeEvent(input$K, {
      updateNumericInput(session, "J", value = 0, min = 0, max = input$K-1)

      output$selectize_paths <- renderUI({
        lapply(1:input$J, function(i) {
          selectizeInput(
            inputId = paste0("invar_path", i),
            paste("Response score in path", i),
            choices = names(blocks()),
            selected = names(blocks())[i],
            multiple = FALSE
          )
        })
      })

      observe({
        if (input$J > 0) {
          for (i in 1:input$J) {
            updateSelectizeInput(session,
              inputId = paste0("invar_idlamb", i),
              choices = c(
                input[[paste0("invar_idlamb", i)]],
                names(blocks())[!names(blocks()) %in% names(paths())]
              ),
              selected = input[[paste0("invar_idlamb", i)]]
            )
          }
        }
      })

      output$selectize_idlamb <- renderUI({
        lapply(1:input$J, function(i) {
          selectizeInput(
            inputId = paste0("invar_idlamb", i),
            paste("Explanatory scores in path", i),
            choices = names(blocks()),
            multiple = TRUE
          )
        })
      })
    })

    isolate(
      observeEvent({ if(input$L>0)TRUE},
                   {        updateNumericInput(session, "L", value = input$L, max = ncol(nonmissnumericdata()))})
    )

    isolate(
    observe({
      if (input$L > 0) {

        for (i in 1:input$L) {
          updateSelectizeInput(session,
            inputId = paste0("invar_exo", i),
            choices = c(
              input[[paste0("invar_exo", i)]],
              colnames(nonmissnumericdata())[(!colnames(nonmissnumericdata()) %in% c(unlist(blocks()), names(exogenous())))]
            ),selected = input[[paste0("invar_exo", i)]]
          )
        }
      }
    })
    )

    observeEvent(if(input$K>0)TRUE, {
      updateNumericInput(session, "L", value = 0, min = 0, max = ncol(nonmissnumericdata()))

      output$selectize_exogenous <- renderUI({
        lapply(1:input$L, function(i) {
          selectizeInput(
            inputId = paste0("invar_exo", i),
            paste("Exogenous variable", i),
            choices = colnames(nonmissnumericdata()),
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
            multiple = TRUE
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

    # -------------------------------------------------------------------------

    blocks <- reactive({
      if (input$K == 0) {
        NULL
      }
      else {
        B <- lapply(1:input$K, function(i) {
          if (length(input[[paste0("invar_block", i)]]) > 0) {
            input[[paste0("invar_block", i)]]
          }
          else {
            NULL
          }
        })
        names(B) <- paste0("F", 1:length(B))
        B
      }
    })

    paths <- reactive({
      if (input$J == 0) {
        NULL
      }
      else {
        G <- lapply(1:input$J, function(i) {
          if (length(input[[paste0("invar_idlamb", i)]]) > 0) {
            input[[paste0("invar_idlamb", i)]]
          }
          else {
            NULL
          }
        })
        names(G) <- sapply(1:input$J, function(i) input[[paste0("invar_path", i)]])
        G
      }
    })

    exogenous <- reactive({
      if (input$L == 0) {
        NULL
      }
      else {
        Y <- lapply(1:input$L, function(i) {
          if (length(input[[paste0("invar_idex", i)]]) > 0) {
            input[[paste0("invar_idex", i)]]
          }
          else {
            NULL
          }
        })
        names(Y) <- sapply(1:input$L, function(i) input[[paste0("invar_exo", i)]])
        Y
      }
    })
  }
)
