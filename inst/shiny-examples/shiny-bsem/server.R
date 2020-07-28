library(visNetwork)
library(dplyr)
library(reshape2)
library(ggplot2)
ggplot2::theme_set(ggplot2::theme_bw())

source("biplot.R")

# Define server logic required to draw a histogram
shinyServer(
  function(input, output, session) {
    ## -----
    # Data
    ## -----

    filedata <- reactive({
      infile <- input$datafile

      shiny::validate(
        shiny::need(strsplit(infile$name, ".", fixed = TRUE)[[1]][2] == "csv", "File must be .csv")
      )
        tbl <- read.csv(infile$datapath, row.names = 1)
      return(tbl)
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
        "Select the input variables",
        choices = sprintf("%s (%s)", varnames(), vartypes()),
        multiple = TRUE
      )
    })

    output$selectize2 <- renderUI({
      selectizeInput(
        inputId = "invar_boxp",
        "Select the input variables",
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
        "Select input variables",
        choices = sprintf("%s (%s)", varnames(), vartypes()),
        multiple = TRUE
      )
    })

    output$table <- DT::renderDT({
      shiny::validate(
        shiny::need(input$datafile, "Load your data!
             Be aware that:
                1) Only numeric variables are allowed.
                2) All variables (columns) in the uploaded dataset are used to fit.
                3) Rownames are considered if the first column is of 'character' type.
             ")
      )

      shiny::validate(
        shiny::need(length(filedata())>0, "")
        )

      if (input$std == "0") {
        tbl <- filedata()
      }
      else {
        tbl <- filedata() %>% scale()
      }
      return(tbl)
    })

    output$hist <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(length(filedata())>0, "Please load your data in the 'Data loader'.")
      )
      shiny::validate(
        shiny::need(input$invar_hist, "Please select some numeric variable(s).")
      )

      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_hist))) %>%
        tidyr::gather()

      p <- ggplot(aux, aes(x = value, color = key, fill = key)) +
        geom_histogram(alpha = 0.6, bins = 20)
      # +    geom_histogram(aes(y = ..density..))

      print(plotly::ggplotly(p))
    })

    output$boxp <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data loader'.")
      )

      shiny::validate(
        shiny::need(input$invar_boxp, "Please select some numeric variable(s).")
      )

      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_boxp))) %>%
        tidyr::gather()
      p <- ggplot(aux, aes(x = key, y = value, fill = key)) +
        geom_boxplot(alpha = 0.6)

      print(plotly::ggplotly(p))
    })

    output$dens <- renderPlot({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data loader'.")
      )

      shiny::validate(
        shiny::need(input$invar_dens, "Please select some numeric variable(s).")
      )

      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_dens))) %>%
        tidyr::gather()

      ggplot(aux, aes(x = value, y = key, fill = key)) +
        ggridges::geom_density_ridges(alpha = 0.6)
    })

    output$miss <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data loader'.")
      )

      shiny::validate(
        shiny::need(input$invar_miss, "Please select some variable(s).")
      )

      aux <- filedata() %>%
        select(matches(gsub(" .*$", "", input$invar_miss)))
      p <- visdat::vis_miss(aux)
      # +    geom_histogram(aes(y = ..density..))

      print(plotly::ggplotly(p))
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

    rv <- reactiveValues()

    observeEvent(input$run1, {
      shiny::validate(
        shiny::need(length(numericdata())>0, "Please load your data in the 'Data' > 'Data loader'.")
      )

      if (input$K > 0) {
        for (i in 1:input$K) {
          shiny::validate(
            shiny::need(length(input[[paste0("invar_block", i)]]) > 0, "At least one manifest varible must be selected in 'Factors'.")
          )
        }
      }

      if (input$J > 0) {
        for (i in 1:input$J) {
          shiny::validate(
            shiny::need(length(input[[paste0("invar_idlamb", i)]]) > 0, "At least one common factor should explain the response factor in 'Paths'.")
          )
        }
      }

      if (input$L > 0) {
        for (i in 1:input$L) {
          shiny::validate(
            shiny::need(length(input[[paste0("invar_idex", i)]]) > 0, "At least one common factor should explain each exogenous response in 'Exogenous'.")
          )
        }
      }

      withBusyIndicatorServer("run1", {
        Sys.sleep(1)

        if (blocks() %>% unlist() %>% is.null()) {
          stop("Please choose the desired variables for each block under 'Model' > 'Blocks'!")
        }
        else if (exogenous() %>% unlist() %>% is.null() && paths() %>% unlist() %>% is.null()) {
          print(blocks())
          rv$fit <- bsem::sem(data = numericdata(), blocks = blocks())
        }
        else if (paths() %>% unlist() %>% is.null()) {
          print(blocks())
          print(exogenous())

          rv$fit <- bsem::sem(data = numericdata(), blocks = blocks(), exogenous = exogenous())
        }
        else if (exogenous() %>% unlist() %>% is.null()) {
          print(blocks())
          print(paths())

          rv$fit <- bsem::sem(data = numericdata(), blocks = blocks(), paths = paths())
        }
        else {
          print(blocks())
          print(paths())
          print(exogenous())

          rv$fit <- bsem::sem(data = numericdata(), blocks = blocks(), paths = paths(), exogenous = exogenous())
        }

        if (length(rv$fit) > 0) {
          shinyjs::hide("downloadData")
          shinyjs::hide("downloadFit")
        }
        else {
          shinyjs::show("downloadData")
          shinyjs::show("downloadFit")
        }
      })

        output$selectize_dens_loadings <- renderUI({
          selectizeInput(
            inputId = "invar_dens_loadings",
            "Select some loading",
            choices = sprintf(
              "alpha[%s,%s] (%s, %s)",
              rep(1:nrow(rv$fit$mean_alpha), rep(ncol(rv$fit$mean_alpha), nrow(rv$fit$mean_alpha))),
              rep(1:ncol(rv$fit$mean_alpha), nrow(rv$fit$mean_alpha)),
              rep(rownames(rv$fit$mean_alpha), rep(ncol(rv$fit$mean_alpha), nrow(rv$fit$mean_alpha))),
              rep(colnames(rv$fit$mean_alpha), nrow(rv$fit$mean_alpha))
            ),
            multiple = FALSE
          )
        })

        output$selectize_dens_scores <- renderUI({
          selectizeInput(
            inputId = "invar_dens_scores",
            "Select some score",
            choices = sprintf(
              "lambda[%s,%s] (%s, %s)",
              rep(1:nrow(rv$fit$mean_lambda), rep(ncol(rv$fit$mean_lambda), nrow(rv$fit$mean_lambda))),
              rep(1:ncol(rv$fit$mean_lambda), nrow(rv$fit$mean_lambda)),
              rep(rownames(rv$fit$mean_lambda), rep(ncol(rv$fit$mean_lambda), nrow(rv$fit$mean_lambda))),
              rep(colnames(rv$fit$mean_lambda), nrow(rv$fit$mean_lambda))
            ),
            multiple = FALSE
          )
        })

        output$selectize_trace_loadings <- renderUI({
          selectizeInput(
            inputId = "invar_trace_loadings",
            "Select some loading",
            choices = sprintf(
              "alpha[%s,%s] (%s, %s)",
              rep(1:nrow(rv$fit$mean_alpha), rep(ncol(rv$fit$mean_alpha), nrow(rv$fit$mean_alpha))),
              rep(1:ncol(rv$fit$mean_alpha), nrow(rv$fit$mean_alpha)),
              rep(rownames(rv$fit$mean_alpha), rep(ncol(rv$fit$mean_alpha), nrow(rv$fit$mean_alpha))),
              rep(colnames(rv$fit$mean_alpha), nrow(rv$fit$mean_alpha))
            ),
            multiple = FALSE
          )
        })

        output$selectize_trace_scores <- renderUI({
          selectizeInput(
            inputId = "invar_trace_scores",
            "Select some score",
            choices = sprintf(
              "lambda[%s,%s] (%s, %s)",
              rep(1:nrow(rv$fit$mean_lambda), rep(ncol(rv$fit$mean_lambda), nrow(rv$fit$mean_lambda))),
              rep(1:ncol(rv$fit$mean_lambda), nrow(rv$fit$mean_lambda)),
              rep(rownames(rv$fit$mean_lambda), rep(ncol(rv$fit$mean_lambda), nrow(rv$fit$mean_lambda))),
              rep(colnames(rv$fit$mean_lambda), nrow(rv$fit$mean_lambda))
            ),
            multiple = FALSE
          )
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
    })


    output$network <- renderVisNetwork({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under
             'Model' > 'Factors', also specify (optional) paths and exogenous variables. Then run 'Fit model' under 'Model' > 'Run'!")
      )

      print(plot(rv$fit))
    })

    output$scores <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under 'Model' > 'Factors', also specify (optional) paths and exogenous variables. Then run 'Fit model' under 'Model' > 'Run'!")
      )

      aux <-  rownames(rv$fit$mean_lambda) %>% as.factor()

      rv$fit$mean_lambda %>%
        data.frame() %>%
        mutate(fact = aux) %>%
        tidyr::gather(key = "key", value = "value", -fact) %>%
        ggplot(aes(key, fact, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(
          limits = c(-max(abs(rv$fit$mean_lambda)), max(abs(rv$fit$mean_lambda))),
          low = "blue", mid = "white", high = "red"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ylim(rev(levels(aux)))
    })

    output$loadings <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under 'Model' > 'Factors', also specify (optional) paths and exogenous variables. Then run 'Fit model' under 'Model' > 'Run'!")
      )

      rv$fit$mean_alpha %>%
        data.frame() %>%
        mutate(var = rownames(.)) %>%
        tidyr::gather(key = "fact", value = "value", -var) %>%
        ggplot(aes(fact, var, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(
          limits = c(-max(abs(rv$fit$mean_alpha)), max(abs(rv$fit$mean_alpha))),
          low = "blue", mid = "white", high = "red"
        )
    })

    output$trace_loadings <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under 'Model' > 'Factors', also specify (optional) paths and exogenous variables. Then run 'Fit model' under 'Model' > 'Run'!")
      )

      p <- bayesplot::mcmc_trace(rv$fit$posterior$alpha,
        pars = gsub(" .*$", "", input$invar_trace_loadings)
      ) +
        theme(axis.text.x = element_blank()) +
        labs(title = "alpha")

      p %>%
        plotly::ggplotly(
          height = 300,
          width = 500
        )
    })

    output$trace_scores <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under 'Model' > 'Factors', also specify (optional) paths and exogenous variables. Then run 'Fit model' under 'Model' > 'Run'!")
      )

      p <- bayesplot::mcmc_trace(rv$fit$posterior$lambda, pars = gsub(" .*$", "", input$invar_trace_scores)) +
        theme(axis.text.x = element_blank()) +
        labs(title = "lambda")

      p %>%
        plotly::ggplotly(
          height = 300,
          width = 500
        )
    })

    output$dens_loadings <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under 'Model' > 'Factors', also specify (optional) paths and exogenous variables. Then run 'Fit model' under 'Model' > 'Run'!")
      )

      p <- bayesplot::mcmc_dens(rv$fit$posterior$alpha, pars = gsub(" .*$", "", input$invar_dens_loadings)) +
        theme(axis.text.x = element_blank()) +
        labs(title = "alpha")

      p %>%
        plotly::ggplotly(
          height = 300,
          width = 500
        )
    })

    output$dens_scores <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under 'Model' > 'Factors', also specify (optional) paths and exogenous variables. Then run 'Fit model' under 'Model' > 'Run'!")
      )

      p <- bayesplot::mcmc_dens(rv$fit$posterior$lambda, pars = gsub(" .*$", "", input$invar_dens_scores)) +
        theme(axis.text.x = element_blank()) +
        labs(title = "lambda")

      p %>%
        plotly::ggplotly(
          height = 300,
          width = 500
        )
    })

    output$biplot <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under 'Model' > 'Factors', also specify (optional) paths and exogenous variables. Then run 'Fit model' under 'Model' > 'Run'!")
      )

      p <- ubiplot(
        scores = t(rbind(
          X = rv$fit$mean_lambda[input$invar_x, ],
          Y = rv$fit$mean_lambda[input$invar_y, ]
        )),
        loadings = cbind(
          X = rv$fit$mean_alpha[, input$invar_x],
          Y = rv$fit$mean_alpha[, input$invar_y]
        )
      )
      plotly::ggplotly(p)
    })

    output$hex <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under 'Model' > 'Factors', also specify (optional) paths and exogenous variables. Then run 'Fit model' under 'Model' > 'Run'!")
      )

      h1 <- input$invar_h1
      h2 <- input$invar_h2
      cnames <- rv$fit$mean_lambda %>% colnames()

      plotly::plot_ly(
        type = "scatterpolar",
        fill = "toself"
      ) %>%
        plotly::add_trace(
          r = c(rv$fit$mean_lambda[, cnames %in% h1], rv$fit$mean_lambda[1, cnames %in% h1]),
          theta = c(rownames(rv$fit$mean_lambda), rownames(rv$fit$mean_lambda)[1]),
          name = input$invar_h1
        ) %>%
        plotly::add_trace(
          r = c(rv$fit$mean_lambda[, cnames %in% h2], rv$fit$mean_lambda[1, cnames %in% h2]),
          theta = c(rownames(rv$fit$mean_lambda), rownames(rv$fit$mean_lambda)[1]),
          name = input$invar_h2
        ) %>%
        plotly::layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(min(
                c(rv$fit$mean_lambda[, input$invar_h1], rv$fit$mean_lambda[, input$invar_h2]),
                max(c(rv$fit$mean_lambda[, input$invar_h1], rv$fit$mean_lambda[, input$invar_h2]))
              )),
              showlegend = T
            )
          )
        )
    })

    output$text_blocks <- renderPrint({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      for (i in 1:input$K) {
        shiny::validate(
          shiny::need(length(input[[paste0("invar_block", i)]]) > 0, "At least one manifest varible must be selected in each block.")
        )
      }
    })

    output$text_paths <- renderPrint({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(input$J > 0, "The path modeling is optional, it distinguishes the SEM model from the factor analysis (particular case). The number of paths (if specified) must be lower than the number of blocks in 'Factors', otherwise it must be set to 0.")
      )

      for (i in 1:input$J) {
        shiny::validate(
          shiny::need(length(input[[paste0("invar_idlamb", i)]]) > 0, "At least one common factor should explain the response factor in each path.")
        )
      }
    })

    output$text_exogenous <- renderPrint({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(input$L > 0, "Including exogenous variables is optional, set 0 to do not include any. The exogenous variables are explained by the common factors, they shoud not include any block variables previously defined in 'Factors'.")
      )

      for (i in 1:input$L) {
        shiny::validate(
          shiny::need(length(input[[paste0("invar_idex", i)]]) > 0, "At least one common factor should explain each exogenous response.")
        )
      }
    })

    output$summary <- renderPrint({
      shiny::validate(
        shiny::need(input$datafile, "Please load your data in the 'Data' > 'Data loader'.")
      )

      shiny::validate(
        shiny::need(length(rv$fit) > 0, "Please choose the desired variables for each block under 'Model' > 'Factors', also specify (optional) the paths and the exogenous variables. Then run 'Fit model' above!")
      )
      summary(rv$fit)
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
            paste("Select the manifest variables in block", i),
            choices = colnames(numericdata()),
            multiple = TRUE
          )
        })
      })
    })

    observeEvent(input$K, {
      updateNumericInput(session, "J", value = 0, min = 0, max = input$K - 1)

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

    observeEvent(
      {
        if (input$L > 0) TRUE
      },
      {
        updateNumericInput(session, "L", value = input$L, max = ncol(nonmissnumericdata()))
      }
    )

    observe({
      if (input$L > 0) {
        for (i in 1:input$L) {
          updateSelectizeInput(session,
            inputId = paste0("invar_exo", i),
            choices = c(
              input[[paste0("invar_exo", i)]],
              colnames(nonmissnumericdata())[(!colnames(nonmissnumericdata()) %in% c(unlist(blocks()), names(exogenous())))]
            ), selected = input[[paste0("invar_exo", i)]]
          )
        }
      }
    })

    observeEvent(if (input$K > 0) TRUE, {
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

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-bsem", Sys.Date(), ".csv", sep = "")
      },
      content = function(con) {

        median_lambda <- matrix(rv$fit$stats[startsWith(rownames(rv$fit$stats), "lambda"), "50%"], nrow = input$K) %>% t()
        colnames(median_lambda) <- paste0("F", 1:ncol(median_lambda), "_median")

        if(input$std == "0"){
          aux <- numericdata()
        }
        else{
          aux <- numericdata() %>% scale()
        }

        if(length(rv$fit$mean_Xna)>0){
          for (i in 1:nrow(rv$fit$idna)){
            aux[rv$fit$idna[i,2], rv$fit$idna[i,1]] <- rv$fit$mean_Xna[i]
          }
        }
        write.csv(data.frame(aux, rv$fit$mean_lambda %>% t(), median_lambda), con)
      }
    )

    output$downloadFit <- downloadHandler(
      filename = function() {
        paste("fit-bsem", Sys.Date(), ".rds", sep = "")
      },
      content = function(con) {
        saveRDS(object = rv$fit, file = con)
      }
    )

    observe({
      if (length(rv$fit) > 0) {
        shinyjs::show(id = "downloadData")
        shinyjs::show(id = "downloadFit")
      }
      else {
        shinyjs::hide(id = "downloadData")
        shinyjs::hide(id = "downloadFit")
      }
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
