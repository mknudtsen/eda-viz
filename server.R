
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(ggplot2)
library(GGally)
library(tableplot)

options(shiny.maxRequestSize = 25*1024^2)
options(shiny.trace = TRUE)

r_data <<- list()
r_data <- reactiveValues()

shinyServer(function(input, output) {

    #filedata <- reactive({
    #    infile <- input$uploadfile
    #    if (is.null(infile)) return()
    #    read.csv(infile$datapath, header = input$header, sep = input$sep)
    #})
    
    

############### NEW -- Adapted from Radiant -- #####################
        
    output$ui_file_upload <- renderUI({
        
        if (is.null(input$data_type)) return()
        if (input$data_type == "csv") {
            fileInput('uploadfile', '', multiple = FALSE,
                      accept =c('text/csv', 'text/comma-separated-values',
                                'text/tab-separated-values', 
                                'text/plain', '.csv', '.tsv'))
        } else if (input$data_type == "rda") {
            fileInput('uploadfile', '', multiple = FALSE,
                      accept = c('.rda', '.rds', '.rdata'))
        }
    }) 
    
    output$ui_load <- renderUI({
        list(
          #wellPanel(
            
            radioButtons(inputId = "data_type", label = "Load data",
                         c("rda" = "rda", "csv" = "csv"),
                         selected = "rda", inline = TRUE),
            conditionalPanel(condition = "input.data_type == 'csv'",
              with(tags, table(td(checkboxInput('header', 'Header', TRUE)),
                               td(HTML("&nbsp;&nbsp")),
                               td(checkboxInput('str_as_factor', 'Factorize', TRUE)))),
              radioButtons('sep', 'Separator', c(Comma = ',', Semicolon = ';', Tab = '\t'),
                           ',', inline = FALSE)
            ),
            uiOutput("ui_file_upload")
          #)
        )
    })
    
    observe({
        in_file <- input$uploadfile
        if (!is.null(in_file)) {
          isolate({
            loadData(in_file$name, in_file$datapath,
                     input$data_type, 
                     header = input$header,
                     str_as_factor = input$str_as_factor,
                     sep = input$sep)
          })
        }
    })
    
    loadData <- function(fname, ufile, ext,
                         header = TRUE, 
                         str_as_factor = TRUE,
                         sep = ",") {
    
      filename <- basename(fname)
      objname <- sub(paste0(".", ext, "$"), "", filename)
      
      if (ext == 'rda') {
        robjname <- try(load(ufile), silent = FALSE)
        r_data[[objname]] <- as.data.frame(get(robjname))
      } 
      
      if (ext == 'csv') {
          r_data[[objname]] <- try(read_delim(ufile, sep, col_names = header), silent = TRUE) %>%
          {if (is(., 'try-error'))
            try(read.table(ufile, header = header, sep = sep, stringsAsFactors = FALSE), silent = TRUE)
           else . } %>% as.data.frame
      }
      
      r_data[['datasetlist']] <- c(objname, r_data[['datasetlist']]) %>% unique 
    }
    
    
    
    .getdata <- reactive({
        if (is.null(input$dataset)) return()
        r_data[[input$dataset]]
    })
    
    .getclass <- reactive({
        head(r_data[[input$dataset]]) %>% getclass
    })
    
    varnames <- reactive({
        .getclass() %>%
          set_names(., paste0(., " {", .getclass(), "}"))
    })
    
    getclass <- function(dat) {
        sapply(dat, function(x) class(x)[1]) %>%
          sub("ordered", "factor", .) %>%
          sub("POSIXct", "date", .) %>%
          sub("POSIXlt", "date", .) %>%
          sub("Date", "date", .) 
    }
    
    output$ui_datasets <- renderUI({
        # Drop-down selection of data set
        tagList(
            selectInput(inputId = "dataset", label = "Datasets",
                        choices = r_data$datasetlist,
                        selected = "", multiple = FALSE)
        )
    })
    
############### NEW -- Adapted from Radiant -- #####################
    
    output$varlist_load <- renderUI({
        #df <- filedata()
        #if (is.null(df)) return(NULL)
        #items <- names(df)
        #names(items) <- items
        df <- .getdata()
        items <- names(df)
        names(items) <- items
        checkboxGroupInput(inputId = "varlist",
                           label = "Variables", 
                           choices = items, 
                           selected = items)
    })
    
    output$varlist_select <- renderUI({
        df <- filedata()
        if (is.null(df)) return()
        # Limit df to 'varlist' from load
        df <- df[, input$varlist]
        # Retrieve variable options from column names
        items <- names(df)
        names(items) <- items
        # UI select dropdown for variable selection
        selectInput(inputId = 'var_select',
                    label = 'Variable',
                    choices = items,
                    selected = NULL)
    })
    
    output$contents <- renderTable({
        df <- .getdata()
        if (is.null(df)) return(NULL)
        return(df[, input$varlist])
    })
    
    output$summary <- renderPrint({
        df <- filedata()
        if (is.null(df)) return(NULL)
        summary(df[, input$varlist])
    })
    
    output$plot <- renderPlot({
        df <- filedata()
        if (is.null(df)) return(NULL)
        df <- df[, input$varlist]
        ggpairs(df[, sapply(df, is.integer) | sapply(df, is.numeric)])    
    })
    
    output$histogram <- renderPlot({
        df <- filedata()
        if (is.null(df)) return(NULL)
        hist(df[, input$var_select], 
             probability = TRUE, 
             breaks = as.numeric(input$n_breaks),
             xlab = input$var_select,
             main = NULL)
        if (input$individual_obs) {
            rug(df[, input$var_select])
        }
        if (input$density) {
            dens <- density(df[, input$var_select])
            lines(dens, col = 'blue')
        }
        if (input$median) {
            med <- median(df[, input$var_select])
            abline(v = med, col = 'orange', lwd = 2)
        }
    })
    
    output$boxplot <- renderPlot({
        df <- filedata()
        if (is.null(df)) return(NULL)
        boxplot(df[, input$var_select])
        if (input$horizontal) {
          boxplot(df[, input$var_select],
                  horizontal = TRUE)
        }
    })
    
    output$barplot <- renderPlot({
        df <- filedata()
        if (is.null(df)) return()
        var_freq <- table(df[, input$var_select])
        barplot(height = var_freq,
                ylab = 'Count')
    })
    
    output$plot_selections <- renderUI({
        df <- filedata()
        if (is.null(df)) return()
        if (is.null(input$chart_type)) return()
        
        # Depending on input$chart_type, we'll generate a 
        # different UI component for the plot options 
        # and send to the client
        switch(input$chart_type,
          'Histogram' = {
              list(
              # UI select dropdown for number of bins
                selectInput(inputId = 'n_breaks',
                            label = 'Bins',
                            choices = c(5, 10, 20, 35, 50),
                            selected = 20),
                checkboxInput(inputId = 'median',
                              label = 'Show median',
                              value = FALSE),
                checkboxInput(inputId = 'individual_obs',
                              label = 'Show observations',
                              value = FALSE),
                checkboxInput(inputId = 'density',
                              label = 'Show density',
                              value = FALSE)
              )
          },
          'Box Plot' = {
              list(
                  checkboxInput(inputId = 'horizontal',
                                label = 'Horizontal',
                                value = FALSE)
              )
          },
          'Bar Plot' = {
              list(
                  checkboxInput(inputId = 'horizontal',
                                label = 'Horizontal',
                                value = FALSE)
              )
          }
      )
    })

})


