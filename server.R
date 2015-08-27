
library(shiny)
library(dplyr)
library(ggplot2)
library(GGally)
library(tableplot)
library(DT)
library(plyr)

options(shiny.maxRequestSize = 25*1024^2)
options(shiny.trace = TRUE)

r_data <<- list()
r_data <- reactiveValues()

shinyServer(function(input, output) {

######################################################## LOAD DATA UI
        
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
            radioButtons(inputId = "data_type", label = "Load data",
                         c("rda" = "rda", "csv" = "csv"),
                         selected = "rda", inline = TRUE),
            conditionalPanel(condition = "input.data_type == 'csv'",
              with(tags, table(td(checkboxInput('header', 'Header', TRUE)),
                               td(HTML("&nbsp;&nbsp")),
                               td(checkboxInput('str_as_factor', 'Str. as Factor', TRUE)))),
              radioButtons('sep', 'Separator', c(Comma = ',', Semicolon = ';', Tab = '\t'),
                           ',', inline = FALSE)
            ),
            uiOutput("ui_file_upload")
        )
    })
################### END

######################################################## FUNCTIONS TO LOAD AND MANAGE DATA
    
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
            try(read.table(ufile, header = header, sep = sep, stringsAsFactors = str_as_factor), silent = TRUE)
           else . } %>% as.data.frame
      }
      
      r_data[['datasetlist']] <- c(objname, r_data[['datasetlist']]) %>% unique 
    }
    
    .getdata <- reactive({
        validate(
            need(input$dataset != "", "Please load a tidy data set (.rda or .csv)")
        )
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
################### END
    
######################################################## VARIABLE SELECT
    
    output$varlist_load <- renderUI({
        df <- .getdata()
        items <- names(df)
        names(items) <- items
        checkboxGroupInput(inputId = "varlist",
                           label = "Variables", 
                           choices = items, 
                           selected = items)
    })
    
    output$varlist_select <- renderUI({
        df <- .getdata()
        df <- df[, input$varlist]
        selectInput(inputId = 'var_select',
                    label = 'Variable',
                    choices = names(df),
                    selected = NULL)
    })
    
    output$xvar_select <- renderUI({
        df <- .getdata()
        df <- df[, input$varlist]
        selectInput(inputId = 'xvar',
                    label = 'X Variable',
                    choices = names(df),
                    selected = NULL)
    })
    
    output$yvar_select <- renderUI({
        df <- .getdata()
        df <- df[, input$varlist]
        selectInput(inputId = 'yvar',
                    label = 'Y Variable',
                    choices = names(df),
                    selected = NULL)
    })
################### END
    
######################################################## MANAGE DATA OUTPUT
    
    output$contents <- renderTable({
        df <- .getdata()
        return(df[1:20, input$varlist])
    })
    
    output$dataTable <- renderDataTable({
        df <- .getdata()
        datatable(df[, input$varlist])
    })
    
    output$summary <- renderPrint({
        df <- .getdata()
        summary(df[, input$varlist])
    })
    
    output$str <- renderPrint({
        df <- .getdata()
        str(df[, input$varlist])
    })
    
    output$plot <- renderPlot({
        df <- .getdata()
        df <- df[, input$varlist]
        ggpairs(df[, sapply(df, is.integer) | sapply(df, is.numeric)])    
    })
################### END

######################################################## UNIVARIATE PLOT OUTPUT   
    
    output$histogram <- renderPlot({
        df <- .getdata()
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
        df <- .getdata()
        boxplot(df[, input$var_select])
        if (input$horizontal) {
          boxplot(df[, input$var_select],
                  horizontal = TRUE)
        }
    })
    
    output$barplot <- renderPlot({
        df <- .getdata()
        var_freq <- table(df[, input$var_select])
        barplot(height = var_freq,
                ylab = 'Count')
        if (input$horizontal) {
            barplot(height = var_freq,
                    xlab = 'Count',
                    horiz = TRUE,
                    las = 1)
        }
    })
    
    output$qqplot <- renderPlot({
        df <- .getdata()
        x <- df[, input$var_select]
        qqnorm(x); qqline(x)
    })
################### END
    
######################################################## UNIVARIATE PLOT UI
    
    output$plot_selections <- renderUI({
        df <- .getdata()
        df <- df[, input$varlist]
        if (is.null(input$chart_type)) return()
        
        # Depending on input$chart_type, we'll generate a 
        # different UI component for the plot options 
        # and send to the client
        switch(input$chart_type,
          'Histogram' = {
              
              list(
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
          },
          'Normal Q-Q Plot' = {

              list(

              )
          }
      )
    
    })
################### END

######################################################## MULTIVARIATE PLOT UI
    
    output$plot_selections_multi <- renderUI({
        df <- .getdata()
        df <- df[, input$varlist]
        if (is.null(input$chart_type_multi)) return()
        
        switch(input$chart_type_multi,
               'Scatter Plot' = {
                   list(
                       selectInput(inputId = 'color',
                                   label = 'Color',
                                   choices = c("", names(df)),
                                   selected = NULL),
                       checkboxInput(inputId = 'smooth',
                                     label = 'Smooth',
                                     value = FALSE),
                       checkboxInput(inputId = 'jitter',
                                     label = 'Jitter',
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
    
######################################################## MULTIVARIATE PLOT OUTPUT
    
    output$scatterplot <- renderPlot({
        df <- .getdata()
        validate(
            need(!is.discrete(df[,input$xvar]), "Please select a continuous variable for X"),
            need(!is.discrete(df[,input$yvar]), "Please select a continuous variable for Y")
        )
        p <- ggplot(.getdata(), aes_string(x=input$xvar, y=input$yvar)) + geom_point()
        if (input$color != '') 
            p <- p + aes_string(color=input$color)
        if (input$jitter) 
            p <- p + geom_jitter()
        if (input$smooth) 
            p <- p + geom_smooth()
        print(p)
    })
    
    output$boxplot_multi <- renderPlot({
        df <- .getdata()
        validate(
            need(is.discrete(df[,input$xvar]), "Please select a factor (discrete variable) for X")
        )
        p <- ggplot(df, aes_string(x=input$xvar, y=input$yvar)) + geom_boxplot()
        if (input$horizontal)
            p <- p + coord_flip()
        print(p)
    })
    
    output$barplot_multi <- renderPlot({
        df <- .getdata()
        validate(
            need(is.discrete(df[,input$xvar]), "Please select a factor (discrete variable) for X")
        )
        p <- ggplot(df, aes_string(x=input$xvar, y=input$yvar)) + geom_bar(stat="Identity")
        if (input$horizontal) 
            p <- p + coord_flip()
        print(p)
    })
    
    output$time_series <- renderPlot({
        df <- .getdata()
        p <- ggplot(df, aes_string(x=input$xvar, y=input$yvar)) + geom_line()
        print(p)
    })
})


