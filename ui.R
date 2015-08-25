
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyUI(navbarPage("EDA",
                   
    tabPanel("Manage Data",
        fluidRow(
            column(3,
                wellPanel(

                    uiOutput('ui_datasets'),
                    uiOutput('ui_load'),
                    tags$hr(),
                    uiOutput("varlist_load")
                    
                )
            ),

    # Show a plot of the generated distribution
            column(9,
                tabsetPanel(
                    
                    tabPanel('Preview', 
                             br(),
                             tableOutput('contents')
                             ),
                    tabPanel('Explore',
                             br(),
                             dataTableOutput('dataTable')),
                    tabPanel('Summary', 
                             h4("Data Structure"),
                             verbatimTextOutput('str'),
                             h4("Summary"),
                             verbatimTextOutput("summary")
                             ),
                    tabPanel('Correlations', 
                             br(),
                             plotOutput('plot', height = 650)
                             )
                )
            )
        )
    ),
    
    tabPanel('Univariate Analysis',
        fluidRow(
            
            column(3, wellPanel(
              uiOutput('varlist_select'),
              tags$hr(),
              uiOutput('plot_selections')
            )),
        
            
            column(9,
                tabsetPanel(
                    id = 'chart_type',
                    tabPanel('Histogram', 
                             plotOutput('histogram', height = 650)
                             ),
                    tabPanel('Box Plot', 
                             plotOutput('boxplot', height = 650)
                             ),
                    tabPanel('Bar Plot', 
                             plotOutput('barplot', height = 650)
                             ),
                    tabPanel('Q-Q Plot', 
                             plotOutput('qqplot', height = 650)
                             )
                )
            )
        )
    ),
    
    tabPanel("Multivariate Analysis",
        fluidRow(
            
            column(3, wellPanel()),
            
            column(9, 
                   tabsetPanel()
                   )
        )
    )
    
))

