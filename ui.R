
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(navbarPage("Dora",
                   
    tabPanel("Manage Data",
        fluidRow(
            column(3,
                wellPanel(
                    #fileInput('file1', 'Choose CSV File',
                    #accept = c('text/csv', 
                    #           'text/comma-separated-values,text/plain', 
                    #           '.csv')),
                    #tags$hr(),
                    #checkboxInput('header', 'Header', TRUE),
                    #radioButtons('sep', 'Separator',
                    #            c(Comma = ',',
                    #            Semicolon = ';',
                    #            Tab = '\t'),
                    #            ','),
                    #tags$hr(),
                    uiOutput('ui_datasets'),
                    uiOutput('ui_load'),
                    tags$hr(),
                    uiOutput("varlist_load")
                    
                )
            ),

    # Show a plot of the generated distribution
            column(9,
                tabsetPanel(
                    tabPanel('Load', tableOutput('contents')),
                    tabPanel('View', verbatimTextOutput('summary')),
                    tabPanel('Summary', plotOutput('plot'))
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
                    tabPanel('Histogram', plotOutput('histogram')),
                    tabPanel('Box Plot', plotOutput('boxplot')),
                    tabPanel('Bar Plot', plotOutput('barplot')),
                    tabPanel('Q-Q Plot', plotOutput('qqplot'))
                )
            )
        )
    ),
    
    tabPanel("Multivariate Analysis",
        sidebarLayout(
            sidebarPanel(
                
            ),
            
            mainPanel(
                
            )
        )
    )
    
))

