# Shiny app for itemselectr
# author: ian hussey (ian.hussey@ugent.be)
# license: GPLv3+

# dependencies ----
library(tidyverse)
library(shiny)
library(patchwork)
library(psych)
library(lavaan)

source("https://raw.githubusercontent.com/ianhussey/itemselectr/master/R/loadings_and_errors.r")
source("https://raw.githubusercontent.com/ianhussey/itemselectr/master/R/max_item_alpha.r")
source("https://raw.githubusercontent.com/ianhussey/itemselectr/master/R/r_with_cis.r")

# options ----

#options(shiny.deprecation.messages = FALSE)


# UI ----

ui <- 
  
  navbarPage("itemselectr",
             
             # tab 1 - plots ----
             tabPanel("CFA & attenuation correction",
                      
                      # Sidebar layout with input and output definitions
                      sidebarLayout(
                        
                        # Sidebar panel for inputs
                        sidebarPanel(
                          
                          h3("Upload your item-level data"),
                          
                          "CSV file must contain one column per item, one row per participant. Only include columns for the items of the scale of interest. Plots will appear after a few seconds - Bootstrapping takes a moment to run",
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Select a file ----
                          fileInput("file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          tags$b("CSV file properties"),
                          
                          # Input: Checkbox if file has header ----
                          checkboxInput("header", "Header", TRUE),
                          
                          # Input: Select separator ----
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          
                          tags$a(href="https://github.com/ianhussey/itemselectr", "R package and documentation"), 
                          
                          " and ",
                          
                          tags$a(href="https://github.com/ianhussey/itemselectr-shiny-app", "code for this app")
                          
                        ),
                        
                        # Main panel for displaying outputs
                        mainPanel(
                          
                          # Output: Plot of the requested variable against mpg
                          plotOutput("plot")
                          
                        )
                      )
             )
  )


# Server ----

server <- function(input, output) {
  
  
  output$plot <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    # analyze
    results_cfa         <- loadings_and_errors(df)
    results_attenuation <- max_item_alpha(df)
    
    # plot
    p1 <-
      results_cfa$estimates %>%
      mutate(type = dplyr::recode(type,
                                  `error` = "Errors",
                                  `factor_loading` = "Factor loadings"),
             type = fct_relevel(type, "Factor loadings", "Errors")) %>%
      ggplot(aes(item, estimate)) +
      geom_point() +
      geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
      facet_wrap(~type,
                 scales = "free",
                 nrow = 2,
                 ncol = 1) +
      coord_flip() +
      ylab("Estimate") +
      xlab("Item") +
      ggtitle("(a) Confirmatory Factor Analysis")
    
    p2 <-
      ggplot(results_attenuation$estimates, aes(item, estimate)) +
      geom_point() +
      geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
      coord_flip() +
      ylab(expression(alpha)) +
      xlab("Item") +
      ggtitle("(b) Attentuation correction")
    
    # output combiend plot
    return(p1 + p2 + plot_layout(ncol = 1, nrow = 2, heights = c(2, 1)))
    
    }, height = 750)
  
}


# build app ----

shinyApp(ui, server)


