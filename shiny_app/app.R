# Shiny app for itemselectr
# author: ian hussey (ian.hussey@ugent.be)
# license: GPLv3+

# dependencies ----
library(tidyverse)
library(shiny)
library(patchwork)
library(psych)
library(lavaan)

# use CFA to find factor loadings and errors
loadings_and_errors <- function(data) {
  
  require(tidyverse)
  require(lavaan)
  require(semPlot)
  
  # model specification
  model <- paste("L =~", paste(colnames(data), collapse = ' + '))
  
  # fit model
  fit <- cfa(model, 
             data = data) 
  
  # extract estimates
  estimates <- as.data.frame(fit@ParTable) %>%
    select(lhs, op, rhs, est, se) %>%
    filter(!(op == "~~" & lhs == "L" & rhs == "L")) %>%
    rename(item = rhs,
           estimate = est) %>%
    mutate(ci_lower = estimate - se*1.96,
           ci_upper = estimate + se*1.96,
           type = ifelse(op == "=~", "factor_loading", 
                         ifelse(op == "~~", "error", NA))) %>%
    select(item, type, estimate, ci_lower, ci_upper) 
  
  plot <- 
    estimates %>%
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
    xlab("Item")
  
  return(results = list(estimates = estimates,
                        model_fit = fit, 
                        plot = plot))
}

# pearson's r with 95% CIs
r_with_cis <- function(data){
  
  require(tidyverse)
  require(psych)
  
  temp <- data %>%
    select(sum_score, score) %>%
    cor.ci(x = ,
           use = "pairwise.complete.obs", 
           plot = FALSE, 
           n.iter = 5000)
  
  results <- temp$ci %>%
    select(low.e, up.e) %>%
    mutate(r = temp$means)
  
  return(results)
  
}

# attentution correction for each item with 95% CIs
max_item_alpha <- function(data) {
  
  require(psych)
  require(tidyverse)
  
  ic_alpha <- psych::alpha(data,
                           n.iter = 5000)
  
  estimates <- data %>%
    mutate(sum_score = rowSums(.)) %>%
    gather(item, score, c(-sum_score)) %>%
    group_by(item) %>%
    do(r_with_cis(.)) %>%
    ungroup() %>%
    mutate(estimate = r^2 / ic_alpha$boot.ci[2],
           ci_lower = low.e^2 / ic_alpha$boot.ci[1],
           ci_upper = up.e^2 / ic_alpha$boot.ci[3]) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    select(-low.e, -up.e, -r)
  
  plot <- 
    ggplot(estimates, aes(item, estimate)) +
    geom_point() +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
    coord_flip() +
    ylab(expression(alpha)) +
    xlab("Item") 
  
  return(results = list(estimates = estimates,
                        plot = plot))
  
}


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
                          
                          tags$b("CSV files properties"),
                          
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


