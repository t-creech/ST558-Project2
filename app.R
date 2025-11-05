
library(shiny)
library(shinyWidgets)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# Load data and obtain variable lists

all_data <- load_bank_data()
cat_vars <- names(select(all_data, where(is.factor)))
num_vars <- names(select(all_data, where(is.numeric)))

# Define UI for application that draws a histogram
ui <- fluidPage(
  h2("Bank Marketing Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      h2("Choose filter on panel below then click Apply."),
      # Categorical 1
      selectizeInput(
        "cat1", "Filter Category 1",
        choices = cat_vars,
        selected = cat_vars[1],
        options = list(placeholder = "Filter Category 1")
      ),
      uiOutput("cat_levels1"),
      # Categorical 2
      selectizeInput(
        "cat2", "Filter Category 2",
        choices = cat_vars,
        selected = cat_vars[2],
        options = list(placeholder = "Filter Category 2")
      ),
      uiOutput("cat_levels2"),
      # Numerical 1
      selectizeInput(
        "num1", "Filter Numerical 1",
        choices = num_vars,
        selected = num_vars[1],
        options = list(placeholder = "Filter Numerical 1"),
      ),
      uiOutput("num_range1"),
      # Numerical 2
      selectizeInput(
        "num2", "Filter Numerical 2",
        choices = num_vars,
        selected = num_vars[2],
        options = list(placeholder = "Filter Numerical 2"),
      ), 
      uiOutput("num_range2"),
      actionButton("apply","Apply")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About", uiOutput("about")),
        tabPanel("Data Download", uiOutput("download")),
        tabPanel("Data Exploration", uiOutput("explore"))
      )
    )
  )
)


server <- function(input, output, session) {
  ### Code for dropdowns and sliders
  output$cat_levels1 <- renderUI({
    req(input$cat1)
    lvls <- sort(unique(all_data[[input$cat1]]))
    pickerInput(
      inputId = "cat_vals1",
      choices = lvls,
      selected = lvls,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$cat_levels2 <- renderUI({
    req(input$cat2)
    lvls <- sort(unique(all_data[[input$cat2]]))
    pickerInput(
      inputId = "cat_vals2",
      choices = lvls,
      selected = lvls,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$num_range1 <- renderUI({req(input$num1); make_slider(all_data, input$num1, "num_rng1")})
  output$num_range2 <- renderUI({req(input$num2); make_slider(all_data, input$num2, "num_rng2")})
  
  ### Code for Apply Button
  
  filtered <- reactiveValues(data = all_data)
  
  observeEvent(input$apply, {
    data_table <- all_data
    
    # Filter Categories
    data_table <- data_table |> filter(.data[[input$cat1]] %in% input$cat_vals1)
    data_table <- data_table |> filter(.data[[input$cat2]] %in% input$cat_vals2)
    
    # Filter Numeric
    rng1 <- input$num_rng1
    rng2 <- input$num_rng2
    data_table <- data_table |> filter(.data[[input$num1]] >= rng1[1] & .data[[input$num1]] <= rng1[2])
    data_table <- data_table |> filter(.data[[input$num2]] >= rng2[1] & .data[[input$num2]] <= rng2[2])
    
    filtered$data <- data_table
    
  })
  
  ### Data Download Functionality
  output$download <- renderUI({
    tagList(
      br(),
      downloadButton("download_csv", "Download subset as CSV"),
      br(), br(),
      DT::dataTableOutput("data_table")
    )
  })
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      filtered$data,
      options = list(pageLength = 20, scrollX = TRUE),
      filter = "top"
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {paste('bank_data-', Sys.Date(), '.csv', sep='')},
    content = function(file) {write.csv(filtered$data, file)}
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
