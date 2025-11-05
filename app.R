
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
  
  ### About Tab Functionality
  output$about <- renderUI({
    tagList(
      br(),
      tags$img(
        src = "bank_image.jpg",
        style = "max-width:100%; height:auto",
      ),
      h3("Purpose of this app"),
      p("This app was created to allow users to explore the Bank Marketing dataset. The data is sourced from direct marketing campaigns of a Portuguese
        banking institution. The app allows for users to subset data using two categorical and two numerical filters. Once data is subseted, the data can be downloaded as a CSV
        or explored in the app."),
      hr(),
      h3("How to use it"),
      h5("Side Panel"),
      tags$ol(
        tags$li(tags$b("Pick Filters:"), "Choose two categorical varialbes and two numerical variables you would like to subset the data with from the right side panel"),
        tags$li(tags$b("Set Levels and Ranges:"), "Use the dropdowns and sliders to select the specific levels and ranges that you are interested in"),
        tags$li(tags$b("Click Apply:"), "Once you are happy with your selection, click apply button to subset the data"),
      ),
      h5("Main Tabs"),
      tags$ul(
        tags$li(tags$b("Download:"), "Go to the Download tab if you would like to see the tabular data and/or download the subsetted data as a CSV"),
        tags$li(tags$b("Explore:"), "Go to the Explore tab to access tables and graphs that are useful in exploring the subsetted date")
      ),
      hr(),
      h3("The Data"),
      p("Source: UCI/Kaggle Banking Dataset - Marketing Targets"),
      tags$ul(
        tags$li(tags$b("Age:"), "Age of campaign target"),
        tags$li(tags$b("Job:"), "Job type of the campaign target"),
        tags$li(tags$b("Marital:"), "Marital status of the campaign target"),
        tags$li(tags$b("Education:"), "Education level of the campaign target"),
        tags$li(tags$b("Default:"), "Whether or not the ccampaign target has credit in default"),
        tags$li(tags$b("Balance:"), "Average yearly balance of campaign target, in euros"),
        tags$li(tags$b("Housing:"), "Whether or not the campaign target has a housing loan"),
        tags$li(tags$b("Loan:"), "Whether or not the campaign target has a personal loan"),
        tags$li(tags$b("Contact:"), "Communication type of last contact of the current marketing campaign"),
        tags$li(tags$b("Day:"), "Day of the month of the last contact of the current marketing campaign"),
        tags$li(tags$b("Month:"), "Month of the last contact of the current marketing campaign"),
        tags$li(tags$b("Duration:"), "Duration (in seconds) of the last contact of the current marketing campaign"),
        tags$li(tags$b("Campaign:"), "Number of contacts performed during this marketing campaign and for this target"),
        tags$li(tags$b("PDays:"), "Number of days that passed by after the target was last contacted from a previous marketing campaign (-1 means target was not contacted previously)"),
        tags$li(tags$b("Previous:"), "Number of contacts performed before this marketing campaign and for this target"),
        tags$li(tags$b("POutcome:"), "Outcome of the previous marketing campaign before this campaign for this target"),
        tags$li(tags$b("Success:"), "Outcome of this marketing campaign for a term deposit")
      ),
      p(
        "More Information:",
        a(
          href = "https://www.kaggle.com/datasets/prakharrathi25/banking-dataset-marketing-targets",
          "Banking Dataset - Marketing Targets"
        )
      ),
    )
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
