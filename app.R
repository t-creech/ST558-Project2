
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
        options = list(placeholder = "Filter Numerical 1")
      ),
      uiOutput("num_range1"),
      # Numerical 2
      selectizeInput(
        "num2", "Filter Numerical 2",
        choices = num_vars,
        selected = num_vars[2],
        options = list(placeholder = "Filter Numerical 2")
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
    
    # Drop unused factors
    data_table <- data_table |> mutate(across(where(is.factor), droplevels))
    
    # Assign to filtered$data
    filtered$data <- data_table
    
  })
  
  ### About Tab Functionality
  output$about <- renderUI({
    tagList(
      br(),
      tags$img(
        src = "bank_image.jpg",
        style = "max-width:100%; height:auto"
      ),
      h3("Purpose of this app"),
      p("This app was created to allow users to explore the Bank Marketing dataset. The data is sourced from direct marketing campaigns of a Portuguese
        banking institution. The app allows for users to subset data using two categorical and two numerical filters. Once data is subsetted, the data can be downloaded as a CSV
        or explored in the app."),
      hr(),
      h3("How to use it"),
      h5("Side Panel"),
      tags$ol(
        tags$li(tags$b("Pick Filters:"), "Choose two categorical variables and two numerical variables you would like to subset the data with from the right side panel"),
        tags$li(tags$b("Set Levels and Ranges:"), "Use the dropdowns and sliders to select the specific levels and ranges that you are interested in"),
        tags$li(tags$b("Click Apply:"), "Once you are happy with your selection, click apply button to subset the data"),
      ),
      h5("Main Tabs"),
      tags$ul(
        tags$li(tags$b("Download:"), "Go to the Download tab if you would like to see the tabular data and/or download the subsetted data as a CSV"),
        tags$li(tags$b("Explore:"), "Go to the Explore tab to access tables and plots that are useful in exploring the subsetted data")
      ),
      hr(),
      h3("The Data"),
      p("Source: UCI/Kaggle Banking Dataset - Marketing Targets"),
      tags$ul(
        tags$li(tags$b("Age:"), "Age of campaign target"),
        tags$li(tags$b("Job:"), "Job type of the campaign target"),
        tags$li(tags$b("Marital:"), "Marital status of the campaign target"),
        tags$li(tags$b("Education:"), "Education level of the campaign target"),
        tags$li(tags$b("Default:"), "Whether or not the campaign target has credit in default"),
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
    content = function(file) {write.csv(filtered$data, file, row.names = FALSE)}
  )
  
  ### Explore Tab
  output$explore <- renderUI({
    tagList(
      h3("Explore your subset"),
      radioButtons(
        "exp_mode", "Show:",
        c("Categorical Summaries" = "cat",
          "Numeric Summaries" = "num",
          "Plots" = "plots"),
        inline = TRUE
      ),
      
      # Categorical Summaries
      conditionalPanel("input.exp_mode == 'cat'",
        fluidRow(
          column(6, selectizeInput("oneway", "One-way variable", choices = cat_vars, selected = cat_vars[1])),
          column(6, tagList(
            selectizeInput("twoway_a", "Two-Way First Level", choices = cat_vars, selected = cat_vars[1]),
            selectizeInput("twoway_b", "Two-Way Second Level", choices = cat_vars, selected = cat_vars[2])
          ))
        ),
        hr(),
        h4("One-way Contingency Table"),
        tableOutput("oneway_table"),
        hr(),
        h4("Two-way Contingency Table"),
        tableOutput("twoway_table")
      ),
      
      # Numeric Summaries
      conditionalPanel("input.exp_mode == 'num'",
        fluidRow(
          column(6, selectizeInput("num_summary_var", "Numeric Variable", choices = num_vars, selected = num_vars[1])),
          column(6, selectizeInput("cat_by", "Categorical Variable to Summarize By", choices = c("None", cat_vars), selected = cat_vars[1]))
        ),
        br(),
        tableOutput("num_table")
      ),
      
      # Plots
      conditionalPanel("input.exp_mode == 'plots'",
        selectizeInput(
          "plot_type", "Select Plot Type",
          choices = c(
            "Density" = "density",
            "Scatter Plot" = "scatter",
            "Bar Graph" = "bar",
            "Faceted Histogram" = "hist",
            "Categorical Heatmap" = "cat_heat",
            "Boxplot" = "box"
          ),
          selected = "density"
        ),
        conditionalPanel("input.plot_type == 'density'",
          selectizeInput("dens_num_var", "Numeric Variable for Density", choices = num_vars, selected = num_vars[[1]])                 
        ),
        conditionalPanel("input.plot_type == 'scatter'",
          fluidRow(
            column(6, selectizeInput("scatter_num_var_x", "Numeric Variable for X Axis", choices = num_vars, selected = num_vars[[1]])),
            column(6, selectizeInput("scatter_num_var_y", "Numeric Variable for Y Axis", choices = num_vars, selected = num_vars[[2]]))
          ),
            selectizeInput("scatter_cat_var", "Color By (Optional)", choices = c("None", cat_vars), selected = "None")
        ),
        conditionalPanel("input.plot_type == 'bar'",
          fluidRow(
           column(6, selectizeInput("bar_cat_var_x", "Categorical Variable for X Axis", choices = cat_vars, selected = cat_vars[[1]])),
           column(6, selectizeInput("bar_fill_by", "Fill By (Optional)", choices = c("None", cat_vars), selected = "None"))
          )                 
        ),
        conditionalPanel("input.plot_type == 'hist'",
         fluidRow(
           column(6, selectizeInput("hist_num_var_x", "Numeric Variable for X Axis", choices = num_vars, selected = num_vars[[1]])),
           column(6, selectizeInput("facet_by", "Facet By (Optional)", choices = c("None", cat_vars), selected = "None"))
         )                 
        ),
        conditionalPanel("input.plot_type == 'cat_heat'",
         fluidRow(
           column(6, selectizeInput("heat_cat_var_x", "Categorical Variable for X Axis", choices = cat_vars, selected = cat_vars[[1]])),
           column(6, selectizeInput("heat_cat_var_y", "Categorical Variable for Y Axis", choices = cat_vars, selected = cat_vars[[2]]))
         )                 
        ),
        conditionalPanel("input.plot_type == 'box'",
         fluidRow(
           column(6, selectizeInput("box_cat_var_x", "Categorical Variable for X Axis", choices = cat_vars, selected = cat_vars[[1]])),
           column(6, selectizeInput("box_num_var_y", "Numeric Variable for Y Axis", choices = num_vars, selected = num_vars[[1]]))
         )                 
        ),
        # Print Plot
        br(),
        plotOutput("plot_out")
      )
    )
  })
  
  # Contingency Table Creation
  output$oneway_table <- renderTable({
    req(input$oneway)
    d <- filtered$data
    var <- input$oneway
    tab <- table(d[[var]]) |> as.data.frame()
    colnames(tab) <- c(var, "Frequency")
    tab
  }, rownames = FALSE)
  
  output$twoway_table <- renderTable({
    req(input$twoway_a)
    req(input$twoway_b)
    d <- filtered$data
    var_a <- input$twoway_a
    var_b <- input$twoway_b
    tab <- table(d[[var_a]], d[[var_b]]) |> as.data.frame()
    colnames(tab) <- c(var_a, var_b, "Frequency")
    tab
  }, rownames = FALSE)
  
  #Numeric Summary Creation
  output$num_table <- renderTable({
    req(input$num_summary_var)
    req(input$cat_by)
    d <- filtered$data
    num <- input$num_summary_var
    cat <- input$cat_by
    if (cat == "None") {
      int_by_cat <- d |>
        summarise(
          n = n(),
          mean = mean(.data[[num]]),
          median = median(.data[[num]]),
          sd = sd(.data[[num]])
        )
    } else {
      int_by_cat <- d |>
        # first we group by the factor
        group_by(.data[[cat]]) |>
        # then we summarize the current int_var for that factor
        summarise(
          n = n(),
          mean = mean(.data[[num]]),
          median = median(.data[[num]]),
          sd = sd(.data[[num]])
        )
    }
  })
  
  # Plot Creation
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
