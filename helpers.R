### Author: Thad Creech
### Date: 11/5/2025
### Purpose: Functions to help support shiny app

# Data loader function to load the data in from and rds file
load_bank_data <- function() {
  readRDS("data/full.rds")
}

# Funciton supporting dynamic sliders, allows for slider to be made for a given var by finding range
make_slider <- function(data, var, input_id) {
  vals <- data[[var]]
  rng <- range(vals)
  sliderInput(
    inputId = input_id,
    label = paste("Range of", var),
    min = floor(rng[1]),
    max = ceiling(rng[2]),
    value = rng,
    ticks = FALSE
  )
}




