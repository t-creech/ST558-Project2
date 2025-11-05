#Functions and information to help in the app functionality


# Data loader
load_bank_data <- function() {
  readRDS("data/full.rds")
}

# Funciton supporting dynamic sliders
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




