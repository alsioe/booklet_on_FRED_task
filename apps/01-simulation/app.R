# Extracted from the original FRED teaching document.
# Requires the supporting scripts listed in README.md.
library(shiny)
library(tidyverse)
library(cowplot)
source("r_scripts/simulate_FRED_ab.R")
source("r_scripts/plot_FRED.R")

ui <- fluidPage(
# Settings
fluidRow(
  column(4,
    sliderInput("n_trials", "Number of trials:",
                min = 20, max = 200, value = 40, step = 10)
  ),
  column(4,
    sliderInput("alpha", "alpha (reward bias):",
                min = 0, max = 30, value = 10, step = 1)
  ),
  column(4,
    sliderInput("beta", "beta (effort sensitivity):",
                min = 0, max = 3, value = 0.5, step = 0.1)
  )
),

# Run simulation
actionButton(
  inputId = "run_sim",
  label   = "Run simulation",
  icon    = icon("play")
),

plotOutput("plot_example_mouse")
)

server <- function(input, output, session) {
# Simulate data when button is clicked
sim_data <- eventReactive(input$run_sim, {
    simulate_FRED_ab(alpha = input$alpha,
                     beta = input$beta,
                     n_trials = input$n_trials)
})


# Wait for the button press, then render
output$plot_example_mouse <- renderPlot({
    req(sim_data())
    plot_FRED(sim_data())
})
}
shinyApp(ui, server)
