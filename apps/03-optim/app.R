# Extracted from the original FRED teaching document.
# Requires the supporting scripts listed in README.md.
library(shiny)
library(tidyverse)
library(cowplot)
source("r_scripts/simulate_FRED_ab.R")
source("r_scripts/plot_FRED.R")
source("r_scripts/get_MLE_traces.R")
source("r_scripts/nll_FRED_ab.R")

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
    
    plotOutput("plot_example_mouse"),
    actionButton(
        inputId = "run_optim",
        label   = 'Fit model "ab" using optim',
        icon    = icon("calculator")
    ),
    
    plotOutput("optim", height = "300px")
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
#TODO: move this to where is belongs

# Only fit the optim when users ask for it
optim_result <- eventReactive(input$run_optim, {
    
    get_MLE_traces(data = sim_data())

})
 
# Plots the new optim  result    
output$optim <- renderPlot({
    req(optim_result()) # ensure plot exists
    optim_result()
})
}
shinyApp(ui, server)
