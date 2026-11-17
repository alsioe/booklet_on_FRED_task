# Extracted from the original FRED teaching document.
# Requires the supporting scripts listed in README.md.
library(shiny)
library(tidyverse)
library(cowplot)
source("r_scripts/simulate_FRED_ab.R")
source("r_scripts/plot_FRED.R")
source("r_scripts/map_FRED_ab.R")
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

fluidRow(
    column(
        width = 6,
        numericInput(
            inputId = "prior_alpha_mean",
            label   = "Prior on alpha: mean (location)",
            value   = 15,
            min     = 1,
            max     = 30,
            step    = 1
        )
    ),
    column(
        width = 6,
        numericInput(
            inputId = "prior_alpha_sd",
            label   = "Prior on alpha: standard deviation (shape)",
            value   = 5,
            min     = 1,
            max     = 10,
            step    = 0.5
        )
    )
),

fluidRow(
    column(
        width = 6,
        numericInput(
            inputId = "prior_beta_shape",
            label   = "Prior on beta: shape",
            value   = 1.5,
            min     = 0.5,
            max     = 3,
            step    = 0.1
        )
    ),
    column(
        width = 6,
        numericInput(
            inputId = "prior_beta_scale",
            label   = "Prior on beta: scale",
            value   = 1,
            min     = 0.5,
            max     = 1.5,
            step    = 0.1
        )
    )
),

plotOutput("priors", height = "300px"),
actionButton(
    inputId = "run_grid_search",
    label   = "Fit the model using grid search",
    icon    = icon("calculator")
),

plotOutput("grid_search", height = "300px")
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
# Plot the priors
priors_result <- reactive({

    norm_x <- seq(from = 0, to = 30, length.out = 100)
    gamma_x <- seq(from = 0.01, to = 2, length.out = 100)

    # Create a nested for loop to go through every combination of alpha and beta
    for (alpha in seq(0, 30, by = 1)) {
        
        # In the very first loop, create a data frame to store the values
        if (alpha == 0) {
            df <- data.frame(
                alpha = vector(),
                beta = vector(),
                nll = vector()
            )
        }
        
        for (beta in seq(0.01, 2, by = 0.1)) {
            
            # Calculate the prior nll for each parameter combo ...
            nll <- - dnorm(x = alpha,
                             mean = input$prior_alpha_mean,
                             sd = input$prior_alpha_sd,
                             log = TRUE) +
                     - dgamma(x = beta,
                              shape = input$prior_beta_shape,
                              scale = input$prior_beta_scale,
                              log = TRUE)
            
            # And add the corresponding line to the data frame
            df <- rbind(df,
                        data.frame(
                            alpha = alpha,
                            beta = beta,
                            nll = nll)
                        )
            
        } # end of beta loop
        
    } # end of alpha loop
    
    plot_prior_alpha <- ggplot(
        data.frame(
            x = norm_x,
            y = dnorm(norm_x,
                      input$prior_alpha_mean,
                      input$prior_alpha_sd)
            ),
        aes(x, y)) +
        geom_line() +
        theme_bw() +
        # Labs
        labs(title = 'Prior on alpha')

    plot_prior_beta <- ggplot(
        data.frame(
            x = gamma_x,
            y = dgamma(gamma_x,
                       shape = input$prior_beta_shape,
                       scale = input$prior_beta_scale)
            ),
        aes(x, y)) +
        lims(x = c(0, 2)) +
        coord_flip() +
        geom_line() +
        theme_bw() +
        # Labs
        labs(title = 'Prior on beta')
    
    
    plot_heatmap_prior <- # Plot the priors in a heatmap
        ggplot(data = df,
               aes(x = alpha,
                   y = beta)) +
        
        # Plot one tile per grid cell
        geom_tile(aes(fill = nll),
                  colour = "white") +
        
        scale_fill_gradientn(
            name = waiver(),
            colours = c(
                "#f7f7f7",  # near white
                "#d9d9d9",
                "#bdbdbd",
                "#969696",
                "#636363"
            ),
            values = c(0, 0.015, 0.05, 0.15, 1),
            na.value = "#e0e0e0"
        ) +
        
        # Remove grey background
        theme_bw() +
        
        # Labs
        labs(title = 'Joint priors')
    
    cowplot::plot_grid(plotlist = list(cowplot::plot_grid(plotlist = list(plot_prior_alpha,
                                                                          NULL),
                                                          rel_widths = c(2,3)),
                                       cowplot::plot_grid(plotlist = list(plot_heatmap_prior,
                                                                          plot_prior_beta))
                                       ),
                       nrow = 2)
    
    })

# Plots the priors    
output$priors <- renderPlot({
    req(priors_result()) # ensure plot exists
    priors_result()
})
# Only fit the grid search etc. when users ask for it
grid_search_result <- eventReactive(input$run_grid_search, {
    
    req(sim_data())   # ensure data exist
    
    map_FRED_ab(
      data = sim_data(),
      priors_on_alpha = c(
        input$prior_alpha_mean,
        input$prior_alpha_sd
      ),
      priors_on_beta = c(
        input$prior_beta_shape,
        input$prior_beta_scale
      )
    )

})
    
# Plots the new grid-search result    
output$grid_search <- renderPlot({
    req(grid_search_result()) # ensure plot exists
    grid_search_result()
})
}
shinyApp(ui, server)
