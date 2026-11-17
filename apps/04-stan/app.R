# Extracted from the original FRED teaching document.
# Requires the supporting scripts listed in README.md.
library(shiny)
library(tidyverse)
library(cowplot)
library(rstan)
library(bayesplot)
library(posterior)
rstan::rstan_options(auto_write = TRUE)
mod_ab <- rstan::stan_model("stan_scripts/FRED_ab.stan", model_name = "FRED_ab")
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
    
    stan_file <- file.path('stan_scripts',
                           'FRED_ab.stan'),
    
    print(stan_file),
    
    cat(readLines(stan_file),
        sep = "\n"),
    
    actionButton(
        inputId = "run_stan_ab",
        label   = 'Fit model "ab" using Stan',
        icon    = icon("calculator")
    ),
    
    plotOutput("stan_ab",
               height = "600px")
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
    stan_ab_result <- eventReactive(input$run_stan_ab, {
    
        # Read data from sim_data() for easier handling
        data <- sim_data()
    
        # Sorting errors
        # input <- list(prior_alpha_mean = 10,
        #               prior_alpha_sd = 3,
        #               prior_beta_shape = 1.5,
        #               prior_beta_scale = 1)
        
        # Arrange the data for the specific Stan script
        data_for_stan <- list(n_trials = nrow(data),
                              ratio = data$ratio,
                              chose_large = data$chose_high,
                              
                              alpha_prior_mean = input$prior_alpha_mean,
                              alpha_prior_sd =  input$prior_alpha_sd,
                              beta_prior_shape = input$prior_beta_shape,
                              beta_prior_scale = input$prior_beta_scale)
    
        # Insert progress bar to check on the sampling
        withProgress(message = 'Sampling ...', value = 0, {
        
            setProgress(value = 0.5,
                        detail = 'FITTING STAN MODEL TO DATA')
            
            # Fit the Stan model to the data
            fit_ab <- rstan::sampling(data = data_for_stan,
                                      object = mod_ab,
                                      chains = 4,
                                      cores = 1,
                                      iter = 1000,
                                      warmup = 500,
                                      # Keep the warmups for illustration
                                      save_warmup = TRUE)
            
            setProgress(value = 1,
                        detail = 'STAN FINISHED')
            
            fit_ab
            
        })
    
    })
    
    # Plots the Stan result from the ab model    
    output$stan_ab <- renderPlot({
        
        message("RENDER PLOT TRIGGERED")
        
        req(stan_ab_result()) # ensure the fit exists
        
        message("FIT AVAILABLE")
        
        fit <- stan_ab_result()
        
        posterior_warmup <-
            posterior::as_draws_df(rstan::extract(object = fit,
                                                  permuted = FALSE,
                                                  inc_warmup = TRUE))
        
        posterior <-
            posterior::as_draws_df(rstan::extract(object = fit,
                                                  permuted = FALSE,
                                                  inc_warmup = FALSE))
    
    # Plot traceplot    
    
        warmup <- 500
    
        plot_trace_ab <-
          bayesplot::mcmc_trace(
              posterior_warmup,
              pars = c('alpha', 'beta')
              ) +
            annotate(
                'rect',
                xmin = 0.5,
                xmax = warmup + 0.5,
                ymin = -Inf,
                ymax = Inf,
                alpha = 0.15,
                fill = 'grey60'
                ) +
            geom_vline(
            xintercept = warmup + 0.5,
            linetype = 2,
            linewidth = 0.6
            ) +
            labs(
                title = 'Trace plot',
                subtitle = 'Grey region = warmup (discarded)'
                ) +
            theme_bw()
        
        plot_scatter <- 
            ggplot(data = posterior,
                   aes(x = alpha,
                       y = beta)) +
            geom_point(
                shape = 21,
                size = 3,
                alpha = 0.2,
                colour = '#003366',  # outline
                fill = '#9ecae1'     # inside
            ) +
            theme_bw() +
            labs(title = 'Posterior samples',
                 subtitle = 'Note warmup samples discarded')
        
        plot_paths <- 
            ggplot(data = posterior %>%
                       filter(.chain == 1,
                              .iteration < 50),
                   aes(x = alpha,
                       y = beta)) +
            geom_path(
                aes(group = .chain),
                colour = 'lightblue',
                alpha = 0.6
                ) +
            geom_path(
                data = posterior_warmup %>%
                       filter(.chain == 1,
                              .iteration < 50),
                aes(group = .chain),
                colour = 'darkgrey',
                alpha = 0.6
                ) +
            theme_bw() +
            labs(
                title = 'HMC exploration of parameter space',
                subtitle = 'Warmup: grey | Sampling: light blue (first 50 iterations)'
                )
        
        plot_areas_ab <-
            bayesplot::mcmc_areas(posterior,
                                  pars = c('alpha', 'beta'),
                                  prob = 0.8) +
            labs(title = 'Posterior distributions',
                 subtitle = 'with medians and 80% intervals') +
            theme_bw()
        
        message("PLOTS CONSTRUCTED")
        
        cowplot::plot_grid(plot_trace_ab,
                           cowplot::plot_grid(plot_paths,
                                              plot_scatter),
                           plot_areas_ab,
                           ncol = 1)
        
    })
    

}
shinyApp(ui, server)
