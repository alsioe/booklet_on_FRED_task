map_FRED_ab <- function(data,
                       priors_on_alpha = c(10, 3),
                       priors_on_beta = c(1.5, 1)) {
    
    # Create a nested for loop to go through every combination of alpha and beta
    for (alpha in seq(1, 30, by = 1)) {
        
        # In the very first loop, create a data frame to store the values
        if (alpha == 1) {
            df <- data.frame(
                alpha = vector(),
                beta = vector(),
                nll = vector(),
                prior = vector(),
                map = vector()
            )
        }
        
        for (beta in seq(0.1, 3, by = 0.1)) {
            
            # Calculate the sum of squared errors ...
            nll <- nll_FRED_ab(data = data,
                               par = c(alpha, beta))
            
            # Calculate the prior for each parameter combo ...
            prior <- - dnorm(x = alpha,
                             mean = priors_on_alpha[1],
                             sd = priors_on_alpha[2],
                             log = TRUE) +
                - dgamma(x = beta,
                         shape = priors_on_beta[1],
                         scale = priors_on_beta[2],
                         log = TRUE)
            
            # Combine them
            map <- nll + prior
            
            # And add the corresponding line to the data frame
            df <- rbind(df,
                        data.frame(
                            alpha = alpha,
                            beta = beta,
                            nll = nll,
                            prior = prior,
                            map = map)
            )
            
        } # end of alpha loop
        
    } # end of beta loop
    
    # We can find the best values from the grid search using this code
    grid_result_nll <-
        df %>%
        # Sort by negative log likelihood
        dplyr::arrange(nll) %>%
        # Display the first line only
        head(1)
    
    # And again, but using the maximum a posteriori approach
    grid_result_map <-
        df %>%
        # Sort by negative log likelihood
        dplyr::arrange(map) %>%
        # Display the first line only
        head(1)
    
    # We can plot the resulting values in a heatmap with ggplot
    plot_heatmap_nll <-
        ggplot(data = df,
               aes(x = alpha,
                   y = beta)) +
        
        # Plot one tile per grid cell
        geom_tile(aes(fill = nll),
                  colour = "white") +
        
        # Add a gradient that highlights the lowest nll's
        scale_fill_gradientn(
            name = waiver(),
            colours = c(
                "#ffffff",
                "#fdd49e",
                "#fdbb84",
                "#e34a33"
            ),
            values = c(0, 0.4, 0.7, 1),
            na.value = "#e0e0e0"
        ) +
        
        # Indicate the winning alpha (according to nll)
        geom_vline(xintercept = grid_result_nll$alpha,
                   linetype = 'dotted') +
        
        # Indicate the winning beta (according to nll)
        geom_hline(yintercept = grid_result_nll$beta,
                   linetype = 'dotted') +
        
        # Remove grey background
        theme_bw() +
        
        # Labs
        labs(title = 'Negative log likelihood (grid-search)')
    
    # Plot the priors in a heatmap
    plot_heatmap_priors <-
        ggplot(data = df,
               aes(x = alpha,
                   y = beta)) +
        
        # Plot one tile per grid cell
        geom_tile(aes(fill = prior),
                  colour = "white") +
        
        # Add a gradient that highlights the lowest nll's
        # scale_fill_gradientn(name = waiver(),
        #                      colours = c('darkred', 'red', 'pink',
        #                                  'white', 'lightblue', 'blue'),
        #                      values = c(0, 0.01, 0.05, 0.1, 0.2, 0.5, 1),
        #                      na.value = "#e0e0e0") +
        
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
        labs(title = 'Priors')
    
    # We can plot the resulting values in a heatmap with ggplot
    plot_heatmap_map <-
        ggplot(data = df,
               aes(x = alpha,
                   y = beta)) +
        
        # Plot one tile per grid cell
        geom_tile(aes(fill = map),
                  colour = "white") +
        
        # Add a gradient that highlights the lowest nll's
        scale_fill_gradientn(
            name = waiver(),
            colours = c(
                "#ffffff",
                "#fdd49e",
                "#fdbb84",
                "#e34a33"
            ),
            values = c(0, 0.4, 0.7, 1),
            na.value = "#e0e0e0"
        ) +
        
        # Indicate the winning alpha (according to nll)
        geom_vline(xintercept = grid_result_nll$alpha,
                   linetype = 'dotted') +
        
        # Indicate the winning beta (according to nll)
        geom_hline(yintercept = grid_result_nll$beta,
                   linetype = 'dotted') +
        
        # Indicate the winning alpha (according to map)
        geom_vline(xintercept = grid_result_map$alpha,
                   linetype = 'dashed') +
        
        # Indicate the winning beta (according to map)
        geom_hline(yintercept = grid_result_map$beta,
                   linetype = 'dashed') +
        
        # Remove grey background
        theme_bw() +
        
        # Labs
        labs(title = 'Maximum a posteriori method')
    
    # One plot to rule them all!
    plot_heatmap <- cowplot::plot_grid(plotlist = list(plot_heatmap_nll,
                                                       plot_heatmap_priors,
                                                       plot_heatmap_map),
                                       nrow = 1)
    
    return(plot_heatmap)
    
}