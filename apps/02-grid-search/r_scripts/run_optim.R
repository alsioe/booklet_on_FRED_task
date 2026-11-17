run_optim <- function(data = data,
                      fn = nll_FRED_ab,
                      n_iter = 10,
                      method = 'L-BFGS',
                      lower = c(1, 0.01),
                      upper = c(30, 10),
                      only_best_fit = TRUE
) {
    
    # Create an empty list to store the optim() results in
    output <- list()
    
    # Loop over the number of iterations suggested
    for (i in 1:n_iter) {
        
        if (i == 1) {
            df <- data.frame(
                iter = vector(length = n_iter),
                value = vector(length = n_iter),
                convergence = vector(length = n_iter)
            )
            
            for (par in 1:length(lower)) {
            
                df <- cbind(df,
                            vector(length = n_iter)
                            )
                
                names(df)[3 + par] <- paste0('par', par)
            }
        }
        
        starting_par <- vector(length = length(lower))
        
        # The starting values for each loop will be randomly sampled from a
        # uniform distribution with boundaries as specified in lower and upper
        for (par in 1:length(lower)) {
            starting_par[par] <- runif(n = 1,
                                       min = lower[par],
                                       max = upper[par])
            
        }
        
        # Run the optim() function with the starting values sampled above.
        # storing the result into the output list
        output[[i]] <- optim(par = starting_par,
                             fn = fn,
                             data = data,
                             method = method,
                             lower = lower,
                             upper = upper)
        
    
        # Create output list
        df$iter[i] <- i
        df$value[i] <- output[[i]]$value
        df$convergence[i] <- output[[i]]$convergence
        df[i, 4:(3 + length(lower))] <- output[[i]]$par
    
    }
    
    # Choose whether to return the best fit only or the whole output list 
    if (only_best_fit == TRUE) {
      
      # This line of code ensures we get the first occurence of the best fit
      best <- df %>%
        filter(value == min(value)) %>%
        head(n = 1)
      
      # Return the best fit only
      return(list(par = best[, 4:(3 + length(lower))],
                  value = best$value,
                  convergence = best$convergence)
               )
    
    } else if (only_best_fit == FALSE) {
    
      # Return the full output list
      return(df)
    
    }
    
}
