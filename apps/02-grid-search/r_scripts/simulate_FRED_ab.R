simulate_FRED_ab <- function(alpha = 0.8,
                             beta = 0.1,
                             n_trials = 60) {
    
    ratios <- c(1, 5, 10, 20)
    
    df <- data.frame(trial = 1:n_trials,
                     ratio = rep(x = ratios,
                                 length.out = n_trials))
    
    # alpha is the indifference point, we will refer to this as reward bias
    # beta is slope, we will refer to this as effort sensitivity
    
    df$value_high <- -beta * (df$ratio - alpha)
    
    df$prob_high <- 1 / (1 + exp(-df$value_high))
    
    df$chose_high <- rbinom(n = n_trials,
                            size = 1,
                            prob = df$prob_high)
    
    return(df)
    
}