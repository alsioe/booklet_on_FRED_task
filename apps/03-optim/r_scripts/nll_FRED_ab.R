nll_FRED_ab <- function(data,
                        par) {
    
    alpha <- par[1]
    beta <- par[2]
    
    data$value_high <- -beta * (data$ratio - alpha)
    
    data$prob_high <- 1 / (1 + exp(-data$value_high))
    
    # Not needed - the choices are already made!    
    # df$chose_high <- rbinom(n = n_trials,
    #                         size = 1,
    #                         prob = df$prob_high)
    
    neg_log_lik <-
        -sum(dbinom(x = data$chose_high,
                    size = 1,
                    prob = data$prob_high,
                    log = TRUE)
        )
    
    # optim() needs the negative log likelihood to have something to minimise
    return(neg_log_lik)
    
}