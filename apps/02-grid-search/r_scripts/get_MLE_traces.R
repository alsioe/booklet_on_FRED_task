get_MLE_traces <- function(data) {
    
    make_tracer <- function(start_id) {
        
        path <- list()
        i <- 0
        
        tracer <- function(par, data) {
            
            i <<- i + 1
            
            path[[i]] <<- c(
                alpha = par[1],
                beta  = par[2]
            )
            
            nll_FRED_ab(par = par, data = data)
        }
        
        get_path <- function() {
            
            do.call(rbind, path) %>%
                as.data.frame() %>%
                mutate(
                    iter  = row_number(),
                    start = start_id
                )
        }
        
        list(
            fn = tracer,
            get_path = get_path
        )
    }
    
    # Set starting points
    starts <- list(
        c(10, 0.8),
        c(5, 2),
        c(15, 0.1),
        c(20, 0.5)
    )
    
    # Run all paths
    all_paths <- list()
    
    for (s in seq_along(starts)) {
        
        tracer_obj <- make_tracer(paste0("start_", s))
        
        fit <- optim(
            par = starts[[s]],
            data = data,
            lower = c(0, 0.001),
            upper = c(30, 3),
            fn = tracer_obj$fn,
            method = "L-BFGS-B"
        )
        
        all_paths[[s]] <- tracer_obj$get_path()
    }
    
    df_paths <- bind_rows(all_paths)
    
    # final MLE point per trajectory
    df_final <- df_paths %>%
        group_by(start) %>%
        slice_tail(n = 1) %>%
        ungroup()
    
    # starting points per trajectory
    df_start <- df_paths %>%
        group_by(start) %>%
        slice_head(n = 1) %>%
        ungroup()
    
    ggplot(df_paths, aes(alpha, beta, colour = start)) +
        
        # optimisation paths
        geom_line(linewidth = 1) +
        
        # final MLE estimate (open circles)
        geom_point(
            data = df_final,
            aes(alpha,
                beta,
                colour = start,
                fill = start),
            shape = 21,
            stroke = 1.5,
            size = 5,
            alpha = 0.2
        ) +
        
        # starting points MLE estimate (open circles)
        geom_point(
            data = df_start,
            aes(alpha,
                beta,
                colour = start,
                fill = start),
            shape = 21,
            stroke = 1.5,
            size = 3,
            alpha = 0.2
        ) +
        
        theme_bw()
}