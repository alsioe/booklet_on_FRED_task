plot_FRED <- function(data) {
    
    data %>%
        group_by(ratio) %>%
        summarise(chose_high = mean(chose_high)) %>%
        ggplot(aes(x = ratio,
                   y = chose_high)) +
        geom_line() +
        geom_point(data = data,
                   position = position_jitter(width = 0.5,
                                              height = 0),
                   colour = 'turquoise',
                   size = 5,
                   alpha = 0.2) +
        geom_hline(yintercept = 0.5,
                   linetype = 'dashed') +
        theme_bw()

}
