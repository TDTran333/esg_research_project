f_summarize_plot <- function(df, x) {
  {{ df }} %>%
    group_by(date) %>%
    filter(!is.na({{ x }})) %>%
    summarize(mean = mean({{ x }}),
              median = median({{ x }}),
              q25 = quantile({{ x }}, 0.25),
              q75 = quantile({{ x }}, 0.75)) %>%
    pivot_longer(c(mean, median), names_to = "measure")
}