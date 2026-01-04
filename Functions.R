#' @title Visualization trend of GBD data
#' @param EPI data.frame queried by function [query_GBD].
#' @param group grouped by variable.
#' @param Measure `Deaths`, `Incidence`, `DALYs` (Disability-Adjusted Life Years), `YLDs` (Years Lived with Disability) or `YLLs` (Years of Life Lost).
#' @param Metric type of ratio plot: `Rate` or `Counts`.
#' @param Sex Can be "Both" (default), "Male", 'Female'.
#' @param fun calculate method, e.g., sum, mean, median, etc.
#' 
#' @export 
#' @return a ggplot object
#' 
trendGBD <- function(EPI, group = "sex", Measure = "Deaths", Metric = "Rate", Sex = "Both", fun = 'sum') {
    library(ggplot2, quietly = T)
    group <- group
    #
    EPI %>%
        dplyr::filter(measure == Measure, metric == Metric, sex %in% Sex) %>%
        dplyr::group_by(get(group), year) %>%
        dplyr::mutate(
            mval = do.call(fun, list(val)),
            mlower = do.call(fun, list(lower)),
            mupper = do.call(fun, list(upper))
        ) %>%
        ggplot(aes_string("year", "mval", color = group)) +
        geom_ribbon(
            aes_string(ymin = "mlower", ymax = "mupper", fill = group),
            alpha = 0, linetype = 0
        ) +
        geom_line(aes_string(color = group), lwd = 1) +
        labs(
            y = paste(Measure, switch(Metric,
                "Rate" = "(per 10^5)",
                "Percent" = "(%)"
            )),
            x = NULL,
            caption = if (standard) "Standardized" else NULL
        ) +
        theme_pubclean(15) +
        theme(legend.title = element_blank())
}


