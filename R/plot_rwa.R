#' @title Plot the rescaled importance values from the output of `rwa()`
#'
#' @description Pass the output of `rwa()` and plot a bar chart of the rescaled importance values.
#' Signs are always calculated and taken into account, which is equivalent to setting the `applysigns`
#' argument to `TRUE` in `rwa()`.
#'
#' @import ggplot2
#'
#' @param rwa Direct list output from `rwa()`.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' diamonds %>%
#'   rwa(outcome = "price",
#'       predictors = c("depth","carat", "x", "y", "z"),
#'       applysigns = TRUE) %>%
#'   plot_rwa()
#' }
#'
#' @export
plot_rwa <- function(rwa){
  result <- rwa$result %>% # Extract results
    dplyr::mutate(Sign.Rescaled.RelWeight = ifelse(Sign == "-",
                                                   Rescaled.RelWeight * -1,
                                                   Rescaled.RelWeight))

  max_weight <- max(result$Sign.Rescaled.RelWeight)

  result %>%
    ggplot(aes(x = stats::reorder(Variables, Sign.Rescaled.RelWeight), y = Sign.Rescaled.RelWeight)) +
    geom_col(fill = "#0066cc") +
    geom_text(aes(label = round(Sign.Rescaled.RelWeight, 1)), hjust = -0.3) +
    coord_flip() +
    ylim(c(NA, max_weight * 1.1)) + # Automatic lower limit
    labs(title = "Variable importance estimates",
         subtitle = "Using the Relative Weights Analysis method",
         x = "Predictor variables",
         y = "Rescaled Relative Weights",
         caption = paste0("Note: Rescaled Relative Weights sum to 100%. n = ",
                          rwa$n, ". ",
                          "R-squared: ",
                          round(rwa$rsquare, 2),
                          ".")) +
    theme_classic()
}
