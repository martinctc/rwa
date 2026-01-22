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
#'
#' library(ggplot2)
#' # Use a smaller sample for faster execution
#' diamonds_small <- diamonds[sample(nrow(diamonds), 1000), ]
#' diamonds_small %>%
#'   rwa(outcome = "price",
#'       predictors = c("depth","carat", "x", "y", "z"),
#'       applysigns = TRUE) %>%
#'   plot_rwa()
#'
#'
#' @export
plot_rwa <- function(rwa){
  result <- rwa$result %>% # Extract results
    dplyr::mutate(Sign.Rescaled.RelWeight = ifelse(Sign == "-",
                                                   Rescaled.RelWeight * -1,
                                                   Rescaled.RelWeight))
 
  # Get variable names from the Variables column
  if (!"Variables" %in% names(result)) {
    stop("Could not find 'Variables' column in result.")
  }
  result$variable_name <- result$Variables

  # Calculate appropriate axis limits for both positive and negative values
  max_abs_weight <- max(abs(result$Sign.Rescaled.RelWeight))
  min_weight <- min(result$Sign.Rescaled.RelWeight)
  max_weight <- max(result$Sign.Rescaled.RelWeight)
  
  # Set symmetric or appropriate limits based on data
  if (min_weight < 0 && max_weight > 0) {
    # Mixed signs: use symmetric limits
    y_limits <- c(-max_abs_weight * 1.15, max_abs_weight * 1.15)
  } else if (min_weight < 0) {
    # All negative: extend left for labels
    y_limits <- c(min_weight * 1.15, max(0, max_weight * 1.1))
  } else {
    # All positive: extend right for labels
    y_limits <- c(min(0, min_weight), max_weight * 1.15)
  }

  result %>%
    ggplot(aes(x = stats::reorder(variable_name, Sign.Rescaled.RelWeight), 
               y = Sign.Rescaled.RelWeight)) +
    geom_col(fill = "#0066cc") +
    geom_text(aes(label = round(Sign.Rescaled.RelWeight, 1),
                  hjust = ifelse(Sign.Rescaled.RelWeight >= 0, -0.3, 1.3))) +
    coord_flip() +
    ylim(y_limits) +
    labs(title = "Variable importance estimates",
         subtitle = "Using the Relative Weights Analysis method",
         x = "Predictor variables",
         y = "Rescaled Relative Weights (with sign)",
         caption = paste0("Note: Absolute Rescaled Relative Weights sum to 100%. n = ",
                          rwa$n, ". ",
                          if (!is.null(rwa$rsquare)) {
                            paste0("R-squared: ", round(rwa$rsquare, 2), ".")
                          } else {
                            ""
                          })) +
    theme_classic()
}
