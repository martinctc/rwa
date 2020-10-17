#' @title Remove any columns where all the values are missing
#'
#' @description Pass a data frame and returns a version where all columns
#' made up of entirely missing values are removed.
#'
#' @details
#' This is used within `rwa()`.
#'
#' @param df Data frame to be passed through.
#'
#'
#' @export
remove_all_na_cols <- function(df){
  Filter(function(x)!all(is.na(x)),df)
}
