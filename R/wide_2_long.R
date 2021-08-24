#' Convert a data with Wide format to long format
#' A simplified `pivot_longer` transfomation for easy ggplotting. Name of the columns were stored in `variables` while the values were stored in `values`.
#' For a more detailed pivot, just use the `pivot_longer` function in the package `tidyr`.
#' @param wide_data The wide data object.
#' @param fixed_variables The variables that you wish not to be involved with the wide-to-long process.
#'
#' @return
#' @export
#'
#' @examples
#' data(iris)
#' wide_2_long(wide_data = iris, fixed_variables = "Species")
wide_2_long <- function(wide_data, fixed_variables){

  if(missing(fixed_variables)){
    df <-
      wide_data %>%
      pivot_longer(cols = everything(), # remove factorized information
                   names_to = "variables",
                   values_to = "values")
    return(df)
  }

  # Numeric inputs
  if(is.numeric(fixed_variables)){
    df <-
      wide_data %>%
      pivot_longer(cols = - fixed_variables, # remove factorized information
                   names_to = "variables",
                   values_to = "values")
    return(df)
  }

  # character inputs
  if(is.character(fixed_variables)){
    df <-
      wide_data %>%
      pivot_longer(cols = - fixed_variables, # remove factorized information
                   names_to = "variables",
                   values_to = "values")
    return(df)
  }
}
