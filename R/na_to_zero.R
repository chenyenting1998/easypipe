#' Converting NAs to zeros
#' Often used after pivoting the data from long format to wide format. Although the empty entries are NAs in essence, we still need to cenvert them to zero in order to perform the calculations.
#' @param data Any object (e.g. data.frame, vector).
#'
#' @return
#' @export
#'
#' @examples
#' x <- c(1:3, NA, "a", "b", NA)
#' na_to_zero(x)
na_to_zero <- function(data) {
  find_na <- as.matrix(data) %>% is.na()
  data[find_na] <- 0
  data
}
