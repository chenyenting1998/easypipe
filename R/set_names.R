#' Set column or row names
#' This function changes the names of the rows or columns of a matrix-like object. It can also be used with the pipe operator `%>%`.
#' @param data Any matrix-like objects that accept `rownames` and `colnames`.
#' @param change A character string ("row" or "col") for choosing whether the names of rows or columns to be changed. Default is "col".
#' @param names  A vector that contains the names of for the columns or rows.
#'
#' @return
#' @export
#'
#' @examples
#' data(iris)
#' setnames(object = iris, change = "col", names = rep("a", 5))
set_names <- function(data, change = "col", names){
  if (change == "col"){
    colnames(data) <- names
    return(data)
  }
  if (change == "row"){
    rownames(data) <- names
    return(data)
  }else{
    print("error")
    stop()
  }
}
