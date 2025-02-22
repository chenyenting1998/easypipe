% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_names.R
\name{set_names}
\alias{set_names}
\title{Set column or row names
This function changes the names of the rows or columns of a matrix-like object. It can also be used with the pipe operator `%>%`.}
\usage{
set_names(data, change = "col", names)
}
\arguments{
\item{data}{Any matrix-like objects that accept `rownames` and `colnames`.}

\item{change}{A character string ("row" or "col") for choosing whether the names of rows or columns to be changed. Default is "col".}

\item{names}{A vector that contains the names of for the columns or rows.}
}
\value{

}
\description{
Set column or row names
This function changes the names of the rows or columns of a matrix-like object. It can also be used with the pipe operator `%>%`.
}
\examples{
data(iris)
setnames(object = iris, change = "col", names = rep("a", 5))
}
