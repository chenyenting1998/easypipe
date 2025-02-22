% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wide_2_long.R
\name{wide_2_long}
\alias{wide_2_long}
\title{Convert a data with Wide format to long format
A simplified `pivot_longer` transfomation for easy ggplotting. Name of the columns were stored in `variables` while the values were stored in `values`.
For a more detailed pivot, just use the `pivot_longer` function in the package `tidyr`.}
\usage{
wide_2_long(wide_data, fixed_variables)
}
\arguments{
\item{wide_data}{The wide data object.}

\item{fixed_variables}{The variables that you wish not to be involved with the wide-to-long process.}
}
\value{

}
\description{
Convert a data with Wide format to long format
A simplified `pivot_longer` transfomation for easy ggplotting. Name of the columns were stored in `variables` while the values were stored in `values`.
For a more detailed pivot, just use the `pivot_longer` function in the package `tidyr`.
}
\examples{
data(iris)
wide_2_long(wide_data = iris, fixed_variables = "Species")
}
