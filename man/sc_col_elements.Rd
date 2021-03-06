% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convenience.R
\name{sc_col_elements}
\alias{sc_col_elements}
\title{Checks that the elements of a column belong to a certain set}
\usage{
sc_col_elements(object, col, feasible_elements, ...)
}
\arguments{
\item{object}{table with a column specified by \code{col}}

\item{col}{name as a character of the column which is checked}

\item{feasible_elements}{vector with characters that are feasible
for \code{col}. Note that an element that is NA it is always
counted as a fail if \code{feasible_elements} does not
explicitly contains NA.}

\item{...}{further parameters that are passed to \link{add_sanity_check}.}
}
\value{
see return object of \link{add_sanity_check}
}
\description{
Checks that the elements of a column belong to a certain set
}
\examples{
d <- data.frame(type = letters[1:4], nmb = 1:4)
dummy_call <- function(x) {
  sc_col_elements(object = d, col = "type", feasible_elements = letters[2:4])
}
dummy_call(x = d)
get_sanity_checks()
}
