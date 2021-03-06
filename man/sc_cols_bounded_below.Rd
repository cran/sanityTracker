% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convenience.R
\name{sc_cols_bounded_below}
\alias{sc_cols_bounded_below}
\title{Checks that all elements from the given columns are above a certain number}
\usage{
sc_cols_bounded_below(
  object,
  cols,
  lower_bound,
  include_lower_bound = TRUE,
  ...
)
}
\arguments{
\item{object}{table with a columns specified by \code{cols}}

\item{cols}{vector of characters of columns that are checked against
the specified range}

\item{lower_bound}{elements of the specified columns must be above this
bound}

\item{include_lower_bound}{if TRUE (default), elements are allowed to be
equal to the \code{lower_bound}}

\item{...}{further parameters that are passed to \link{add_sanity_check}.}
}
\value{
list of logical vectors where TRUE indicates where the check failed.
  Every list entry represents one of the columns specified in cols.
  This might be helpful if one wants to apply a counter-measure
}
\description{
Checks that all elements from the given columns are above a certain number
}
\examples{
d <- data.frame(a = c(0, 0.2, 3, Inf), b = c(1:4))
dummy_call <- function(x) {
  sc_cols_bounded_below(
    object = d, cols = c("a", "b"),
    lower_bound = 0.2,
    include_lower_bound = FALSE,
    description = "Measurements are expected to be bounded from below")
}
dummy_call(x = d)
get_sanity_checks()
}
