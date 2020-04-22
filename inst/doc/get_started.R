## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(sanityTracker)
sc <- sanityTracker::add_sanity_check(
  fail_vec = mtcars$mpg > 30,
  description = "mpg should be below 30"
)
get_sanity_checks()

## -----------------------------------------------------------------------------
sc <- sanityTracker::add_sanity_check(
  fail_vec = mtcars$mpg > 30,
  description = "mpg should be below 30. extract example ",
  data = mtcars,
  param_name = "mpg"
)
get_sanity_checks()

## -----------------------------------------------------------------------------
get_sanity_checks()[["example"]][[2]]

## -----------------------------------------------------------------------------
g <- function(x) {
  sanityTracker::add_sanity_check(
    fail_vec = x$mpg > 30,
    description = "mpg should be below 30. check in function",
    data = x,
    param_name = "mpg"
  )
}
f <- function(x) {g(x = x)}
dummy <- f(x = mtcars)
get_sanity_checks()

## -----------------------------------------------------------------------------
sanityTracker::clear_sanity_checks()
sanityTracker::get_sanity_checks()

## -----------------------------------------------------------------------------
sc <- sanityTracker::sc_cols_positive(
  object = mtcars,
  cols = names(mtcars),
  description = "Exemplary sanity checks"
)
get_sanity_checks()

## -----------------------------------------------------------------------------
clear_sanity_checks()
sc <- sanityTracker::sc_col_elements(
  object = mtcars,
  col = "carb",
  feasible_elements = 1:4,
  description = "Only usual number of carburetors",
  fail_callback = warning,
  call = "directly from vignette"
)
get_sanity_checks()

