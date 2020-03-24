
#' Define a view function that will recognize it is in RStudio
#'
#' See the discussion here: https://stackoverflow.com/questions/48234850/how-to-use-r-studio-view-function-programatically-in-a-package.
#' Turns out that we can't just call View directly, because it needs more
#' information about the environment it is in.  I won't export this, cuz it will just get used
#' within a function in this package.
myView <- function(x, title) {
  get("View", envir = as.environment("package:utils"))(x, title)
}
