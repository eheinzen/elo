#' The Elo Package
#'
#' An implementation of Elo ratings for general use in 'R'.
#'
#' @section Functions:
#'
#' Below are listed some of the functions available in \code{elo}:
#'
#' \code{\link{elo.calc}}: Calculate Elos for a series of matches.
#'
#' @section Data:
#'
#' \code{\link{tournament}}: Mock data for examples.
#'
#' @examples
#' library(elo)
#' @references Elo, A. E. 1978. The Rating of Chess Players, Past and Present. New York: Arco.
#' @docType package
#' @name elo
#'
NULL

#' @useDynLib elo
#' @importFrom Rcpp sourceCpp
NULL

#### commands to build the package using devtools
# devtools::check_man()
# devtools::test()
# devtools::check()
# devtools::install(build_vignettes = TRUE, dependencies = FALSE))
# devtools::build("../elo/")
## < restart R >
## library(elo")

#### to upload to CRAN
## Update DESCRIPTION, README.md, NEWS.md, and cran-comments.md
# devtools::revdep_check()
# devtools::release()
