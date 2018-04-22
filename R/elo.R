#' The Elo Package
#'
#' An implementation of Elo ratings for general use in 'R'.
#'
#' @section Functions:
#'
#' Listed below are the most useful functions available in \code{elo}:
#'
#' \code{\link{elo.prob}}: Calculate the probability that team A beats team B.
#'
#' \code{\link{elo.update}}: Calculate the update value for a given Elo matchup.
#'
#' \code{\link{elo.calc}}: Calculate post-update Elo values.
#'
#' \code{\link{elo.run}}: Calculate Elos for a series of matches.
#'
#' \code{\link{score}}: Create a 1/0/0.5 win "indicator" based on two teams' scores.
#'
#' @section Data:
#'
#' \code{\link{tournament}}: Mock data for examples.
#'
#' @examples
#' library(elo)
#' @references Elo, A. E. 1978. The Rating of Chess Players, Past and Present. New York: Arco.
#'
#' @importFrom stats predict fitted residuals weights
#' @importFrom pROC auc
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
