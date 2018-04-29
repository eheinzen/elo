
#' @rdname formula.specials
#' @export
players <- function(..., weights = NULL)
{
  args <- lapply(list(...), as.character)
  as.elo.players.matrix(do.call(cbind, args), weights = weights)
}

as.elo.players.matrix <- function(x, weights = attr(x, "weights"))
{
  if(!is.matrix(x)) stop("'x' isn't a matrix.")
  if(nrow(x)*ncol(x) == 0) stop('No rows or no columns.')
  if(is.null(weights)) weights <- rep(1, ncol(x))
  if(!is.numeric(weights) || length(weights) != ncol(x)) stop("'weights' isn't the right size.")
  weights <- weights / sum(weights)

  structure(x, class = c("elo.players.matrix", class(x)), weights = weights)
}

#' @export
as.matrix.elo.players.matrix <- function(x, ...)
{
  if(!is.matrix(x)) stop("x isn't already a matrix")
  class(x) <- "matrix"
  attr(x, "weights") <- NULL
  x
}

#' @export
`[.elo.players.matrix` <- function(x, i, j, drop = FALSE)
{
  as.elo.players.matrix(as.matrix(x)[i, j, drop = FALSE], attr(x, "weights")[j])
}

#' @export
length.elo.players.matrix <- function(x) nrow(x)

#' @export
weights.elo.players.matrix <- function(object, ...) attr(object, "weights")

is.players <- function(x) inherits(x, "elo.players.matrix")
