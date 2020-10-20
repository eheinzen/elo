
#' @rdname formula.specials
#' @export
players <- function(..., weights = NULL)
{
  args <- lapply(list(...), as.character)
  as.elo.players.matrix(do.call(cbind, args), weights = weights)
}

#' @export
is.na.elo.players.matrix <- function(x) rowSums(is.na(unclass(x))) > 0

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
  if(!missing(j)) return(NextMethod())
  as.elo.players.matrix(as.matrix(x)[i, , drop = FALSE], attr(x, "weights"))
}

#' @export
length.elo.players.matrix <- function(x) nrow(x)

#' @export
weights.elo.players.matrix <- function(object, ...) attr(object, "weights")

is.players <- function(x) inherits(x, "elo.players.matrix")

#' @rdname formula.specials
#' @export
multiteam <- function(...)
{
  args <- lapply(list(...), as.character)
  as.elo.multiteam.matrix(do.call(cbind, args))
}

as.elo.multiteam.matrix <- function(x)
{
  if(!is.matrix(x)) stop("'x' isn't a matrix.")
  if(nrow(x)*ncol(x) == 0) stop('No rows or no columns.')
  structure(x, class = c("elo.multiteam.matrix", class(x)))
}

#' @export
`[.elo.multiteam.matrix` <- function(x, i, j, drop = FALSE)
{
  if(!missing(j)) return(NextMethod())
  as.elo.multiteam.matrix(unclass(x)[i, , drop = FALSE])
}

#' @export
length.elo.multiteam.matrix <- function(x) nrow(x)

#' @export
is.na.elo.multiteam.matrix <- function(x) rowSums(is.na(unclass(x))) == ncol(unclass(x))
