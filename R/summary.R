
#' Summarize an \code{elo} Object
#'
#' @param object An object to summarize.
#' @param ... Other arguments
#' @return A summary of \code{object}.
#' @examples
#' summary(elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, k = 20))
#' summary(elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament))
#' mc <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, subset = points.Home != points.Visitor, k = 0.7)
#' summary(mc)
#' co <- elo.colley(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, subset = points.Home != points.Visitor)
#' summary(co)
#' wp <- elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, subset = points.Home != points.Visitor, k = 0.7)
#' summary(wp)
#' @seealso \code{\link{favored}}, \code{\link{auc.elo.run}}, \code{\link{mse}}
#' @name summary.elo
NULL
#> NULL

#' @rdname summary.elo
#' @export
summary.elo.run <- function(object, ...)
{
  object$favored <- favored(object)
  object$mse <- mse(object)
  object$auc <- auc.elo.run(object)
  class(object) <- c("summary.elo.run", class(object))
  object
}

#' @export
print.summary.elo.run <- function(x, ...)
{
  NextMethod()
  cat("Mean Square Error: ", round(x$mse, 4), "\n",
      "AUC: ", round(x$auc, 4), "\n",
      "Favored Teams vs. Actual Wins: \n",
      sep = "")
  print(x$favored)

  invisible(x)
}

#' @rdname summary.elo
#' @export
summary.elo.glm <- function(object, ...)
{
  out <- NextMethod()
  out$favored <- favored(object, ...)
  out$mse <- mse(object, ...)
  out$auc <- if(object$outcome == "mov") NA_real_ else auc.elo.glm(object, ...)
  class(out) <- c("summary.elo.glm", class(out))
  out
}

#' @export
print.summary.elo.glm <- function(x, ...)
{
  NextMethod()
  cat("Mean Square Error: ", round(x$mse, 4), "\n",
      "AUC: ", round(x$auc, 4), "\n",
      "Favored Teams vs. Actual Wins: \n",
      sep = "")
  print(x$favored)

  invisible(x)
}

#' @rdname summary.elo
#' @export
summary.elo.markovchain <- function(object, ...)
{
  object$favored <- favored(object, ...)
  object$mse <- mse(object, ...)
  object$auc <- if(object$outcome == "mov") NA_real_ else auc.elo.markovchain(object, ...)
  class(object) <- c("summary.elo.markovchain", class(object))
  object
}

#' @export
print.summary.elo.markovchain <- function(x, ...)
{
  NextMethod()
  cat("Mean Square Error: ", round(x$mse, 4), "\n",
      "AUC: ", round(x$auc, 4), "\n",
      "Favored Teams vs. Actual Wins: \n",
      sep = "")
  print(x$favored)

  invisible(x)
}

#' @rdname summary.elo
#' @export
summary.elo.colley <- function(object, ...)
{
  object$favored <- favored(object, ...)
  object$mse <- mse(object, ...)
  object$auc <- if(object$outcome == "mov") NA_real_ else auc.elo.colley(object, ...)
  class(object) <- c("summary.elo.colley", class(object))
  object
}

#' @export
print.summary.elo.colley <- function(x, ...)
{
  NextMethod()
  cat("Mean Square Error: ", round(x$mse, 4), "\n",
      "AUC: ", round(x$auc, 4), "\n",
      "Favored Teams vs. Actual Wins: \n",
      sep = "")
  print(x$favored)

  invisible(x)
}

#' @rdname summary.elo
#' @export
summary.elo.winpct <- function(object, ...)
{
  object$favored <- favored(object, ...)
  object$mse <- mse(object, ...)
  object$auc <- if(object$outcome == "mov") NA_real_ else auc.elo.winpct(object, ...)
  class(object) <- c("summary.elo.winpct", class(object))
  object
}

#' @export
print.summary.elo.winpct <- print.summary.elo.markovchain
