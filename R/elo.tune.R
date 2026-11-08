#' Select a constant K using designated evaluation matches
#'
#' Replay all matches with each candidate K and minimize mean squared error on
#' explicitly selected evaluation rows. Unscored matches still update ratings.
#'
#' @inheritParams elo.run
#' @param data A nonempty \code{data.frame}, in match order.
#' @param k A nonempty numeric vector of distinct, finite, nonnegative candidate
#'   K values. Zero is allowed as a no-update baseline for the default update rule.
#' @param evaluate A logical vector of length \code{nrow(data)}, with no missing
#'   values and at least one \code{TRUE}, identifying the matches to score.
#' @param ... Named arguments passed to \code{\link{elo.run}}:
#'   \code{initial.elos}, \code{prob.fun}, \code{update.fun}, and \code{verbose}.
#' @details
#' Each candidate starts from the same initial ratings and replays every supplied
#' match. Only the scoring step uses \code{evaluate}, through
#' \code{mse(fit, subset = evaluate)}. Draws retain their score of 0.5. Formula
#' specials such as \code{group()}, \code{regress()}, and \code{adjust()} keep
#' their usual behavior; evaluation rows need not comprise complete groups.
#'
#' Supply matches in chronological order, with grouping appropriate to when
#' results become available. This function does not check dates or create
#' cross-validation splits. The minimum is a selection score, not an independent
#' estimate of predictive performance. Use a later test period or an outer
#' rolling evaluation to assess the selected procedure.
#'
#' To keep evaluation rows aligned with predictions, \code{subset} is not
#' supported and \code{na.action} is fixed at \code{na.fail}. Missing values in
#' model variables cause an error, including on unscored rows. The formula must
#' not contain a \code{k()} term, which would override the candidate grid.
#' Candidate values are processed in ascending order; exact loss ties select
#' the smallest K. Non-finite candidate losses cause an error.
#' @return A list containing:
#' \describe{
#'   \item{scores}{A data frame with columns \code{k}, \code{n} (the number of
#'     scored matches), and \code{mse}, sorted by K.}
#'   \item{best.k}{The K with the smallest evaluation MSE.}
#'   \item{fit}{The selected \code{elo.run} fit. It includes all supplied rows,
#'     including any unscored matches after the evaluation period; it is not
#'     necessarily the rating state at the end of that period.}
#' }
#' @examples
#' data(tournament)
#' selection <- elo.tune(
#'   score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, k = c(0, 10, 20, 40),
#'   evaluate = tournament$week > 4
#' )
#' selection$scores
#' selection$best.k
#' final.elos(selection$fit)
#' @seealso \code{\link{elo.run}}, \code{\link{mse}}
#' @export
elo.tune <- function(formula, data, k, evaluate, ...)
{
  if(!is.data.frame(data) || nrow(data) == 0) stop("'data' must be a nonempty data frame")
  if(!is.numeric(k) || is.complex(k) || !is.null(dim(k)) || length(k) == 0 ||
     anyNA(k) || any(!is.finite(k)) || any(k < 0) || anyDuplicated(k))
    stop("'k' must contain distinct, finite, nonnegative numeric candidates")
  if(!is.logical(evaluate) || !is.null(dim(evaluate)) || length(evaluate) != nrow(data) ||
     anyNA(evaluate) || !any(evaluate))
    stop("'evaluate' must be a logical vector with one value per row, no NAs, and at least one TRUE")
  args <- list(...)
  if("subset" %in% names(args)) stop("'subset' is not supported; use 'evaluate' to select scoring rows")
  if("na.action" %in% names(args)) stop("'na.action' is fixed at na.fail")
  allowed <- c("initial.elos", "prob.fun", "update.fun", "verbose")
  if(length(args) && (is.null(names(args)) || anyDuplicated(names(args)) ||
                     any(!names(args) %in% allowed)))
    stop("'...' only accepts named arguments: ", paste(allowed, collapse = ", "))
  Terms <- stats::terms(formula, specials = "k", data = data)
  if(length(attr(Terms, "specials")$k)) stop("'formula' must not contain a k() term")

  k <- sort(unname(k))
  losses <- numeric(length(k))
  best.loss <- Inf
  best <- NULL
  for(i in seq_along(k))
  {
    fit <- do.call(elo.run, c(list(formula = formula, data = data, k = k[i],
                                 na.action = stats::na.fail), args))
    losses[i] <- mse(fit, subset = evaluate)
    if(!is.finite(losses[i])) stop("Non-finite evaluation MSE for K = ", k[i])
    if(losses[i] < best.loss)
    {
      best.loss <- losses[i]
      best <- fit
    }
  }
  list(scores = data.frame(k = k, n = sum(evaluate), mse = losses),
       best.k = k[which.min(losses)], fit = best)
}
