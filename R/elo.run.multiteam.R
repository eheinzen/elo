
#' Calculate running Elos for a series of multi-team matches.
#'
#' @inheritParams elo.run
#' @param formula A one-sided formula with a \code{\link{multiteam}()} object.
#'   See also the \link[=formula.specials]{the help page for formulas} for details.
#' @details
#' This is like \code{\link{elo.run}} (and in fact it runs \code{\link{elo.run}} in the background).
#'   The formula takes a \code{\link{multiteam}()} object, which assumes that teams "win"
#'   in a well-ordered ranking. It assumes that the first place team beats all other teams,
#'   that the second place team loses to the first but beats the others, etc. In that regard,
#'   \code{elo.run.multiteam} reduces to \code{elo.run} when the number of teams (\code{ncol(multiteam())}) is 2
#'
#' However, this is less flexible than \code{elo.run}, because (1) there cannot be ties; (2) it does not accept
#' adjustments; and (3) k is constant within a "game"
#'
#' @examples
#' data(tournament.multiteam)
#' elo.run.multiteam(~ multiteam(Place_1, Place_2, Place_3, Place_4),
#'                   data = tournament.multiteam, subset = -28, k = 20)
#' @export
elo.run.multiteam <- function(formula, data, na.action, subset, k = NULL, initial.elos = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("elos", "k", "group", "regress")
  Call$ncol.k <- 2
  Call$ncol.elos <- 1
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  if(any(mf$adjust.A != 0)) warning("Any adjustments using 'adjust()' are being ignored.")
  mf2 <- multiteam_model_frame(mf)
  er <- elo.run(wins.A ~ elo.A + elo.B + k(k) + group(group) + regress(regress, to = attr(mf$regress, "to"), by = attr(mf$regress, "by")), data = mf2, ...)
  er$Call <- Call
  er$terms <- Terms
  er$na.action <- stats::na.action(mf)
  er$n.matches <- nrow(mf)
  class(er) <- c("elo.run.multiteam", class(er))
  er
}

multiteam_model_frame <- function(mf)
{
  mf$i <- seq_len(nrow(mf))
  mf2 <- lapply(mf$i, function(i) {
    row <- as.character(mf$elo.A[i, ])
    row <- row[!is.na(row)]
    if(length(row) < 2) stop("One or more row of 'multiteam()' has fewer than two (non-NA) teams")
    out <- as.data.frame(t(utils::combn(row, 2)), stringsAsFactors = FALSE)
    names(out) <- c("elo.A", "elo.B")
    out$wins.A <- +(match(out$elo.A, row) < match(out$elo.B, row))
    out$i <- i
    out
  })
  mf2 <- do.call(rbind, c(list(stringsAsFactors = FALSE), mf2))
  mf2 <- merge(mf2, mf[names(mf) != "elo.A"], by = "i", sort = TRUE)
  mf2$group <- !duplicated(mf2$i, fromLast = TRUE) & check_group_regress(mf2$group, gt.zero = TRUE)
  mf2$regress <- !duplicated(mf2$i, fromLast = TRUE) & check_group_regress(mf2$regress)
  mf2
}
