
#' @export
"[.adjustedElo" <- function(x, i)
{
  out <- NextMethod()
  attr(out, "adjust") <- if(!missing(i)) attr(x, "adjust")[i] else attr(x, "adjust")
  out
}

remove_adjustedElo <- function(x)
{
  class(x) <- class(x)[!(class(x) %in% "adjustedElo")]
  attr(x, "adjust") <- NULL
  x
}

#' @export
"[.regressElo" <- function(x, i)
{
  out <- NextMethod()
  attr(out, "to") <- attr(x, "to")
  attr(out, "by") <- attr(x, "by")
  attr(out, "regress.unused") <- attr(x, "regress.unused")
  out
}

check_elo_run_vars <- function(mf, initial.elos = NULL)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B

  if(is.numeric(t1)) stop("team.A shouldn't be numeric (team.B can be, though!)")
  all.teams <- t1 <- as.character(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")
  if(!is.numeric(t2))
  {
    t2 <- as.character(t2)
    if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
    all.teams <- c(all.teams, t2)
  }
  flag <- is.numeric(t1) + 2L*is.numeric(t2) # now either 2 or 0

  all.teams <- sort(unique(all.teams))
  initial.elos <- check_initial_elos(initial.elos, all.teams)

  t1 <- as.integer(factor(t1, levels = names(initial.elos))) - 1L
  if(flag != 2) t2 <- as.integer(factor(t2, levels = names(initial.elos))) - 1L

  return(list(wins.A = mf$wins.A, team.A = t1, team.B = t2, k = mf$k,
              adj.A = mf$adj.A, adj.B = mf$adj.B,
              initial.elos = initial.elos, flag = flag))
}

check_initial_elos <- function(init.elos = NULL, teams)
{
  if(is.null(init.elos))
  {
    init.elos <- rep(1500, times = length(teams))
    names(init.elos) <- teams
  }

  if(!is.numeric(init.elos)) stop("Supplied Elos should be numeric.")
  if(is.null(names(init.elos)) || anyDuplicated(names(init.elos)))
    stop("Supplied Elos should have (unique) names!")
  if(any(!(teams %in% names(init.elos))))
    stop("Some teams were found without supplied Elos.")

  return(init.elos[teams])
}

check_group_regress <- function(x, gt.zero = FALSE)
{
  if(anyNA(x)) stop("NAs found in group or regress columns.")
  if(!is.logical(x))
  {
    x <- !duplicated(x, fromLast = TRUE)
  }
  if(gt.zero)
  {
    if(sum(x) == 0) stop("At least one entry in group column must be TRUE.")
  }
  x
}

check_as_matrix <- function(x, group, regr = FALSE)
{
  stopifnot(is.matrix(x$elos), is.numeric(x$elos))
  stopifnot(is.numeric(x$initial.elos))
  stopifnot(length(x$teams) == length(x$initial.elos))

  if(regr)
  {
    stopifnot(is.matrix(x$elos.regressed), is.numeric(x$elos.regressed))
    stopifnot(length(x$teams) == ncol(x$elos.regressed))
  }

  group <- check_group_regress(group, gt.zero = TRUE)
  stopifnot(length(group) == nrow(x$elos))
  invisible(group) # to avoid checking it again later
}

check_final_elos <- function(x, len)
{
  stopifnot(is.matrix(x$elos), is.numeric(x$elos))
  stopifnot(length(x$teams) == max(c(x$elos[, 1], x$elos[, 2])))
}
