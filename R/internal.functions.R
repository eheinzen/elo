
check_elo_run_vars <- function(mf, initial.elos = NULL)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B

  if(is.numeric(t1)) stop("team.A shouldn't be numeric (team.B can be, though!)")
  if(!inherits(t1, "elo.players.matrix"))
  {
    t1 <- players(t1)
  }
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")
  all.teams <- as.character(t1)
  wts1 <- weights(t1)

  flag <- 2L*is.numeric(t2) # now either 2 or 0
  if(!is.numeric(t2))
  {
    if(!inherits(t2, "elo.players.matrix")) t2 <- players(t2)
    if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
    all.teams <- c(all.teams, as.character(t2))
    wts2 <- weights(t2)
  } else
  {
    t2 <- matrix(t2, ncol = 1)
    wts2 <- 1
  }

  all.teams <- sort(unique(all.teams))
  initial.elos <- check_initial_elos(initial.elos, all.teams)

  make_int <- function(x) as.integer(factor(x, levels = names(initial.elos))) - 1L

  t1 <- apply(t1, 2, make_int)
  if(flag != 2) t2 <- apply(t2, 2, make_int)

  return(list(wins.A = mf$wins.A, team.A = t1, team.B = t2, wts.A = wts1, wts.B = wts2, k = mf$k,
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

null_or_length0 <- function(x) is.null(x) || length(x) == 0

clean_elo_formula <- function(Terms)
{
  k.col <- attr(Terms, "specials")$k
  grp.col <- attr(Terms, "specials")$group
  reg.col <- attr(Terms, "specials")$regress

  if(!null_or_length0(cols <- c(k.col, grp.col, reg.col)))
  {
    Terms <- stats::drop.terms(Terms, dropx = cols - 1, keep.response = TRUE)
  }
  stats::formula(stats::delete.response(Terms))
}
