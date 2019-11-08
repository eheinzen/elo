
check_elo_run_vars <- function(mf, initial.elos = NULL)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B

  if(is.numeric(t1)) stop("team.A shouldn't be numeric (team.B can be, though!)")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")
  all.teams <- as.character(t1)
  wts1 <- weights(t1)

  flag <- 2L*is.numeric(t2) # now either 2 or 0
  if(!is.numeric(t2))
  {
    if(!is.players(t2)) t2 <- players(t2)
    if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
    all.teams <- c(all.teams, as.character(t2))
    wts2 <- weights(t2)
  } else
  {
    t2 <- matrix(t2, ncol = 1)
    wts2 <- 1
  }

  all.teams <- sort(unique(all.teams))
  initial.elos <- check_named_elos(initial.elos, all.teams)

  regress <- check_group_regress(mf$regress)
  to <- check_named_elos(attr(mf$regress, "to"), all.teams)

  tmp <- stats::setNames(seq_along(initial.elos) - 1L, names(initial.elos))
  t1 <- matrix(tmp[t1], nrow = nrow(t1))
  if(flag != 2) t2 <- matrix(tmp[t2], nrow = nrow(t2))

  list(winsA = mf$wins.A, teamA = t1, teamB = t2, weightsA = wts1, weightsB = wts2,
       k = mf$k, adjTeamA = mf$adj.A, adjTeamB = mf$adj.B, regress = regress,
       to = to, by = attr(mf$regress, "by"),
       regressUnused = attr(mf$regress, "regress.unused"),
       initialElos = initial.elos, flag = flag)
}

check_named_elos <- function(init.elos = NULL, teams)
{
  sing <- length(init.elos) == 1 && is.null(names(init.elos))
  if(is.null(init.elos) || sing)
  {
    init.elos <- rep(if(sing) init.elos else 1500, times = length(teams))
    names(init.elos) <- teams
  }

  if(!is.numeric(init.elos)) stop("Supplied Elos should be numeric.")
  if(is.null(names(init.elos)) || anyDuplicated(names(init.elos)))
    stop("Supplied Elos should have (unique) names!")
  if(any(!(teams %in% names(init.elos))))
    stop("Some teams were found without supplied Elos.")

  init.elos[teams]
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
  stopifnot(length(x$teams) == max(x$elos[, 1:sum(x$n.players)]))
}

null_or_length0 <- function(x) is.null(x) || length(x) == 0

clean_elo_formula <- function(Terms, drop.neutral = TRUE)
{
  k.col <- attr(Terms, "specials")$k
  grp.col <- attr(Terms, "specials")$group
  reg.col <- attr(Terms, "specials")$regress
  neu.col <- attr(Terms, "specials")$neutral

  if(!null_or_length0(cols <- c(k.col, grp.col, reg.col, if(drop.neutral) neu.col)))
  {
    Terms <- stats::drop.terms(Terms, dropx = cols - 1, keep.response = TRUE)
  }
  stats::formula(stats::delete.response(Terms))
}



mf_to_wide <- function(mf, teams = NULL)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B

  if(is.numeric(t1) || is.numeric(t2)) stop("Neither team should be numeric")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")

  if(!is.players(t2)) t2 <- players(t2)
  if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
  all.teams <- sort(unique(c(as.character(t1), as.character(t2))))
  if(!is.null(teams))
  {
    if(!all(all.teams %in% teams)) stop("Unknown teams: ", paste0(unique(all.teams[!(all.teams %in% teams)]), collapse = ", "))
    all.teams <- teams
  }

  dat <- lapply(all.teams, function(tm) (rowSums(t1 == tm) > 0) - (rowSums(t2 == tm) > 0))
  names(dat) <- all.teams
  dat$home.field <- mf$home.field
  dat <- dat[c("home.field", all.teams)] # rearrange to put home field first
  dat$adj.A <- mf$adj.A
  dat$adj.B <- mf$adj.B
  structure(dat, class = "data.frame", row.names = c(NA_integer_, nrow(mf)), all.teams = all.teams)
}

check_elo_markovchain_vars <- function(mf)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B

  if(is.numeric(t1) || is.numeric(t2)) stop("Neither team should be numeric")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")

  if(!is.players(t2)) t2 <- players(t2)
  if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
  all.teams <- sort(unique(c(as.character(t1), as.character(t2))))

  tmp <- stats::setNames(seq_along(all.teams) - 1L, all.teams)
  wts1 <- weights(t1)
  wts2 <- weights(t2)
  t1 <- matrix(tmp[t1], nrow = nrow(t1))
  t2 <- matrix(tmp[t2], nrow = nrow(t2))

  if(!all(mf$weights > 0)) stop("Weights should be positive numbers")

  if(!all(0 <= mf$k & mf$k <= 1)) stop("'k' should be between 0 and 1 (inclusive)")
  winsA <- if(attr(mf, "outcome") == "mov") score(mf$wins.A, 0) else mf$wins.A
  structure(list(winsA = winsA, k = mf$k, weights = mf$weights, teamA = t1, teamB = t2,
                 weightsA = wts1, weightsB = wts2, nTeams = length(all.teams)), teams = all.teams)
}

group_to_int <- function(grp, skip)
{
  grp2 <- check_group_regress(grp, gt.zero = FALSE)
  grp2 <- rev(cumsum(rev(grp2)))
  mx <- max(grp2)
  if(skip > mx || skip < 0) stop("skip must be between 0 and ", mx, " (inclusive)")
  mx + 1 - grp2 # from mx : 1 to 1 : mx
}



check_elo_winpct_vars <- function(mf)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B

  if(is.numeric(t1) || is.numeric(t2)) stop("Neither team should be numeric")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")

  if(!is.players(t2)) t2 <- players(t2)
  if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
  all.teams <- sort(unique(c(as.character(t1), as.character(t2))))

  tmp <- stats::setNames(seq_along(all.teams) - 1L, all.teams)
  wts1 <- weights(t1)
  wts2 <- weights(t2)
  t1 <- matrix(tmp[t1], nrow = nrow(t1))
  t2 <- matrix(tmp[t2], nrow = nrow(t2))

  if(!all(mf$weights > 0)) stop("Weights should be positive numbers")

  winsA <- if(attr(mf, "outcome") == "mov") score(mf$wins.A, 0) else mf$wins.A
  out <- list(winsA = winsA, weights = mf$weights, teamA = t1, teamB = t2,
              weightsA = wts1, weightsB = wts2, nTeams = length(all.teams))
  attr(out, "teams") <- all.teams
  out
}

mean_vec_subset_matrix <- function(vec, mat)
{
  rowMeans(matrix(vec[mat], nrow = nrow(mat)))
}
