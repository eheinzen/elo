
"[.adjustedElo" <- function(x, i)
{
  out <- NextMethod()
  attr(out, "adjust") <- if(!missing(i)) attr(x, "adjust")[i] else attr(x, "adjust")
  out
}

check_elo_run_vars <- function(mf, initial.elo = NULL)
{
  t1 <- mf[[2]]
  t2 <- mf[[3]]
  all.teams <- character(0)
  if(!is.numeric(t1))
  {
    t1 <- as.character(t1)
    all.teams <- c(all.teams, t1)
  }
  if(!is.numeric(t2))
  {
    t2 <- as.character(t2)
    all.teams <- c(all.teams, t2)
  }
  if(anyNA(t1) || anyNA(t2)) stop("The teams shouldn't be NA (if applicable, check they can be coerced to character)")
  flag <- is.numeric(t1) + 2L*is.numeric(t2)

  if(flag == 3L)
  {
    if(!is.null(initial.elo)) warning("Initial Elo specifications being ignored.")
    return(list(wins.A = mf[[1]], team.A = t1, team.B = t2, k = mf[[4]],
                adj.team.A = mf$`(adj1)`, adj.team.B = mf$`(adj2)`,
                initial.elo = 0, flag = flag))
  }

  all.teams <- sort(unique(all.teams))

  if(is.null(initial.elo))
  {
    initial.elo <- rep(1500, times = length(all.teams))
    names(initial.elo) <- all.teams
  }

  if(!is.numeric(initial.elo)) stop("Initial Elo should be numeric.")
  if(length(initial.elo) != length(all.teams)) stop(paste0(length(all.teams), " initial Elos should be specified; ", length(initial.elo), " given."))
  if(is.null(names(initial.elo)) || anyDuplicated(names(initial.elo))) stop("Initial Elo should have (unique) names!")
  if(!setequal(names(initial.elo), all.teams)) stop("names(initial.elo) should equal all teams specified in 'formula'.")

  initial.elo <- initial.elo[all.teams]

  if(flag != 1) t1 <- as.integer(factor(t1, levels = names(initial.elo))) - 1L
  if(flag != 2) t2 <- as.integer(factor(t2, levels = names(initial.elo))) - 1L

  return(list(wins.A = mf[[1]], team.A = t1, team.B = t2, k = mf[[4]],
              adj.team.A = mf$`(adj1)`, adj.team.B = mf$`(adj2)`,
              initial.elo = initial.elo, flag = flag))
}
