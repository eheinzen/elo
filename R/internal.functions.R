
"[.adjustedElo" <- function(x, i)
{
  out <- NextMethod()
  attr(out, "adjust") <- if(!missing(i)) attr(x, "adjust")[i] else attr(x, "adjust")
  out
}

check_elo_vars <- function(mf, initial.elo = NULL)
{
  wins.A <- as.numeric(mf[[1]])
  if(!is.numeric(wins.A) || anyNA(wins.A) || !all(0 <= wins.A & wins.A <= 1)) stop("The wins should be between 0 and 1 (inclusive).")
  if(!is.numeric(mf$`(adj1)`) || !is.numeric(mf$`(adj2)`)) stop("Any Elo adjustments should be numeric!")
  if(!is.numeric(mf$`(k)`)) stop("'k' should be numeric.")

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
    return(list(wins.A = wins.A, team.A = t1, team.B = t2, k = mf$`(k)`,
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

  return(list(wins.A = mf[[1]], team.A = t1, team.B = t2, k = mf$`(k)`,
              adj.team.A = mf$`(adj1)`, adj.team.B = mf$`(adj2)`,
              initial.elo = initial.elo, flag = flag))
}


make_tournament_dataset <- function(seed = NULL)
{
  set.seed(seed)

  all.teams <- c("Athletic Armadillos", "Blundering Baboons", "Cunning Cats", "Defense-less Dogs", "Elegant Emus",
                 "Fabulous Frogs", "Gallivanting Gorillas", "Helpless Hyenas")
  true.elo <- c(1800, 1200, 1700, 1300, 1600, 1550, 1400, 1450)
  names(true.elo) <- all.teams

  tournament <- expand.grid(team.Home = all.teams, team.Visitor = all.teams)
  tournament <- tournament[tournament$team.Home != tournament$team.Visitor, ]

  tournament$elo.Home <- true.elo[tournament$team.Home]
  tournament$elo.Visitor <- true.elo[tournament$team.Visitor]

  tournament$adjust.Home <- 200

  tournament$p.Home <- elo.prob(tournament$elo.Home + tournament$adjust.Home, tournament$elo.Visitor)
  tournament$wins.Home <- stats::runif(nrow(tournament)) < tournament$p.Home

  tournament$elo.Diff <- tournament$elo.Home + tournament$adjust.Home - tournament$elo.Visitor

  tournament$points.Home <- stats::rpois(nrow(tournament), lambda = 10)
  tournament$points.Visitor <- tournament$points.Home +
    ifelse(tournament$elo.Diff >= 0 & tournament$wins.Home >  0, pmin(-floor(tournament$elo.Diff / 100), -1), # home team was supposed to win and did
    ifelse(tournament$elo.Diff >= 0 & tournament$wins.Home == 0, pmax(2 - floor(tournament$elo.Diff / 100),  1), # home team was supposed to win but didn't
    ifelse(tournament$elo.Diff <  0 & tournament$wins.Home >  0, pmin(2 + floor(tournament$elo.Diff / 100), -1), # visiting team was supposed to win but didn't
    ifelse(tournament$elo.Diff <  0 & tournament$wins.Home == 0, pmax(-floor(tournament$elo.Diff / 100),  1), NA # visiting team was supposed to win and did
    ))))

  stopifnot(isSymmetric(matrix(table(tournament$wins.Home, tournament$points.Home > tournament$points.Visitor), nrow = 2)))
  tournament$elo.Home <- NULL
  tournament$elo.Visitor <- NULL
  tournament$elo.Diff <- NULL
  tournament$p.Home <- NULL
  tournament$wins.Home <- NULL
  tournament$adjust.Home <- NULL

  rownames(tournament) <- NULL
  attr(tournament, "out.attrs") <- NULL

  tournament
}





