
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


prep_elo_formula <- function(formula, data, na.action, subset, ..., envir)
{
  Call <- match.call()

  indx <- match(c("formula", "data", "subset", "na.action"), names(Call), nomatch = 0)
  if(indx[1] == 0) stop("A formula argument is required.")

  temp.call <- Call[c(1, indx)]
  temp.call[[1L]] <- quote(stats::model.frame)
  specials <- c("adjust", "k")

  temp.call$formula <- if(missing(data))
  {
    stats::terms(formula, specials)
  } else stats::terms(formula, specials, data = data)

  adjenv <- new.env(parent = environment(formula))
  if(!is.null(attr(temp.call$formula, "specials")$adjust))
  {
    assign("adjust", function(x, y) {
      if(length(y) == 1)
      {
        attr(x, "adjust") <- rep(y, times = length(x))
      } else if(length(y) == length(x))
      {
        attr(x, "adjust") <- y
      } else stop("The second argument to 'adjust' needs to be length 1 or the same length as the first argument.")

      class(x) <- c("adjustedElo", class(x))
      x
    }, envir = adjenv)
  }
  if(!is.null(attr(temp.call$formula, "specials")$k))
  {
    assign("k", function(x) x, envir = adjenv)
  }
  environment(temp.call$formula) <- adjenv


  mf <- eval(temp.call, envir)

  if(nrow(mf) == 0) stop("No (non-missing) observations")
  return(mf)
}



