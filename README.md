
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/elo)](https://cran.r-project.org/package=elo)
[![Downloads](http://cranlogs.r-pkg.org/badges/elo)](https://cran.r-project.org/package=elo)
[![Travis-CI Build Status](https://travis-ci.org/eheinzen/elo.svg?branch=master)](https://travis-ci.org/eheinzen/elo)


# The `elo` Package

The `elo` package includes functions to address all kinds of Elo calculations.

```{r}
library(elo)
```

Please see the vignette for examples.

# Naming Schema

Most functions begin with the prefix "elo.", for easy autocompletion.

- Vectors or scalars of Elo scores are denoted `elo.A` or `elo.B`.

- Vectors or scalars of wins by team A are denoted by `wins.A`.

- Vectors or scalars of win probabilities are denoted by `p.A`.

- Vectors of team names are denoted `team.A` or `team.B`.
