# The `elo` Package

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/elo)](https://cran.r-project.org/package=elo)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/elo)](https://cran.r-project.org/package=elo)
[![Downloads](http://cranlogs.r-pkg.org/badges/elo)](https://cran.r-project.org/package=elo)
[![Travis-CI Build Status](https://travis-ci.org/eheinzen/elo.svg?branch=master)](https://travis-ci.org/eheinzen/elo)

The `elo` package includes functions to address all kinds of Elo calculations.

- `elo.prob()`: calculate probabilities based on Elo scores

- `elo.update()`: calculate Elo updates

- `elo.calc()`: calculate post-update Elo values

- `elo.run()` and `elo.run2()`: calculate "running" Elo values for a series of matches

It also includes comparable models for accuracy (auc, MSE) benchmarking:

- `elo.glm()` which fits a logistic regression model

- `elo.markovchain()` which fits a Markov chain model

- `elo.colley()` for a method based on the Colley matrix.

- `elo.winpct()` which fits a model based on win percentage

Please see the vignette for examples.

# Naming Schema

Most functions begin with the prefix "elo.", for easy autocompletion.

- Vectors or scalars of Elo scores are denoted "elo.A" or "elo.B".

- Vectors or scalars of wins by team A are denoted by "wins.A".

- Vectors or scalars of win probabilities are denoted by "p.A".

- Vectors of team names are denoted "team.A" or "team.B".
