# elo v2.1.1

* Fixed the "Date" in DESCRIPTION.

# elo v2.1.0

* Added `elo.colley()`, with its corresponding helper functions.

* Allowed `k()` to take two arguments, to give differential updates to "team.A" and "team.B". This has one user-visible effect:
  `as.data.frame.elo.run()` now has one more column than it did before, and its column names have changed. (#45)
  
* Added `elo.run2()`, which allows for custom probabilities and updates, but by default returns the same as `elo.run()` (except more slowly). (#47)

* Added a `pkgdown` site: https://eheinzen.github.io/elo/

# elo v2.0.0

## New functions/functionality:

* Added the `running=TRUE` option to `elo.glm()`. This gives an object of class `"elo.running"`,
  with corresponding methods for `summary()`, `fitted()`, `predict()`, `mse()`, `auc()`, and `favored()`.

* Added `weights=` to `elo.glm()`.

* Added support for `adjust()` in `elo.glm()` to include adjustments in the logistic regression.

* Added a new inline function `neutral()`, to denote neutral field in `elo.glm()` and `elo.markovchain()`.

* Removed the `rm.ties=` argument from `elo.glm()`. Ties will have to be removed instead with `subset=` or before
  running the function altogether.

* Added `elo.markovchain()`, with corresponding methods for `summary()`, `fitted()`, `predict()`, `mse()`, `auc()`, and
  `favored()`. This also has the `running=TRUE` option.

* Added `elo.winpct()`, with corresponding methods for `summary()`, `fitted()`, `predict()`, `mse()`, `auc()`, and
 `favored()`. This also has the `running=TRUE` option.

* Added a function to denote margin of victory, for continuous modeling in `elo.glm()`,
  `elo.markovchain()`, and `elo.winpct()`: `mov()`.

## New helper methods:

* Added `auc.elo.glm()`. (#37)

* Made `favored()` S3 and added `favored.elo.glm()`. (#38)

* Made `mse()` S3 and added `mse.elo.glm()`. (#43)

* Added `summary.elo.glm()`.

* Added `predict.elo.glm()`.

* Added `brier()` as a synonym for `mse()`.

* Added `rank.teams()`.

## Bug fixes:

* Fixed a bug with adding NAs back in to fitted values and residuals with `na.exclude()` in `elo.glm()` and `elo.run()`.
  (#39, #42)

* Fixed a bug with `adjust()` variables not getting subsetted correctly with `na.action` in `model.frame()`. (#40)

* Added `is.na.elo.adjust()` to test for NAs in the adjustment vector. (#41)

# elo v1.1.0

* Widened the version dependency to R 3.3.0.

* Allowed `players()` matrices in `elo.run()` to find Elos of individual players playing at the same time.

* Added `elo.glm()`, a simple function to run logistic regressions on Elo setups.

* Fixed a bug in the `favored()` function (used in `summary.elo.run()`). (#29)

* Exported and revamped the class structure of the specials allowed in formulas. (#30)

* Allowed access to `elo.model.frame()` even when the package isn't loaded. (#34)

* Allowed regression to different values for each team. (#35)

# elo v1.0.1

## Smaller Changes

* Fixed a bug with initial Elos and deep copying in C++. (#25)

* Added an argument to `regress()` allowing users to stop regressing teams which have stopped playing. (#26)

# elo v1.0.0

This version is not backwards compatible!

## Major Changes:

* Changed the signatures of `elo.calc()` and `elo.update()` to match formula interface.

* Changed `elo.calc()`, `elo.update()`, and `elo.prob()` to S3 generics, and implemented
  formula methods. The default methods now include options to adjust Elos. (#3)

* `elo.run()`:

    - `elo.run()` no longer accepts numeric values for `team.A`.

    - `elo.run()` now accepts special functions `group()` and `regress()`. If the latter is used,
      the class of the returned object becomes `"elo.run.regressed"`. (#11, #12, #19, #22)

    - The `$elos` component of `"elo.run"` objects has been completely reworked, and now uses 1-based indexing.
      Because of this, the `print.elo.run()` method also had to be fixed. (#16)

* Renamed `last()` to `final.elos()` (#9).

* Changed `tournament` dataset.

## Smaller Changes:

* The `elo` package now imports `pROC::auc()`.

* `elo.prob()` now accepts vectors of team names (like `elo.run()`) as input. (#6)

* Documentation and the vignette have been updated.

## New Functions:

* Implemented `elo.model.frame()`. The output is a `data.frame` with appropriately named columns.

* Implemented `predict.elo.run()` and `predict.elo.run.regressed()`. (#2, #19)

* Added `is.score()` to test for "score-ness".

* Implemented `summary.elo.run()`, along with helpers to calculate AUC and MSE (`auc()` and `mse()`). (#15)

# elo 0.1.2

* Fixed a spelling error in DESCRIPTION.

# elo 0.1.1

* Made the title more succinct.

* Elaborated the description of the package.

* Tweak the internal `"elo.run"` object.

* Tweaked the README and vignette.

# elo 0.1.0

* Submit first version of `elo` to CRAN.

* Issues and code can be found on GitHub: https://github.com/eheinzen/elo/


