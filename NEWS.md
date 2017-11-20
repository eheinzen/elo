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


