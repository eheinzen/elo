# elo ...

* Implemented `elo.model.frame()`.

* Implemented `predict.elo.run()` (#2).

* Changed the signatures of `elo.calc()` and `elo.update()` to match formula interface.

* Changed `elo.calc()`, `elo.update()`, and `elo.prob()` to S3 generics, and implemented
  formula methods. The default methods now include options to adjust Elos. (#3)
  
* Added `is.score()` to test for "score-ness".

* `elo.run()` no longer accepts numeric values for `team.A`.

* `elo.prob()` now accepts vectors of team names (like `elo.run()`) as input. (#6)

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


