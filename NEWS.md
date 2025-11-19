# discrim (development version)

* Bug fixed where FDA models could fail at prediction time (#79).

# discrim 1.0.2

* Updated document linking to stay on CRAN.

# discrim 1.0.1

* Updated Authors@R.

# discrim 1.0.0

* Case weights were enabled for [discrim_flexible()] and [discrim_linear()] (`"mda"` engine).

# discrim 0.2.0

* Model definition functions (e.g. `discrim_linear()`) were moved to the parsnip package.

# discrim 0.1.3

* An LDA engine was added for the shrunken discriminant analysis method of Ahdesmaki and Strimmer (2010) with `engine = "sda"`. 

* LDA and QDA models now have an engine that fits several regularized discriminant models from the `sparsediscrim` package. 

# discrim 0.1.2

 * Added `discrim_quad()`
 
 * Fixed a bug in the parameter definitions (#19)
 
 * Re-licensed package from GPL-2 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/discrim/issues/22).

# discrim 0.1.1

 * Small updates so that `discrim` can be run in parallel using psock clusters (#13)
 
 * Updates for encoding requirements related to current version of `parsnip`. 

# discrim 0.1.0

 * Small updates to work with new `parsnip` version.

# discrim 0.0.2

* Added `naivebayes` engine for `naive_Bayes()` (#5).

* Change for new `parsnip` version 0.1.0.

# discrim 0.0.1

First CRAN version
