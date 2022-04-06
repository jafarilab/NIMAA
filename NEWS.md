# NIMAA 0.2.1

## New verbs

* `plotIncMatrix` is the new name of the function previously known as `plotInput`.

* `nominalAsBinet` is the new name of the function previously known as `el2IncMatrix`.

## Minor improvments

* The `readme` and `vignette` files are now updated to the latest modifications.

* The manual help page of the `nominalAsBinet` is modified to better describe the function.

* A few typographical and writing problems have been corrected throughout all of the related documents.

# NIMAA 0.2.0

* Added a `NEWS.md` file to track changes to the package.

## New verbs

* `predictEdge` makes it possible to predict edges for the pairwise relationship of labels of nominal variables by imputing weight scores. This function was substituted by the deprecated `imputeMissingValue` function. 

* `validateEdgePrediction` makes it possible to validate the predicted edges for the pairwise relationship of labels of nominal variables by imputing weight scores. This function was substituted by the deprecated `validateImputation` function.

## Minor improvments

* The argument `dim` for `findCluster` function was deprecated and it has been substituted by `part`.

* The argument `dim` for `plotBipartite` function was deprecated and it has been substituted by `part`.

* The `beatAML` dataset description and reference have been updated.

* The `herbIngredient` dataset description and reference have been updated.

* The `drugComb` dataset description and reference have been updated.

* The `robertson` dataset description and reference have been updated.

* The example section of `analyseNetwork` function was expanded.

* The example section of `el2IncMatrix` function was expanded.

* The `readme` file is now updated to the latest modifications.

## New vignette

* The current vignette describes the concept of **edge prediction** based on new functions and arguments. In other words, validation of network clusters using ground truth and validation of edge prediction based on the benchmark is now clearer to utilize in the nominal data mining analysis pipeline.

## Bug fixes

* `findCluster` example provides the output of comparison.

* The default of `print_skim` argument set to `FALSE` in all related functions. 
