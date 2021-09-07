
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NIMAA <a href='https://github.com/jafarilab/NIMAA'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/NIMAA)](https://cran.r-project.org/package=NIMAA)
[![GitHub
Release](https://img.shields.io/github/release/jafarilab/NIMAA?style=flat)](https://github.com/jafarilab/NIMAA/releases)
[![Github All
Releases](https://img.shields.io/github/downloads/jafarilab/NIMAA/total.svg?style=flat)](https://github.com/jafarilab/NIMAA)
<!-- badges: end -->

The goal of NIMAA is to use bipartite graphs for nominal data mining.

It can select a larger sub-matrix with no missing values in a matrix
containing missing data, and then use the matrix to generate a bipartite
graph and cluster on two projections. In addition, NIMAA can also impute
the missing data, verify and score according to the previous clustering
results obtained from the sub-matrix, and give suggestions on which
imputation method is better.

## Installation

You can install the released version of NIMAA from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("NIMAA")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jafarilab/NIMAA")
```

## Example

### Plot the original data:

``` r
library(NIMAA)
## load the beatAML data
beatAML_data <- NIMAA::beatAML

# plot the original data
beatAML_incidence_matrix <- plotInput(
  x = beatAML_data, # original data with 3 columns
  index_nominal = c(2,1), # the first two columns are nominal data
  index_numeric = 3,  # the third column inumeric data
  print_skim = FALSE, # if you want to check the skim output, set this as TRUE(Default)
  plot_weight = TRUE, # when plotting the figure, show the weights
  )
#> 
#> Na/missing values Proportion:     0.2603
```

<div class="figure" style="text-align: center">

<img src="vignettes/patient_id-inhibitor.png" alt="beatAML dataset as incidence matrix" width="100%" />
<p class="caption">
beatAML dataset as incidence matrix
</p>

</div>

### Plot the bipartite graph of the original data

``` r
graph <- plotBipartite(inc_mat = beatAML_incidence_matrix)
```

<img src="man/figures/README-plotBipartite-1.png" width="100%" style="display: block; margin: auto;" />

### Extract the sub-matrices without missing data

**extractSubMatrix()** will extract the sub-matrices which have no
missing value inside or with specific proportion of missing values
inside (not for elements-max matrix), depends on the user’s input.

``` r
sub_matrices <- extractSubMatrix(
  beatAML_incidence_matrix,
  shape = c("Square", "Rectangular_element_max"), # the shapes you want to extract
  row.vars = "patient_id",
  col.vars = "inhibitor",
  plot_weight = T,
  )
```

<div class="figure" style="text-align: center">

<img src="vignettes/Row_wise_arrangement.png" alt="Row-wise arrangement" width="30%" />
<p class="caption">
Row-wise arrangement
</p>

</div>

<div class="figure" style="text-align: center">

<img src="vignettes/Column_wise_arrangement.png" alt="Column-wise arrangement" width="100%" />
<p class="caption">
Column-wise arrangement
</p>

</div>

### Do clustering based on sub-matrices

**findCluster()** will perform optional pre-processing on the input
incidence matrix, such as normalization. Then use the matrix to perform
bipartite graph projection, and perform optional pre-processing in one
of the specified parts, such as removing edges with lower weights, that
is, weak edges.

``` r
cls <- findCluster(
  sub_matrices$Rectangular_element_max,
  dim = 1,
  method = "all", # clustering mehod
  normalization = TRUE, # normalize the input matrix
  rm_weak_edges = TRUE, # remove the weak edges in graph
  rm_method = 'delete', # removing method is deleting the edges
  threshold = 'median', # edges with weights under the median of all edges' weight are weak edges
  set_remaining_to_1 = TRUE, # set the weights of remaining edges to 1
  )
```

### Impute missing data

The **imputeMissingValue()** function can impute the missing values in
the matrix, we only need to select which methods are needed. The result
will be a list, each element is a matrix with no missing values.

it will perform a variety of numerical imputation according to the
user’s input, and return all the data that does not contain any missing
data, a list of matrices.

-   ‘median’ will replace the missing values with the median of each
    rows (observations)

-   ‘knn’ is the method in package

-   ‘als’ and ‘svd’ are methods from package

-   ‘CA’, ‘PCA’ and ‘FAMD’ are from package

-   others are from the famous package.

``` r
imputations <- imputeMissingValue(
  inc_mat = beatAML_incidence_matrix,
  method = c('svd','median','als','CA')
  )
```

## License

[![GPLv3
License](https://img.shields.io/badge/License-GPL%20v3-yellow.svg)](https://opensource.org/licenses/)
