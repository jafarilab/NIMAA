#' beatAML
#'
#' The Beat AML program publishes an extensive genomic, drug response, and
#' clinical dataset on acute myeloid leukemia. We selected some data from it.
#'
#' @format A tibble with three variables:
#' \describe{
#' \item{\code{inhibitor}}{names of inhibitors}
#' \item{\code{patient_id}}{anonymous patient ID}
#' \item{\code{median}}{the median of drug response at corresponding \code{inhibitor} and \code{patient_id}}
#' }
#' @source \url{http://vizome.org/aml/}
"beatAML"

#' herbIngredient
#'
#' This data set used the second version of the TCMID database, as the largest
#' dataset in TCM herbs field, which contains richer experimental data
#' originating from ingredient-specific and herbal mass spectrometry spectra.
#'
#'
#' @format A tibble with two variables:
#' \describe{
#' \item{\code{herb}}{names of herbs}
#' \item{\code{pubchem_id}}{unique id of ingredients in pubchemn for \code{herb}}
#' }
#' @source
#' \url{https://www.frontiersin.org/articles/10.3389/fphar.2020.01319/full#h6}
"herbIngredient"

#' drugComb
#'
#' DrugComb collect datasets from several major drug combination studies,
#' covering 437 923 drug combination experiments with 7 423 800 data points
#' across 93 human cancer cell lines.
#'
#' @format A tibble with three variables:
#' \describe{
#' \item{\code{inhibitor}}{names of inhibitors}
#' \item{\code{cell_line}}{cell_line for each \code{inhibitor}}
#' \item{\code{median}}{the median of drug response at corresponding \code{inhibitor} and \code{cell_line}}
#' }
#'
#'   For further details, see
#'   \url{https://academic.oup.com/nar/article-abstract/47/W1/W43/5486743}
#' @source \url{https://www.biorxiv.org/content/10.1101/2021.03.18.436040v3.full}
#'
"drugComb"

#' robertson
#'
#' The author listed 1429 animal species visiting flowers of 456 plant species
#' that grew in a small area in soutWhwestern Illinois, USA. Marlin and LaBerge
#' (2001) describe Robertson’s methods.
#'
#' @format A tibble with two variables:
#' \describe{
#' \item{\code{plant}}{names of plants}
#' \item{\code{pollinator}}{names of pollinators for \code{plant}}
#' }
#' @source \url{http://www.ecologia.ib.usp.br/iwdb/html/robertson_1929.html}
"robertson"
