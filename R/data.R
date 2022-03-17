#' beatAML
#'
#' The Beat AML program produced a comprehensive dataset on acute myeloid leukemia (AML) that included genomic data (i.e., whole-exome sequencing and RNA sequencing), ex vivo drug response, and clinical data. We have included only the drug response data from this program in this package.
#'
#' @format A tibble with three variables:
#' \describe{
#' \item{\code{inhibitor}}{names of inhibitors}
#' \item{\code{patient_id}}{anonymous patient ID}
#' \item{\code{median}}{the median of drug response at corresponding \code{inhibitor} and \code{patient_id}}
#' }
#' @source Tyner, Jeffrey W., et al.(2018), Functional Genomic Landscape of Acute Myeloid Leukaemia. Nature 562 (7728): 526–31. \url{http://vizome.org/aml/}
"beatAML"

#' herbIngredient
#'
#' The second version of the TCMID database, which included additional ingredient-specific experimental data based on herbal mass spectrometry spectra, was used as the largest dataset in the TCM herb sector. The herb and ingredient associations from this database are provided here.
#'
#'
#' @format A tibble with two variables:
#' \describe{
#' \item{\code{herb}}{Scientific Latin names of herbs avialbale in TCMID database}
#' \item{\code{pubchem_id}}{unique compound id (CID) of ingredients in pubchem database for available \code{herb} in TCMID database}
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
