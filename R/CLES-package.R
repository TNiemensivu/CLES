#' Common Language Effect Sizes
#'
#'
#'
#'
#' @name CLES-package
#' @title Common Language Effect Size
#' @author Timi Niemensivu \email{timinie@@utu.fi}
#' @author Jari Metsämuuronen \email{jari.metsamuuronen@@gmail.com}
#' @keywords package

NULL



#' Description of PHD example data
#'
#'Hypothetical dataset used in Metsämuuronen 2024 to demonstrate properties of PHD and PHG.
#'
#'
#' @name PHD_data
#' @docType data
#' @author Jari Metsämuuronen \email{jari.metsamuuronen@@gmail.com}
#' @references
#' Metsämuuronen, Jari. (2024). Two new common language estimators of effect size: Somers' delta and Goodman-Kruskal gamma as bases for nonparametric effect sizes. (Preprint) 10.13140/RG.2.2.14774.31045.
#'
#' @keywords data
#' @examples
#'
#'\dontrun{
#'
#' data(PHD_data)
#' PHD_res <- PHD(PHD_data$g1, PHD_data$X)
#' summary(PHD)
#' }

NULL
