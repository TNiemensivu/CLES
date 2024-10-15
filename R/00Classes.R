
#------------------------------------------------------------------------------
#' Class "PHD"
#'
#' Defines the object returned from \code{\link{PHD}}.
#'
#' @section Slots:
#'
#' \describe{
#'     \item{\code{Call}:}{function call }
#'     \item{\code{Statistics}:}{D and PHD values}
#'     \item{\code{Significance}:{standard errors and t-test value}}
#' }
#' @section Methods:
#'
#' \describe{
#'     \item{summary}{\code{signature(object = "PHD")}}
#' }
#'
#' @name PHD-class
#' @rdname PHD-class
#' @exportClass PHD
#' @author Timi Niemensivu \email{timinie@@utu.fi}
#' @author Jari Metsämuuronen \email{jari.metsamuuronen@@gmail.com}
#' @keywords classes


setClass("PHD", representation(statistics = "list", significance = "list", call="list"))




#------------------------------------------------------------------------------
#' Class "PHG"
#'
#' Defines the object returned from \code{\link{PHG}}.
#'
#' @section Slots:
#'
#' \describe{
#'     \item{\code{Call}:}{function call }
#'     \item{\code{Statistics}:}{G and PHG values}
#'     \item{\code{Significance}:{standard errors and t-test value}}
#' }
#' @section Methods:
#'
#' \describe{
#'     \item{summary}{\code{signature(object = "PHD")}}
#' }
#'
#' @name PHG-class
#' @rdname PHG-class
#' @exportClass PHG
#' @author Timi Niemensivu \email{timinie@@utu.fi}
#' @author Jari Metsämuuronen \email{jari.metsamuuronen@@gmail.com}
#' @keywords classes


setClass("PHG", representation(statistics = "list", significance = "list", call="list"))


