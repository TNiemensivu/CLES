
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

setMethod(
  "summary",
  "PHD",
  function(object) {
    stats <- object@statistics
    signif <- object@significance
    D_confint <- paste(round(signif$ci_D[1], digits=3)
                       , round(signif$ci_D[2], digits=3), sep=" - ")
    PHD_confint <- paste(round(signif$ci_PHD[1], digits=3)
                         , round(signif$ci_PHD[2], digits=3), sep=" - ")
    ci_label <- paste("CI ", (1-object@call$conf.level)*100, "%", sep="")
    cat(sprintf("%-5s %-6s %-6s %-6s %-14s %-7s %-7s \n", " ", "Stat.", "ASE1",
                "ASE0", ci_label, "t-stat.", "p-value"))
    cat(sprintf("%-5s %-6.3f %-6.3f %-6.3f %-14s %-7.3f %-7.3f \n", "D", stats$D,
                signif$ASE1_D, signif$ASE0_D, D_confint, signif$t_stat, signif$p_val))
    cat(sprintf("%-5s %-6.3f %-6.3f %-6.3f %-14s %-7.3f %-7.3f \n", "PHD", stats$PHD,
                signif$ASE1_PHD, signif$ASE0_PHD, PHD_confint, signif$t_stat, signif$p_val))
    invisible(list(statistics = stats, significance = signif))
  }
)



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

setMethod(
  "summary",
  "PHG",
  function(object) {
    stats <- object@statistics
    signif <- object@significance
    G_confint <- paste(round(signif$ci_G[1], digits=3)
                       , round(signif$ci_G[2], digits=3), sep=" - ")
    PHG_confint <- paste(round(signif$ci_PHG[1], digits=3)
                         , round(signif$ci_PHG[2], digits=3), sep=" - ")
    ci_label <- paste("CI ", (1-object@call$conf.level)*100, "%", sep="")
    cat(sprintf("%-5s %-6s %-6s %-14s %-7s %-7s \n", " ", "Stat.", "ASE", ci_label,
                "t-stat.", "p-value"))
    cat(sprintf("%-5s %-6.3f %-6.3f %-14s %-7.3f %-7.3f \n", "G", stats$G,
                signif$ASE_G, G_confint, signif$t_stat, signif$p_val))
    cat(sprintf("%-5s %-6.3f %-6.3f %-14s %-7.3f %-7.3f \n", "PHG", stats$PHG,
                signif$ASE_PHG, PHG_confint, signif$t_stat, signif$p_val))
    invisible(list(statistics = stats, significance = signif))
  }
)
