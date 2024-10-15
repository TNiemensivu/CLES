
#------------------------------------------------------------------------------
#' Class "cles"
#'
#' Defines the object returned by different methods in the package
#'
#' @section Slots:
#'
#' \describe{
#'     \item{\code{Call}:}{function call}
#'     \item{\code{Statistics}:}{common language effect size values}
#'     \item{\code{Significance}:{standard errors and t-test value}}
#' }
#' @section Methods:
#'
#' \describe{
#'     \item{summary}{\code{signature(object = "cles")}}
#' }
#'
#' @name cles-class
#' @rdname cles-class
#' @exportClass cles
#' @exportMethod summary
#' @author Timi Niemensivu \email{timinie@@utu.fi}
#' @author Jari Metsämuuronen \email{jari.metsamuuronen@@gmail.com}
#' @keywords classes


setClass("cles", representation(statistics = "list", significance = "list", call="list"))

setMethod(
  "summary",
  "cles",
  function(object) {
    stats <- object@statistics
    signif <- object@significance
    call <- object@call
    rc_confint <- paste(round(signif$ci_rc[1], digits=3)
                       , round(signif$ci_rc[2], digits=3), sep=" - ")
    cles_confint <- paste(round(signif$ci_cles[1], digits=3)
                         , round(signif$ci_cles[2], digits=3), sep=" - ")
    ci_label <- paste("CI ", (1-call$conf.level)*100, "%", sep="")
    cat(sprintf("%-12s %-6s %-6s %-6s %-14s %-7s %-7s \n", " ", "Stat.", "ASE1",
                "ASE0", ci_label, "Z-stat.", paste("p-value (",
                                                   call$alternative, ")", sep="")))
    cat(sprintf("%-12s %-6.3f %-6.3f %-6.3f %-14s %-7.3f %-7.3f \n", call$rank.cor, stats$rank.cor,
                signif$ASE1_rc, signif$ASE0_rc, rc_confint, signif$Z_stat, signif$p_val))
    cat(sprintf("%-12s %-6.3f %-6.3f %-6.3f %-14s %-7.3f %-7.3f \n", call$cles, stats$cles,
                signif$ASE1_cles, signif$ASE0_cles, cles_confint, signif$t_stat, signif$p_val))
    invisible(list(statistics = stats, significance = signif))
  }
)

