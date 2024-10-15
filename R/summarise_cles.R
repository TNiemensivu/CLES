#' Function to calculate common language effect sizes
#'
#'
#' @aliases summarise_cles
#' @param x either a table or a vector of a same length as y
#'
#' @param y if x is supplied as a vector, a vector same length as x
#'
#' @param error.type type of asymptotic standard error to be used in calculation
#' of the p-value. Default is "normal" with other option being "uniform" for
#' the uniform distribution based standard error and hypothesis testing
#'
#' @param conf.level confidence level used for confidence interval, default is 0.05
#'
#' @param alternative type of alternative hypothesis used for calculating p-value.
#' Either "2-sided" (default) or "1-sided"
#'
#' @author Timi Niemensivu \email{timinie@@utu.fi}
#' @author Jari Metsämuuronen \email{jari.metsamuuronen@@gmail.com}
#'
#' @keywords common language effect sizes
#' @export summarise_cles
#' @seealso \code{\link{PHD}} \code{\link{PHG}} \code{\link{cliffs_d}} \code{\link{ordinal_A}}
#' @examples
#'
#' \dontrun{
#'
#' data(PHD_data)
#' summarise_cles(PHD_data$g1, PHD_data$X)
#' }
#'

summarise_cles <- function(x, y=NULL, conf.level = 0.05, error.type = "normal",
                           alternative="2-sided"){
  PHD_val <- PHD(x,y,conf.level, error.type, alternative)
  PHG_val <- PHG(x,y,conf.level, error.type, alternative)
  cliffs_d_val <- cliffs_d(x,y,conf.level, error.type, alternative)
  ordinal_A_val <- ordinal_A(x,y,conf.level, error.type, alternative)

  ci_label <- paste("CI ", (1-conf.level)*100, "%", sep="")

  cat(sprintf("%-12s %-6s %-6s %-6s %-14s \n", " ", "Stat.", "ASE1",
              "ASE0", ci_label))

  cat(sprintf("%-12s %-6.3f %-6.3f %-6.3f %-14s \n", PHD_val@call$rank.cor,
              PHD_val@statistics$rank.cor, PHD_val@significance$ASE1_rc, PHD_val@significance$ASE0_rc,
              paste(round(PHD_val@significance$ci_rc[1], digits=3),
                    round(PHD_val@significance$ci_rc[2], digits=3), sep=" - ")))


  cat(sprintf("%-12s %-6.3f %-6.3f %-6.3f %-14s  \n", PHG_val@call$rank.cor,
              PHG_val@statistics$rank.cor, PHG_val@significance$ASE1_rc, PHG_val@significance$ASE0_rc,
              paste(round(PHG_val@significance$ci_rc[1], digits=3),
                    round(PHG_val@significance$ci_rc[2], digits=3), sep=" - ")))

  cat(sprintf("%-12s %-6.3f %-6.3f %-6.3f %-14s \n", PHD_val@call$cles,
              PHD_val@statistics$cles, PHD_val@significance$ASE1_cles, PHD_val@significance$ASE0_cles,
              paste(round(PHD_val@significance$ci_cles[1], digits=3),
                    round(PHD_val@significance$ci_cles[2], digits=3), sep=" - ")))

  cat(sprintf("%-12s %-6.3f %-6.3f %-6.3f %-14s \n", PHG_val@call$cles,
              PHG_val@statistics$cles, PHG_val@significance$ASE1_cles, PHG_val@significance$ASE0_cles,
              paste(round(PHG_val@significance$ci_cles[1], digits=3),
                    round(PHG_val@significance$ci_cles[2], digits=3), sep=" - ")))

  cat(sprintf("%-12s %-6.3f %-6.3f %-6.3f %-14s \n", cliffs_d_val@call$cles,
              cliffs_d_val@statistics$cles, cliffs_d_val@significance$ASE1_cles, cliffs_d_val@significance$ASE0_cles,
              paste(round(cliffs_d_val@significance$ci_cles[1], digits=3),
                    round(cliffs_d_val@significance$ci_cles[2], digits=3), sep=" - ")))

  cat(sprintf("%-12s %-6.3f %-6.3f %-6.3f %-14s \n", ordinal_A_val@call$cles,
              ordinal_A_val@statistics$cles, ordinal_A_val@significance$ASE1_cles, ordinal_A_val@significance$ASE0_cles,
              paste(round(ordinal_A_val@significance$ci_cles[1], digits=3),
                    round(ordinal_A_val@significance$ci_cles[2], digits=3), sep=" - ")))
}


