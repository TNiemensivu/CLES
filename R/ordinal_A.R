#' Function to calculate ordinal A and ordinal PS effect size.
#' As ordinal A and ordinal PS are equal (see Metsämuuronen 2024), effect size
#' calculated here will be called ordinal A even if it is also ordinal PS.
#'
#'
#' @aliases ordinal_A
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
#' @references
#' Metsämuuronen, Jari. (2024). Two new common language estimators of effect size: Somers' delta and Goodman-Kruskal gamma as bases for nonparametric effect sizes. (Preprint) 10.13140/RG.2.2.14774.31045.
#'
#'
#' @keywords common language effect sizes
#' @export ordinal_A
#' @seealso \code{\link{PHD}} \code{\link{PHG}} \code{\link{ordinal_d}}
#' @examples
#'
#' \dontrun{
#'
#' data(PHD_data)
#' ordinal_A_res <- ordinal_A(PHD_data$g1, PHD_data$X)
#' summary(ordinal_A_res)
#' }



ordinal_A <- function(x, y=NULL, conf.level = 0.05, error.type = "normal",
                     alternative="2-sided"){
  if(!is.null(y)){tab <- table(x,y)}
  else{tab <- x}
  N <- sum(tab)
  D_val <- D(tab)
  PHD_val <- 0.5+0.5*D_val
  ASE1 <- 0.5*D_ASE1(tab)
  if(error.type == "normal"|error.type == "Normal"|error.type == "n"|error.type == "N"){
    ASE0 <- 0.5*D_ASE0(tab)
  }
  else if(error.type == "uniform"|error.type == "Uniform"|error.type == "u"|error.type == "U"){
    ASE0 <- 0.5*D_ASE0_unif(tab)
  }
  else{
    warning("Nonexistent error type, standard error could not be calculated.")
    ASE0  <- 0
  }
  conf <- qt(1-conf.level/2, N-1)
  ordinal_A_val <- 1-PHD_val
  ci_rc <- c(D_val-(2*ASE1/sqrt(N))*conf, D_val+(2*ASE1/sqrt(N))*conf)
  ci_cles <- c(ordinal_A_val-ASE1*conf/sqrt(N), ordinal_A_val+ASE1*conf/sqrt(N))
  Z_val <- (PHD_val-0.5)/ASE0
  p_val <- ifelse(alternative=="1-sided", 1-pnorm(Z_val), 2*(1-pnorm(Z_val)))
  ordinal_A_obj <- new("cles", statistics=list("rank.cor"=D_val, "cles"=ordinal_A_val),
                      significance=list("ASE1_rc"=ASE1*2, "ASE1_cles"=ASE1,
                                        "ASE0_rc"=ASE0*2, "ASE0_cles"=ASE0, "ci_rc"=ci_rc,
                                        "ci_cles"=ci_cles, "Z_stat"=Z_val,
                                        "p_value"=p_val),
                      call = list("rank.cor" = "Somers' D", "cles" = "ordinal A",
                                  "conf.level" = conf.level, "error.type"=error.type,
                                  "alternative"=alternative))
  return(ordinal_A_obj)
}
