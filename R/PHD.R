#' Function to calculate PHD effect size
#'
#'
#' @aliases PHD
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
#' @author Timi Niemensivu \email{timinie@@utu.fi}
#' @author Jari Metsämuuronen \email{jari.metsamuuronen@@gmail.com}
#' 
#' @keywords common language effect sizes
#' @export PHD
#' @seealso \code{\link{PHG}}
#' @examples
#'
#' \dontrun{
#'
#' dat <- expand.table(g_data)
#' PHD_res <- PHD(dat$g1, dat$X)
#' summary(PHD)
#' }



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

PHD <- function(x, y=NULL, conf.level = 0.05, error.type = "normal", 
                alternative="one.sided"){
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
  ci_D <- c(D_val-(2*ASE1/sqrt(N))*conf, D_val+(2*ASE1/sqrt(N))*conf)
  ci_PHD <- c(PHD_val-ASE1*conf/sqrt(N), PHD_val+ASE1*conf/sqrt(N))
  t_val <- (PHD_val-0.5)/ASE0
  p_val <- ifelse(alternative=="one.sided", 1-pnorm(t_val), 2*(1-pnorm(t_val)))
  PHD_obj <- new("PHD", statistics=list("D"=D_val, "PHD"=PHD_val), 
                 significance=list("ASE1_D"=ASE1*2, "ASE1_PHD"=ASE1, 
                                   "ASE0_D"=ASE0*2, "ASE0_PHD"=ASE0, "ci_D"=ci_D, 
                                   "ci_PHD"=ci_PHD, "t_stat"=t_val,
                                   "p_value"=p_val), 
                 call = list("conf.level"=conf.level, "error.type"=error.type,
                             "alternative"=alternative))
}

