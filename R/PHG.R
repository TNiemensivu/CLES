#' Function to calculate PHG effect size
#'
#'
#' @aliases PHG
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
#' @export PHG
#' @seealso \code{\link{PHD}}
#' @examples
#'
#' \dontrun{
#'
#' data(PHD_data)
#' PHD_res <- PHD(PHD_data$g1, PHD_data$X)
#' summary(PHD)
#' }


PHG <- function(x, y=NULL, conf.level = 0.05, error.type = "normal",
                alternative="two.sided"){
  if(!is.null(y)){tab <- table(x,y)}
  else{tab <- x}
  N <- sum(tab)
  G_val <- G(tab)
  PHG_val <- 0.5+0.5*G_val
  ASE1 <- 0.5*G_ASE1(tab)
  if(error.type == "normal"|error.type == "Normal"|error.type == "n"|error.type == "N"){
    ASE0 <- 0.5*G_ASE0(tab)
  }
  else if(error.type == "uniform"|error.type == "Uniform"|error.type == "u"|error.type == "U"){
    ASE0 <- 0.5*G_ASE0_unif(tab)
  }
  else{
    warning("Nonexistent error type, standard error could not be calculated.")
    ASE <- 0
  }
  conf <- qt(1-conf.level/2, N-1)
  ci_G <- c(G_val-2*ASE1*conf/sqrt(N), G_val+2*ASE1*conf/sqrt(N))
  ci_PHG <- c(PHG_val-ASE1*conf/sqrt(N), PHG_val+ASE1*conf/sqrt(N))
  t_val <- (PHG_val-0.5)/ASE0
  p_val <- ifelse(alternative=="one.sided", 1-pnorm(t_val), 2*(1-pnorm(t_val)))
  PHG_obj <- new("PHG", statistics=list("G"=G_val, "PHG"=PHG_val),
                 significance=list("ASE1_G"=ASE1*2, "ASE0_D" = ASE0*2,
                                   "ASE1_PHG"=ASE1, "ASE0_PHG"=ASE0, "ci_G"=ci_G,
                                   "ci_PHG"=ci_PHG, "t_stat"=t_val,
                                   "p_value"=p_val),
                 call = list("conf.level"=conf.level, "error.type"=error.type,
                             "alternative"=alternative))
  return(PHG_obj)
}


