
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

PHG <- function(x, y=NULL, conf.level = 0.05, error.type = "ASE1", 
                alternative="one.sided"){
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
  ci_G <- c(G_val-2*ASE*conf/sqrt(N), G_val+2*ASE*conf/sqrt(N))
  ci_PHG <- c(PHG_val-ASE*conf/sqrt(N), PHG_val+ASE*conf/sqrt(N))
  t_val <- (PHG_val-0.5)/ASE
  p_val <- ifelse(alternative=="one.sided", 1-pnorm(t_val), 2*(1-pnorm(t_val)))
  PHG_obj <- new("PHG", statistics=list("G"=G_val, "PHG"=PHG_val), 
                 significance=list("ASE_G"=ASE*2, "ASE_PHG"=ASE, "ci_G"=ci_G, 
                                   "ci_PHG"=ci_PHG, "t_stat"=t_val,
                                   "p_value"=p_val), 
                 call = list("conf.level"=conf.level, "error.type"=error.type,
                             "alternative"=alternative))
}


