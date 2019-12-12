#' Print the NBAYES object
#'
#' @param objet The NBAYES object returned by the fit function
#'
#' @details Print some attributs of the NBAYES objects:
#' @details Discretisation method used for the fit (disc.param)
#' @details Varriable to classify (Y)
#' @details Variables used by the model - only variables kept after discretisation and selection (X)
#' @details Prior probabilities computed (apriori)
#' @details COnditional probabilities (p_conditional)
#' @export
print.NBAYES = function(objet){
  cat("NBAYES object (from naivebayeserc)\n\n")
  cat("Discretisation method used:\n",objet$disc.param,"\n\n")
  cat("Variables to classify:\n",objet$Y,"\n\n")
  cat("Kept variables :\n",objet$X,"\n\n")
  cat("Computed prior probabilities:\n\n")
  print(objet$apriori)
  cat("\n")
  cat("Computed conditional probabilities :\n\n")
  print( head(objet$p_conditional) )
}

#' Summary from NBAYES object
#'
#' @param objet NBAYES object
#'
#' @description Print the significance of the link between each variable X and Y (signif)
#' @export
summary.NBAYES = function(objet){
  cat("NBAYES object (from naivebayeserc)\n\n")
  cat("Variables to classify:\n",objet$Y,"\n\n")
  cat("Significance of the link between", objet$Y, "and each variable:\n\n")
  print(objet$signif)
}

#' Plot from NBAYES object
#'
#' @description Plot the normalized statistics of the link between Y an each variable
#'
#' @param objet NBAYES object
#' @export
plot.NBAYES = function(objet){
  summary(objet)
  cat("Plot of the \"normalized\" statistics of the link between", objet$Y, "and each variable on the plot window")
  s <- sum(objet$signif[,"Statistic"])
  c <- objet$signif[, "Statistic"]
  plot(c, t='l', main = 'Normalized statistics', xlab = "Sorted X variables", ylab = "part of the significance")
}
