#' Print the NBAYES object
#'
#' @param objet
#'
#' @return
#' @export
print.NBAYES = function(objet){
  cat("NBAYES object (from naivebayeserc)\n\n")
  cat("Discretisation method Used:\n",objet$disc.param,"\n\n")
  cat("Variables to classify:\n",objet$Y,"\n\n")
  cat("Kept variables :\n",objet$X,"\n\n")
  cat("Computed prior probabilities:\n\n")
  print(objet$apriori)
  cat("\n")
  cat("Computed conditional probabilities :\n\n")
  head(bayes$p_conditional)
}
