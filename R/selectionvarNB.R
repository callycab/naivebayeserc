## Prend un vecteur en entree et retourne H(vecteur)
H_ <- function(variable){
  T_proba <- prop.table(table(variable))
  H <- -sum(sapply(T_proba, p_log2_p))
  return(H)
}

## Prend proba en entree et retourne proba*log2(proba)
p_log2_p <- function(proba){
  plog2p <- proba*log2(proba)
  return(plog2p)
}

## Prend deux variables en entree et retourne l'information mutuelle
I_ <- function(classe, variable){
  t_croise <- addmargins(prop.table(table(c(classe, variable))))
  n_modalite_classe <- length(unique(classe[!is.na(classe),]))
  n_modalite_variable <- length(unique(variable[!is.na(variable),]))
  I <- 0
  for(i in 1:n_modalite_classe){
    for(j in 1:n_modalite_variable){
      I <- ifelse(t_croise[i, j] == 0, I, I + t_croise[i, j]*log2(t_croise[i, j]/(t_croise[i, n_modalite_variable+1]*t_croise[n_modalite_classe+1, j])))
    }
  }
  return(I)
}

## Prend deux variables en entree et retourne la statistique G (lien entre les deux variables)
G_ <- function(classe, variable){
  variable = as.data.frame(variable)
  n <- nrow(classe)
  n_modalite_classe <- nrow(unique(classe))
  n_modalite_variable <- nrow(unique(variable))
  df <- (n_modalite_classe-1)*(n_modalite_variable-1)
  g = c()
  g["Statistic"] <- 2*n*log(2)*I_(classe, variable)
  g["df"] <- df
  g["p value"] <- 1-pchisq(g["Statistic"], df)
  return(g)
}


#' Compute the significance of the link between each variable X and Y
#'
#' @param y Column Dataframe, Variable to compare others with. In practice, it is the variable of the classification
#' @param X Dataframe containing all the others variables to compute the significance link with y
#'
#' @return Column Dataframe containing a score of link and the p-value
#' @export
#'
#' @examples
#' data(iris)
#' df = iris
#' y = df["Species"]
#' X = df[colnames(df) != "Species"]
#' signif = signification_(y,X)
signification_ <- function(y, X){
  signification <- apply(X, 2, G_, classe = y)
  m <- t.default(as.matrix(signification))
  m_ordered <- m[order(m[,"Statistic"], decreasing = TRUE),]
  return(m_ordered)
}

## Prend deux variables en entree et retourne l'indicateur symetrical uncertainty
s_ <- function(classe, variable){
  I <- I_(classe, variable)
  H_Y <- H_(classe)
  H_X <- H_(variable)
  s <- 2*I/(H_Y+H_X)
  return(s)
}

## Renvoie le merit entre un vecteur classe et des variables
merit_ <- function(y, X){
  if(ncol(X)==1){
    merit <- s_(y, X)
    return(merit)
  } else {
    s_y_x = c()
    for(i in 1:ncol(X)){
      s_y_x <- c(s_y_x, s_(y, X[i]))
    }

    s_x_x = c()
    for(j in 1:ncol(X)){
      for(k in 1:ncol(X)){
        if(k > j)
          s_x_x <- c(s_x_x, s_(X[k], X[j]))
      }
    }

    merit <- (ncol(X)*mean(s_y_x))/sqrt(ncol(X)+ncol(X)*(ncol(X)-1)*mean(s_x_x))
    return(merit)
  }
}

#' Step-forward variables selection
#'
#' @description Methode used by fit.NBAYES to select variables.
#' @description It is an implementation of CFS (Correlation-based Feature Selection) method.
#'
#' @param y classe to predict
#' @param X Dataframe of variables to choose from
#'
#' @return A list with the name of selected variables
#' @export
#'
#' @examples
#' data(iris)
#' df = iris
#' y = df["Species"]
#' X = df[colnames(df) != "Species"]
#' xselect = selection_(y,X)
selection_ <- function(y, X){
  merit_total <- 0
  var_total <- c()
  continue <- TRUE
  while(continue){
    if(length(var_total)==0){
      X_rest <- X # aucune variable n'a ete encore selectionnee, donc il reste l'entierete des variables
      X_total <- X_rest[var_total] # definition du df des variables deja selectionnees
    } else {
      X_total <- X[var_total]
      X_rest <- as.data.frame(X[,!(colnames(X) %in% var_total)]) # definition du df des autres variables (celles pas encore selectionnees)
    }
    new_merit <- c() # vecteur contenant le nouveau merite pour l'ajout de chacune des autres variables
    for(j in 1:ncol(X_rest)){ # boucle sur chacune des autres variables
      new_merit[names(X_rest[j])] <- merit_(y, cbind(X_total, X_rest[j])) # calcul du merite apres ajout de chacune des autres variables
    }
    if(max(new_merit)>merit_total){ # si ca augmente le merite on continue
      merit_total <- max(new_merit) # stockage du nouveau merite
      var_total <- c(var_total, names(which.max(new_merit))) # stockage de la variable qui augmente le plus le merite
    } else { # sinon on arrete
      continue <- FALSE
    }
  }
  return(var_total)
}
