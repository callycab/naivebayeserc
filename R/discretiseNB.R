
#' Split a dataframe
#'
#' @description Split a DataFrame in 3 parts : categorial (quali), numeric (quanti) and the variable to predict(Y) if present in data.
#'
#' @param formula
#' @param data
#'
#' @return A list with 3 dataframes : quali, quanti and Y
#' @import dplyr
#' @export
#'
#' @examples
#' data(ChickWeight)
#' df = ChickWeight
#' f = Diet ~ .
#' result = splitdf(f, df)
#' categorial = result$quali
#' Y = result$Y
#' numeric = result$quanti
#' print(colnames(categorial))
#' print(colnames(numeric))
#' print(colnames(Y))
splitdf = function(formula, data){
  # Split quanti and quali

  quali = select_if(data, is.factor)

  coln = colnames(quali)
  quanti = select(data, -one_of(coln))

  Y.pres = paste(formula[[2]]) %in% colnames(data)
  if(Y.pres){
    quali = select(quali, -one_of(paste(formula[[2]])))
    Y = select(data, paste(formula[[2]]))
  } else{
    Y = data.frame()
  }
  return(list(quanti = quanti, quali = quali, Y = Y))
}


#' Discretise with rpart method
#'
#' @param Y
#' @param quanti
#'
#' @description Discretise with rpart method using the library rpart
#'
#' @return A Dataframe with values discretised
#' @export
#'
#' @import rpart
#' @examples
fit.rpart = function(Y, quanti){
  #Fonction qui calcule un arbre de d?cision pour chaque X en fonction de Y
  #X : variables explicatives
  #Y : variable ? expliquer


  #Discr?tisation de chaque variable
  cutp = list()
  quanti_disc = c()
  num_col = c()
  noms_col = colnames(quanti)
  for (i in 1:ncol(quanti)){#parcours des variables

    var_to_disc = quanti[[i]] #var_to_disc = select(quanti, one_of(noms_col[i])) #r?cup?ration des donn?es de la variable ? discr?tiser
    Y = as.vector(t(Y))
    tree = rpart(Y ~ var_to_disc) #arbre de d?cision entre Y et la variable ? discr?tiser

    if (!is.null(tree$splits)){
      num_col = append(num_col, i) #num?ro de colonne de la variable qu'on garde
      bornes = c(-Inf, sort(tree$splits[,'index']), +Inf) #concat?nation des points de coupure
      var_disc = cut(var_to_disc, bornes, labels=FALSE) #attribution des classes

      #Ajout des r?sultats
      cutp = append(cutp, list(bornes)) #liste des points de coupure
      quanti_disc = cbind(quanti_disc, var_disc) #tableau des variables discr?tis?es
    }
  }

  disc.data = as.data.frame(quanti_disc) #transformation en data frame
  colnames(disc.data) = noms_col[num_col] #r?cup?ration des noms de variables
  return(list(cutp = cutp, disc.data = disc.data)) #sortie : liste compos?e des points de coupure et du data frame des variables discr?tis?es
}

#' Discretise newdata by using the rpart model
#'
#' @param objetRpart The Rpart object returned by fit.rpart (model)
#' @param newdata New data to discretise
#'
#' @return Dataframe of discretised data
#' @export
#'
#' @import rpart
#' @examples
transform.rpart = function(objetRpart, newdata){
  #Fonction qui discr?tise un nouveau data frame selon des points de coupures existants
  #objetRpart : objet issu de la m?thode rpart_fit
  #newdata : nouveau data frame ? discr?tiser

  newdata_disc = c()

  for(i in 1:ncol(newdata)){ #parcours des variables
    var_to_disc = newdata[[i]] #r?cup?ration des donn?es de la variable ? discr?tiser
    num_cp = which(colnames(objetRpart$disc.data) == colnames(newdata[i])) #r?cup?ration du num?ro de variable pour les points de coupure
    cp = objetRpart$cutp[[num_cp]] #r?cup?ration des points de coupure de la variable
    var_disc = cut(var_to_disc, cp, labels = FALSE) #attribution des classes
    newdata_disc = cbind(newdata_disc, var_disc) #concat?nation des variables discr?tis?es entre elles
  }

  colnames(newdata_disc) = colnames(newdata) #r?cup?ration des noms de variables
  return(as.data.frame(newdata_disc)) #sortie : un dataframe des variables discr?tis?es
}

