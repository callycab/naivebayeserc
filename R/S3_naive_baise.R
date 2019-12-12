# Classe NBAYES
# Constructeur de l'objet NBAYES : calcul des probabilites a priori et conditionnelles

#' Naive Bayes Classifier Constructor
#'
#' @description Build a Naive Bayes Classifier model from a dataframe. It discretise variables, compute conditionnal probabilities and select variables if choosen.
#' @usage fit(formula,data, m=1, discretise="rpart", selectvar=TRUE)
#'
#' @param formula Formula to specifies which variables in data use to fit the model.
#' @param data A dataframe with the data..
#' @param m Integer to use the Laplacian in the calcul of probabilities.
#' @param discretise String to choose the discretising method. Can be "rpart" (recommanded) or "mdlp".
#' @param selectvar Boolean to enable/disable variables selection (stepforward).
#'
#' @return A NBAYES object (S3) with attributs :
#' @return This object has the following attributs:
#' @return formula: The formula given by the user as parameter
#' @return data: Dataframe given by the user as parameter
#' @return m: Integer given by the user as parameter (or m=1)
#' @return disc.param: Name of the methode used for discretising during the fit. It is "rpart" by default, or it can be "mdlp" if user submit it as parameter. Discretisation cannot be disabled.
#' @return disc.values: Result of the discretisation of numeric values in the dataframe data. It is used in the function predict to apply the same discretisation as computed during the fit.
#' @return signif: Dataframe of significance of the link between each variable X and Y, it is used in Summary and Plot. It contains the statistics and the p-value for each variable X.
#' @return Y: Name of the variable to classify. It is extract from the formula and used during the predict.
#' @return X: List of kept values for the classification. It is the result of the formula, the discretisation (some variables can be rejected) and the variables selection if enabled.
#' @return apriori: Table of prior probabilities for each modalities of Y.
#' @return p_conditionnel: List of table of conditional probabilities, for each modalities of every variables.
#'
#' @importFrom FSelectorRcpp discretize
#' @export
#'
#' @examples
#' data(iris)
#' df = iris
#' f = Species ~ .
#' df$Species = as.factor(df$Species)
#' bayes = fit(f, df, discretise = "rpart")
fit = function(formula, data, m=1, discretise="rpart", selectvar=TRUE){

  # Controle sur les parametres
  if (!is.data.frame(data)){
    stop("the parameter data has to be a DataFrame, not ",class(data))
  }
  if (!inherits(formula,"formula")){
    stop("the parameter formula has to be a Formula, not ",class(formula))
  }

  # Instanciation de l'objet
  instance = list()

  # On gade les parametres utilises dans l'objet, afin de pouvoir les consulter ulterieurement
  instance$formula = formula
  instance$data = data

  # Par defaut, on utilise le Laplacien avec m=1
  instance$m = m
  instance$disc.param = discretise

  # Separation X (selon leur type) et Y
  split_data = splitdf(formula,data)
  quali = split_data$quali
  Y = split_data$Y
  quanti = split_data$quanti

  instance$Y = colnames(Y)

  # Suppression des lignes pour lesquelles ont a pas les classes
  names_quali = colnames(quali)
  quali = as.data.frame(quali[complete.cases(Y), ])
  colnames(quali) = names_quali

  names_quanti = colnames(quanti)
  quanti = as.data.frame(quanti[complete.cases(Y), ])
  colnames(quanti) = names_quanti

  Y = as.data.frame(Y[complete.cases(Y), ])
  colnames(Y) = instance$Y


  # S'il y a des quantis on discretise
  if(ncol(quanti)>0){
    # Discretisation avec rpart
    if (discretise == "rpart"){
      disc = rpart_fit(Y, quanti)
      instance$disc.values = disc
      disc = disc$disc.data
    }else if(discretise == "mdlp"){
      # Discretisation avec mldp
      f2 = as.formula(paste(instance$Y,"~ .")) # nouvelle formula incluant uniquement les variables quantis
      quantiY = cbind(quanti,Y)
      disc = FSelectorRcpp::discretize(f2, quantiY)
      instance$disc.values = disc
      disc = disc[,colnames(disc) != colnames(Y)]
    } else{
      stop("Cette discretisation n'est pas proposee.")
    }

    # Merge des X quantis et X qualis
    if(ncol(quali)>0){
      # Si on a des qualis on merge qualis + quantis discretisees
      data = cbind(quali, disc)
    } else{
      # Si on a pas de qualis, on garde que les quantis discretisees
      data = disc
    }
  } else{
    # Si on a pas de quantis, on garde que les qualis
    data = quali
  }

  X = data # X = X apres discretisation

  # Calcul de la significativité des variables de X en fonction de Y.
  # Ceci est indicatif et peut être affiché via Summary
  instance$signif = signification_(y = Y, X = X)

  # Appel a la selection de variables
  if (selectvar){
    xselect = selection_(y = Y, X = X)
    X = X[xselect]
  }

  instance$X = colnames(X) #X gardees apres selection de variables

  # Calcul des probabilites a priori
  classes = table(Y) # Differentes modalites (classes) possibles pour la variable a predire
  instance$apriori = (classes+m) / ( sum(classes)+m*length(classes) )

  # Calcul des probabilites conditionnelles
  instance$p_conditional = list()
  for (i in 1:ncol(X)){
    table_X_selon_Y = table(X[,i], Y[,1])
    num_laplacien = table_X_selon_Y+m # Ajout du paramete m. Par defaut, c'est le Laplacien (m=1)
    denom_laplacien = matrix(colSums(table_X_selon_Y), nrow = nrow(table_X_selon_Y),
                             ncol = ncol(table_X_selon_Y), byrow = TRUE)+m*length(unique(X[,i]))
    instance$p_conditional[[names(X)[i]]] = num_laplacien/denom_laplacien
  }

  class(instance) = "NBAYES"
  return(instance)
}


#' Predict from NBAYES
#'
#' @description Classify new data from a NBAYES model
#'
#' @aliases predict
#' @usage predict(object, newdata, type="class")
#'
#' @param object Object of classe NBAYES fitted model
#' @param newdata Dataframe with data to classify. All the variables used by the model (see NBAYES$X) have to be present in the dataframe.
#' @param type String to choose the format of the output. Can be "class" to only have predicted classes, or "posterior" to have the posterior probability for each class.
#'
#' @return A list with the predictions. The format depends on the parameter type.
#'
#' @importFrom FSelectorRcpp discretize_transform
#' @export
#'
#' @examples
#' data(iris)
#' df = iris
#' f = Species ~ .
#' df$Species = as.factor(df$Species)
#' bayes = fit(f, df, discretise = "rpart")
#' pred = predict(bayes, df)
predict.NBAYES = function(object, newdata, type="class"){
  # Controles sur les parametres
  if (!class(object) == "NBAYES"){
    stop("the object must be of class NBAYES, not", class(object),". Use the function fit.")
  }
  if (!is.data.frame(newdata)){
    stop("the data must be a DataFrame, not ",class(data))
  }
  # Test si toutes les valeurs predictives du modele sont bien presentes dans les donnees a predire
  if (!rlang::is_empty(setdiff(object$X, colnames(newdata)))){
    stop("all the predictives parameters must be present in the dataframe. Missing :", setdiff(object$X, colnames(newdata)))
  }
  # Test sur le parametre type
  if (! type %in% c("class","posterior")){
    stop('type must be in c("class","posterior"). You submitted :', type)
  }

  # On garde uniquement les variables necessaires pour notre modele
  #newdata = newdata[c(object$X, object$Y)]
  newdata = newdata[c(object$X)]
  formula = as.formula(paste(object$Y,"~ ."))

  # Separation X (selon leur type) et Y
  split_data = splitdf(formula,newdata)
  quali = as.data.frame(split_data$quali)
  #Y = as.data.frame(split_data$Y)
  quanti = as.data.frame(split_data$quanti)

  # S'il y a des quantis on discretise
  if (ncol(quanti)>0){
    if (object$disc.param == "rpart"){
      # Si la discretisation du fit etait rpart
      quanti_disc = rpart_transform(object$disc.values, quanti)
    }else{
      # Si la discretisation du fit etait mdlp
      quanti_disc = FSelectorRcpp::discretize_transform(object$disc.values, quanti)
    }
    # Merge des donnees
    if(ncol(quali)>0){
      newdata = cbind(quanti_disc, quali)
    }else{
      newdata = quanti_disc
    }

  }else{
    newdata = quali
  }

  # Liste des probabilites a posteriori et liste des classes retenues
  predict_p = list()
  predict_c = c()

  # Probabilite pour chaque Classe, pour chaque individu
  p_c = c()

  # Pour chaque individu a classer
  for(i in 1:nrow(newdata)){
    topredict = newdata[i,]

    # Calcul des probabilites a posteriori pour chaque classe de la variable a predire
    for(c in rownames(object$apriori)){

      # On part du log de la probabilite a priori ...
      p_c[c] = log(object$apriori[c])

      # ... et on ajoute le log des probabilites conditionnelles de chaque variable predictive
      for(v in object$X){
        modalite_individu = topredict[v][[1]]
        # Si la modalite est NA, on ne prend pas en compte cette variable
        if (!is.na(modalite_individu)){
          p_conditionnal = object$p_conditional[[v]]
          p_c[c] = p_c[c]+log(p_conditionnal[modalite_individu,c])
        }
      } # a la fin de cette boucle, on a la proba pour la classe c
    } # a la fin de cette boucle on a dans c_p un vecteur de score pour chaque classes de C..
    # On transforme ces scores en proba
    p_c = abs(sum(p_c)) + p_c # Les scores sonts negatifs avec les logs. Cette ligne permet de repasser a des scores positifs
    p_c = p_c/sum(p_c)
    predict_p[[i]] = p_c #.. vecteur que l'on ajoute a la liste contenant les probas par classe pour chaque individu a predire
    predict_c[i] = names(which.max(p_c))
  }

  if (type == "class"){
    predict = predict_c # return des classes
  } else {
    predict = predict_p  #return des proba d'appartenance aux classes
  }

  return(predict)
}



