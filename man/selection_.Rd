% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectionvarNB.R
\name{selection_}
\alias{selection_}
\title{Step-forward variables selection}
\usage{
selection_(y, X)
}
\arguments{
\item{y}{classe to predict}

\item{X}{Dataframe of variables to choose from}
}
\value{
A list with the name of selected variables
}
\description{
Methode used by fit.NBAYES to select variables.

It is an implementation of CFS (Correlation-based Feature Selection) method.
}
\examples{
data(iris)
df = iris
y = df["Species"]
X = df[colnames(df) != "Species"]
xselect = selection_(y,X)
}
