% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discretiseNB.R
\name{rpart_fit}
\alias{rpart_fit}
\title{Discretise with rpart method}
\usage{
rpart_fit(Y, quanti)
}
\arguments{
\item{Y}{Classes of observations}

\item{quanti}{Numeric observations to discretise}
}
\value{
An object (objetRpart) with attributs:

cutp = List of cuts (as: -Inf 5.45 6.15 Inf) for each discretised values

disc.data = Dataframe of discretised values. The modalities are factors.
}
\description{
Discretise by recursive tree using the library rpart
}
\examples{
data(iris)
df = iris
Y = df["Species"]
quanti = df[colnames(df) != "Species"]
discretised = rpart_fit(Y, quanti)
}
