% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discretiseNB.R
\name{rpart_transform}
\alias{rpart_transform}
\title{Discretise newdata by using the rpart model (objetRpart)}
\usage{
rpart_transform(objetRpart, newdata)
}
\arguments{
\item{objetRpart}{The objetRpart object returned by fit.rpart}

\item{newdata}{New data to discretise}
}
\value{
Dataframe of discretised newdata
}
\description{
Discretise newdata by using the rpart model (objetRpart)
}
\examples{
data(iris)
df1 = iris[1:120,]
Y1 = df1["Species"]
quanti1 = df1[colnames(df1) != "Species"]
discretised = rpart_fit(Y1, quanti1)
df2 = iris[121:150,]
quanti2 = df2[colnames(df2) != "Species"]
newdata_discretised = rpart_transform(discretised, quanti2)
}
