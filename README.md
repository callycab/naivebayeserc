# naivebayeserc

A Naive Bayse Classifier Package for R

## Installing from Github

```
install.packages("devtools")
install.packages("usethis")
library(usethis)
library(devtools)
install_github("callycab/naivebayeserc")
library(naivebayeserc)
```

## Installing from tar.gz.


If needed, you can also import directly the file naivebayeserc_VERSION.tar.gz to RStudio and load it with :

```
library(naivebayeserc)
```

## Getting Started

Two main functions are available : fit and predict.

- fit is the constructor of the S3 class NBAYES. Use it to create your naive bayes classifier from a dataframe and a formula.
- predict is used to classify new observations, from your created NBAYES model.

Use ?fit and ?predict.NBAYES for more informations.

## License

This project is licensed under GPL-3 - see the [LICENSE.md](LICENSE.md) file for details
