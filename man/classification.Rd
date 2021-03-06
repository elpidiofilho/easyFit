% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classification.R
\name{classification}
\alias{classification}
\title{classification}
\usage{
classification(df.train, formula = NULL, preprocess = NULL,
  classifier = "rf", nfolds = 10, repeats = 1, index = NULL,
  cpu_cores = 4, tune_length = 5, metric = "Kappa", seeds = NULL,
  verbose = FALSE)
}
\arguments{
\item{df.train}{Training dataframe}

\item{formula}{A formula of the form y ~ x1 + x2 + ... If users don't inform formula, the first column will be used as Y values and the others columns with x1,x2....xn}

\item{preprocess}{pre process}

\item{classifier}{Choice of classifier to be used to train model. Uses  algortims names from Caret package.}

\item{nfolds}{Number of folds to be build in crossvalidation}

\item{repeats}{repeats}

\item{index}{index}

\item{cpu_cores}{Number of CPU cores to be used in parallel processing}

\item{tune_length}{This argument is the number of levels for each tuning parameters that should be generated by train}

\item{metric}{metric used to evaluate model fit. For numeric outcome ("RMSE", "Rsquared)}

\item{seeds}{seeds seeds}

\item{verbose}{verbose}
}
\description{
This function performs the training of the chosen classifier
}
\details{
details
}
\examples{
\dontrun{
kappa_cv_evaluation(train,"rf",10,6)
}
}
\author{
Elpidio Filho, \email{elpidio@ufv.br}
}
\keyword{Train}
\keyword{kappa}
