\name{cat_df}
\alias{cat_df}
\title{Cat comand for data.frames}
\description{Prints a data.frame with the knitr::kable.}
\usage{
cat_df(df, colnames, rownames)
}
\arguments{
\item{df}{
df is the data.frame that will be printed.
}

\item{colnames}{
colnames is an string vector that represent the column names
}

\item{rownames}{
rownames is an string vector that represent the row names
}


}

\value{
NULL.
}
\examples{
cat_df(df, colnames = NULL, rownames = NULL)
}