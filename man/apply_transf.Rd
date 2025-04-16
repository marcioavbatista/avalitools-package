\name{apply_transf}
\alias{apply_transf}
\title{Apply Transformations}
\description{Aplly transformations to the columns of a data.frame.}
\usage{
apply_transf(df, trans, is.string)
}
\arguments{
\item{df}{
df is a data.frame tha will be transformed
}

\item{trans}{
trans is a list of transformations or a vector string that contaings the
transformations tha will be aplied in each variable in order.
}

\item{is.string}{
is.string indicates if the parameter trans is a vector string or a list of functions
}

}

\value{
A data.frame with the columns transformed according to the given list
}
\examples{
df_transformed <- apply_transf(df,transf = c("lnx","1/x","x2","y3"), is.string = TRUE)

df_transformed <- apply_transf(df,transf = list(log,sqrt,exp,abs), is.string = FALSE)
}
