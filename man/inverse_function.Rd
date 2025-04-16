\name{inverse_function}
\alias{inverse_function}
\title{Inverse Function.}
\description{Returns the inverse function.}
\usage{
inverse_function(fun)
}
\arguments{
\item{fun}{
fun is an string object that represents a function
possible values:
1/x ; x2; x3; lnx
}

}

\value{
A function inverse to the given parameter.
}
\examples{
fun <- "lnx"
inverse_fun <- inverse_function(fun)
# inverse_fun will be an exponential function
}