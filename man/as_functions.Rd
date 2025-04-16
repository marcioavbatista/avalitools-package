\name{as_functions}
\alias{as_functions}
\title{As Functions}
\description{Creat a list of functions tha correspond to the given string vector.}
\usage{
as_functions(trans)
}
\arguments{
\item{trans}{
trans is a string vector with the transformations used at the model
implemented transformations are (lnx , x2 , x3 , 1/x)
}


}

\value{
A list of corresponding functions for the given string vector.
}
\examples{
list_of_functions <- as_functions( c( lnx,1/x,x2,lny ) )
}