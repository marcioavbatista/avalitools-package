\name{read_avaliation}
\alias{read_avaliation}
\title{Read Avaliation}
\description{Read an avaliation excel arquive.}
\usage{
read_avaliation(path)
}
\arguments{

\item{path}{
path is an string object with the path of the excel arquive.
}

}

\value{
A list with data.frame for the data, variables and informations and others objects.
}
\examples{
avaliacao <- read_avaliation(path)
}