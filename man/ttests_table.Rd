\name{ttests_table}
\alias{ttests_table}}
\title{T-student tests for each independent variable.}
\description{Creates an data.frame with the table for the t-student tests}
\usage{
ttests_table(modelo, vari_uti)
}
\arguments{

\item{modelo}{
modelo is an lm object.
}

\item{vari_uti}{
vari_uti is an string vector with the names of the used variables.
}

}

\value{
A data.frame with a table for the tests.
}
\examples{
df_ttests <- ttests_table(modelo, vari_uti)
}