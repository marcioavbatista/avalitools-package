\name{estimate_equations}
\alias{estimate_equations}
\title{Estimate Equations.}
\description{Returns an string object that contains the estimate equations.}
\usage{
estimate_equations(modelo, vari, transf)
}
\arguments{
\item{modelo}{
modelo is an lm model object of the avaliation model.
}

\item{vari}{
vari is an string vector with the names of the variables used at the model.

}

\item{transf}{
transf is an string vector with the respective transformations used atthe model
}



}

\value{
A string object with the estimate equations for central measures.
}
\examples{
equation_mean <- estimative_equation(modelo, vari, transf)
cat(equation_mean)
}