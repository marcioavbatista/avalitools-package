\name{regression_equation}
\alias{regression_equation}
\title{Regression Equation }
\description{Creates an string object with the equation
of the given regression linear model.}
\usage{
regression_equation(modelo,vari, trans)
}
\arguments{

\item{modelo}{
modelo is an lm object.
}

\item{vari}{
vari is an string vector with the names of the variables.
}

\item{trans}{
trans is an string vector with the names of the transformations.
}

}

\value{
A string object with the equation of the regression model.
}
\examples{
reg_equation <- regression_equation(modelo,vari, trans)
}