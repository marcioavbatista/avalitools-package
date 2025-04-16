\name{qme}
\alias{qme}
\title{Residual mean square.}
\description{Estimates the residuals std of a linear regression model.}
\usage{
qme(modelo)
}
\arguments{

\item{modelo}{
modelo is an lm object.
}

}

\value{
A numeric value.
}
\examples{
residuals_std <- qme(modelo)
}