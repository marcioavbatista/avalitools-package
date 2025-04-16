\name{normality_table}
\alias{normality_table}
\title{Normality table.}
\description{Creates an data.frame with a table for checking normality for the residuals.}
\usage{
nomality_table(modelo)
}
\arguments{
\item{modelo}{
modelo is an lm object.
}

}

\value{
A data.frame with a table for checkin normality pressuposts.
}
\examples{
norm_table <- normality_table(modelo)
}