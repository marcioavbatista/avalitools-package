\name{residuals_tables}
\alias{residuals_tables}
\title{Residuals Table.}
\description{Creates an data.frame with a table for the residuals.}
\usage{
residuals_table(modelo, df, indices)
}
\arguments{

\item{modelo}{
modelo is an lm object.
}

\item{df}{
df is an data.frame object.
}

\item{indices}{
indices is an numeric vector with the sample number of the residuals.
}

}

\value{
A data.frame with a residual table.
}
\examples{
residual_table <- residuals_table(modelo, df, indices)
}