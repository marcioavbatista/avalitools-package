\name{lm_avalitools}
\alias{lm_avalitools}
\title{Linear model for avaliation.}
\description{Creates an lm object for the given data.frame using the las column
the dependent variable.}
\usage{
lm_avalitools(df)
}
\arguments{
\item{df}{
df is an data.frame with the variables represent as columns.
the variables must be already transformed.
}

}

\value{
A list with the lm object and an anova table.
}
\examples{
modelo <- lm_avalitools(df)
lm <- modelo$modelo
anova <- modelo$anova
}