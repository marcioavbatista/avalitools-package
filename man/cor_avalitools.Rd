\name{cor_avalitools}
\alias{cor_avalitools}
\title{Correlation tables for avaliation models.}
\description{Returns a data.frame with an table of correlations.}
\usage{
cor_avalitools(df, vari_uti, inf = F)
}
\arguments{
\item{df}{
df is a data.frame with the transformed variables represented in the columns.
}

\item{vari_uti}{
vari_uti is an string vector with the names of variables.
}

\item{num_d}{
inf is an boolean that determinates wich type of correlation is returned.
}

}

\value{
A data frame with correlation for all variables an their respective names.
}
\examples{
df_cor <- cor_avalitools(df, vari_uti, inf = F)

df_cor_inf <- cor_avalitools(df, vari_uti, inf = T)
}