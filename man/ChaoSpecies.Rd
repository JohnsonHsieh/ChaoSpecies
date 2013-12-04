\name{ChaoSpecies}
\alias{ChaoSpecies}
\title{
Estimation of Species richness
}
\description{
\kbd{ChaoSpecies} is a function to provide a Species richness estimator proposed by Chao (2005) et al. \cr\cr
For individual-based (abundance) data, there are eight estimators which are Homogeneous Model(1992) estimators; Homogeneous MLE (1992) estimator; Chao1 (1984) estimator; Chao1-bc (2005) estimator; ACE (1992) estimator; Ace-1 (1992) estimator; 1st order jackknife (1978) estimators and the 2nd order jackknife (1978) estimators. \cr\cr
For sample-based (incidence) data, there are seven other estimators which are Homogeneous Model(1994) estimators; Chao2 (1987) estimator; Chao2-bc (2005) estimator; ICE (1994) estimator; Ace-1 estimator; 1st order jackknife (1978) estimators and the 2nd order jackknife (1978) estimators.
}
\usage{
ChaoSpecies(data, datatype = c("abundance", "incidence"), k = 10, conf = 0.95)
}
\arguments{
  \item{data}{
   a vector of species abundance or incidence frequency. If \code{datatype = "incidence"}, then the input format of first entry should be total number of sampling units, and followed by species incidence frequency.}
  \item{datatype}{
  the data type of input data. That is individual-based abundance data (\code{datatype = "abundance"}) or sample-based incidence data (\code{datatype = "incidence"}).}
  \item{k}{
  cut-off point. It is a value that separates frequency counts into abundant and rare groups
}
  \item{conf}{
  a positive number \eqn{\le} 1. "\code{conf}" specifies the confidence level for confidence   interval. The default is \code{0.95}.}
}
\value{
\kbd{ChaoSpecies} returns a table of various species estimators, their standard error and 95 \% confidence interval which the \code{method} you choose.
}
\references{
Chao, A. and Lee, S.-M. (1992). Estimating the number of classes via sample converage. \emph{Journal of the American Statistical Association}, \bold{87}, 210-217. \cr\cr
Chao, A. (1984). Nonparametric estimation of the number of classes in a population. \emph{Scandinavian Journal of Statistics}, \bold{11}, 265-270. \cr\cr
Chao, A. (2005). Species estimation and applications. \emph{Encyclopedia of Statistical Sciences}, Second Edition, Vol. 12, 7907-7916 (N. Balakrishnan, C. B. Read and B. Vidakovic, Editors), Wiley, New York. \cr\cr
Burnham, K. P. and Overton, W. S. (1978). Estimaton of the size of a closed population when capture probabilities vary among naimals, \emph{Biometrika}, \bold{65}, 625-633. \cr\cr
}
\author{
K.S. Tseng ; Y.H. Lee : Anne Chao\cr
Maintainer : Anne Chao \email{chao@stat.nthu.edu.tw}
}
\seealso{
  \code{\link[ChaoSpecies]{SpeciesAbundance}}, 
  \code{\link[ChaoSpecies]{SpeciesIncidence}}
}
\examples{
# load the individual-base (abundance) data
data(Spider_data)

# Estimation of species richness
ChaoSpecies(Spider_data, datatype="abundance", k=10, conf=0.95)

# load the sample-base (incidence) data
data(Ant_data)

# Estimation of species richness
ChaoSpecies(Ant_data, datatype="incidence", k=10, conf=0.95)
}
\keyword{SPECIES}
