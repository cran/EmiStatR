\name{Esch_Sure2010}
\alias{Esch_Sure2010}
\docType{data}
\title{
An example time series for the EmiStatR package
}
\description{
This dataset is a data.frame with two columns: Time [y-m-d h:m:s] and Precipitation P [mm]. 
The station of measuring, Esch Sure, is located close to the catchment of the combined sewer 
overflow tank at Goesdorf, Grand-Duchy of Luxembourg.
}
\usage{data("Esch_Sure2010")}
\format{
  A data frame with 52560 observations on the following 2 variables.
  \describe{
    \item{\code{time}}{a POSIXct}
    \item{\samp{P [mm]}}{a numeric vector}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(Esch_Sure2010)

plot(Esch_Sure2010[,1], Esch_Sure2010[,2], col="blue", typ="l", 
     xlab = "time", ylab = "Precipitation [mm]") 

}

\keyword{datasets}
