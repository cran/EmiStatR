\name{input-class}
\Rdversion{1.1}
\docType{class}
\alias{input-class}
\alias{input}
\alias{EmiStatR,input-method}

\title{Class \code{"input"}}
\description{
The class provides a container for inputs required to invoke \code{EmiStatR} method.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{input()} or \code{new("input")}.
}
\section{Slots}{
  \describe{
    \item{\code{spatial}:}{Object of class \code{"numeric"}, 0 (default ) for non-spatial 
    input, 1 for spatial input (not implemented).}
    \item{\code{zero}:}{Object of class \code{"numeric"}, aproximation to zero value. 
    Default 1E-5.}
    %\item{\code{mc}:}{Object of class \code{"numeric"}, 0 (default) for simulation 
    %without Monte Carlo sampling, 1 for Monte Carlo simulations with hydraulic 
    %parameters (not implemented), 2 for Monte Carlo simulations with hydraulic 
    %and water quality parameters (not implemented), 3 for Monte Carlo simulations 
    %and Latin Hypercube sampling with hydraulic and water quality parameters 
    %(not implemented).} 
    \item{\code{folder}:}{Object of class \code{"character"}, path of the Shiny 
    applications. Default } 
    \item{}{\code{system.file("shiny", package = "EmiStatR")}}
    \item{\code{folderOutput}:}{Object of class \code{"character"}, path of the 
    directory to save outputs. By default the same as \code{folder}. }
    \item{\code{cores}:}{Object of class \code{"numeric"}, number of CPU cores 
    to be used in parallel computing. If cores = 0 no parallel computation is  
    done. Default 1.}
    \item{\code{ww}:}{Object of class \code{"list"}. This list contains three 
    numeric elements for the wastewater characteristics. First element
    \code{qs}, individual water consumption of households [l/(PE d)]. Second
    element \code{CODs}, sewage pollution - COD concentration [g/(PE d)]. Third
    element \code{NH4s}, sewage pollution - NH4 concentration [g/(PE d)].}
    \item{\code{inf}:}{Object of class \code{"list"}. This list contains three 
    numeric elements for infiltration water characteristics. First 
    element \code{qf}, infiltration water inflow [l/(s ha)]. Second element
    \code{CODf}, infiltration water pollution - COD concentration [g/(PE d)].
    Third element \code{NH4f}, infiltration water pollution - NH4 concentration 
    [g/(PE d)].}
    \item{\code{rw}:}{Object of class \code{"list"}. This list contains three 
    elements for rainwater characteristics. First 
    element \code{CODr} (numeric), rainwater pollution - COD concentration [mg/l].
    Second element \code{NH4r} (numeric), rainwater pollution - NH4 concentration 
    [mg/l]. Third element \code{stat} (character), name of the rain measurement 
    station.}
    \item{\code{tf}:}{Object of class \code{"numeric"}, stormwater runoff. Flow 
    time in the sewer system [min]. If 
    \code{tf} is less or equal than 20 min, then af = 1, i.e. no attenuation of the 
    rainfall.}
    \item{\code{P1}:}{Object of class \code{"data.frame"} with columns named tt 
    (date and time), P (rain time series), and i (intensity). }
    \item{\code{st}:}{Object of class \code{"list"}. This object contains n lists, 
    where n is the number of structures to simulate. Every list should contain 12
    elements: \code{id}, identification number [-]; \code{ns}, name of the structure [-];
    \code{nm}, name of the municipality [-]; \code{nc}, name of the catchment [-];
    \code{numc}, number of the catchment [-]; \code{use}, use of the soil [-]; 
    \code{Ages}, total area [ha]; \code{Ared}, reduced area - impervious area [ha];
    \code{tfS}, time flow structure [min]; \code{pe}, population equivalent [PE];
    \code{Qd}, throttled outflow [l/s]; and \code{V}, volume [m3].}
    \item{\code{export}:}{Object of class \code{"numeric"}. If 1 (default) then the 
    results are saved in \code{folderOutput}. Set to 0 for not writing in 
    output files.}
  }
}

\section{Methods}{
  \describe{
    \item{EmiStatR}{\code{signature(x = "input")}: execute {EmiStatR} function }
    }
}

%\references{
%%%  ~~put references to the literature/web site here~~
%}

\author{
J.A. Torres-Matallana
}

%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
\examples{
showClass("input")

## running EmiStatR with default input
library("EmiStatR")

  # creating an Input object
  input.default <- new("input")
  str(input.default)

  # running EmiStat
  sim1 <- EmiStatR(input.default)

## running EmiStatR with user defined input
  data("P1")

  # defining estructures E1 and E2
  E1 <- list(id = 1, ns = "Goesdorf", nm = "Goesdorf", nc = "Obersauer", numc = NA, 
             use = "Residencial/Industrial", Ages = 16.5, Ared = 7.6, tfS = 10, 
             pe = 611, Qd = 9, V = 190)

  E2 <- list(id = 2, ns = "Kaundorf", nm = "Kaundorf", nc = "Obersauer", numc = NA,
             use = "Residencial/Industrial", Ages = 22, Ared = 11, tfS = 10,
             pe = 358, Qd = 9, V = 180)

  # defining Input objet
  input.user <- input(spatial = 0, zero = 1e-5, folder = system.file("shiny", package = "EmiStatR"),
                      folderOutput = system.file("shiny", package = "EmiStatR"), cores = 1,
                      ww = list(qs = 150, CODs = 120, NH4s = 11), 
                      inf = list(qf= 0.05, CODf = 0, NH4f =0),
                      rw = list(CODr = 107, NH4r = 0, stat = "Dahl"), 
                      tf = 20, P1 = P1, 
                      st = list(E1=E1, E2=E2), export = 1)
  str(input.user)

  # invoking EmiStatR
  sim2 <- EmiStatR(input.user)
}

\keyword{classes}

%\usage{
%Input(...)
%}
%\arguments{
%  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
%}
%}