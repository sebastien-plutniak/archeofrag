\name{BargerGulch}
\docType{data}

\alias{BargerGulch}
\alias{bargergulch.connection}
\alias{bargergulch.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Barger Gulch site}
\description{
   Barger Gulch is an open-air Folsom site located in the United States of America. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. The material was associated to only one spatial unit.
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{bargergulch.connection}: connection relationships between fragments.
        \item\code{bargergulch.fragments}: information about the fragments.
    }
    
    Metadata are accessible through the \code{comment()} function.
}
\usage{data(BargerGulch)}
\format{
    \itemize{
        \item\code{bargergulch.connection} is a 1082x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{bargergulch.fragments} is a 1205x5 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item type: type of lithic object
            \item x: x coordinate of the location where the lithic fragment was found (in centimetres)
            \item y: y coordinate of the location where the lithic fragment was found (in centimetres)
            \item z: z coordinate of the location where the lithic fragment was found (in centimetres)
        }
    }
    
}
\references{
    \itemize{
      \item Surovell T. A. 2020. Barger Gulch Locality B database (tDAR id: 458541), doi: https://www.doi.org/10.48512/XCV8458541.
      \item Surovell T. A. 2022. Barger Gulch: A Folsom Campsite in the Rocky Mountains. University of Arizona Press: Tucson.
      }
    }
\keyword{datasets}

\examples{
data(BargerGulch)
head(bargergulch.fragments)
comment(bargergulch.fragments)
}

