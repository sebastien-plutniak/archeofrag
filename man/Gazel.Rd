\name{Gazel}
\docType{data}

\alias{gazel}
\alias{gazel.connection}
\alias{gazel.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the gazel site}
\description{
   Gazel is a cave located in Southern France, including Ancient and Middle Neolithic occupations. This dataset describes the refitting 'connection' relationships between pottery fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. The material is located using three spatial variables:
       \itemize{
        \item zone: sectors of the site, including: the Collapsed area, Northern part of the central room, and Southern part of the central room.
        \item layer: stratigraphic layer.
        \item layer.group: chronocultural division assigned to sets of stratigraphic layers.
    }
   Individual objects' X and Y coordinates were not available and were approximated from the grid square where the fragments were excavated. Note that two grids were used (for the central room and the collapsed area, respectively): the difference in the grids orientation (18°) was neglected when computing square coordinates.
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{gazel.connection}: connection relationships between fragments.
        \item\code{gazel.fragments}: information about the fragments.
    }
    Metadata are accessible through the \code{comment()} function.
}
\usage{data(Gazel)}
\format{
    \itemize{
        \item\code{gazel.connection} is a 107x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{gazel.fragments} is a 131x6 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item zone: site sector 
            \item layer:  stratigraphic layer
            \item layer.group: chronocultural division assigned to sets of stratigraphic layers
            \item x: x coordinate of the excavation grid square where the fragment was found (in centimetres)
            \item y: y coordinate of the excavation grid square where the fragment was found (in centimetres)
        }
    }
    
}
\references{
    \itemize{
      \item   Caro J. 2026. Archaeological data from the Gazel site (France) Neolihic Epicardial and Postcardial occupations (pottery refits). https://doi.org/10.5281/zenodo.24093647.
   }
}
\keyword{datasets}

\examples{
data(Gazel)
head(gazel.fragments)
comment(gazel.fragments)
}

