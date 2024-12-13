\name{BoutDesVergnes}
\docType{data}

\alias{BoutDesVergnes}
\alias{bdv.connection}
\alias{bdv.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Bout des Vergnes site}
\description{
   Bout des Vergnes is an open-air Palaeolithic site located in France. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{bdv.connection}: connection relationships between fragments.
        \item\code{bdv.fragments}: information about the fragments.
    }
}
\usage{data(BoutDesVergnes)}
\format{
    \itemize{
        \item\code{bdv.connection} is a 3931x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{bdv.fragments} is 3666x9 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item layer: chronocultural unit associated to the fragment was found. Values are numbered in expected chrono-stratigraphic order
            \item x: x coordinate of the location where the lithic fragment was found (in metre)
            \item y: y coordinate of the location where the lithic fragment was found (in metre)
            \item z: z coordinate of the location where the lithic fragment was found (in metre)
        }
    }
    
}
\references{
    \itemize{
		\item  Ihuel, E. (dir.), with  Baillet M., Barbeyron A., Brenet M., Camus H., Claud E., Mercier N., Michel A., Sellami F. 2020. Le Bout des Vergnes, Bergerac (Dordogne, Nouvelle-Aquitaine), Contournement ouest de Bergerac, RD 709, Rapport final d’opération, Périgueux, Conseil départemental de la Dordogne, Service départemental d'archéologie.
        }
    }
\keyword{datasets}

\examples{
data(BoutDesVergnes)
head(bdv.fragments)
}

