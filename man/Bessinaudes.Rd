\name{Bessinaudes}
\docType{data}

\alias{Bessinaudes}
\alias{bessinaudes.connection}
\alias{bessinaudes.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Bessinaudes site}
\description{
   Bessinaudes is an open-air Paleolithic (Mousterian) site located in France. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. The material was associated to only one spatial unit.
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{bessinaudes.connection}: connection relationships between fragments.
        \item\code{bessinaudes.fragments}: information about the fragments.
    }
    Metadata are accessible through the \code{comment()} function.
}
\usage{data(Bessinaudes)}
\format{
    \itemize{
        \item\code{bessinaudes.connection} is a 522x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in columns 'id1' and 'id2', respectively. 
        \item\code{bessinaudes.fragments} is a 605x9 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item zone: the site was divided into 13 zones
            \item volume: estimated volume of the fragment computed as width by thickness by length (in cubic centimetres)
            \item width: width of the fragment (in millimeters)
            \item thickness: thickness of the fragment (in millimeters)
            \item length: length of the fragment (in millimeters)
            \item x: x coordinate of the location where the lithic fragment was found (in metres)
            \item y: y coordinate of the location where the lithic fragment was found (in metres)
            \item z: z coordinate of the location where the lithic fragment was found (in metres)
        }
    }
    
}
\references{
    \itemize{
		\item Chadelle J.-P. 2026. Lithic refitting data from the Bessinaudes (France) Middle Palaeolithic occupation, https://doi.org/10.5281/zenodo.22813949
		\item Chadelle J.-P. (dir.), with M. Baillet, M. Brenet, H. Camus, E. Claud, A. Michel, N. Mercier, M. Rabanit, F. Sellami. 2018. Les Bessinaudes. Saint-Médard-de-Mussidan (Dordogne Nouvelle-Aquitaine). Rapport final d’opération archéologique. Périgueux, Conseil départemental de la Dordogne, Service départemental d'archéologie.
        }
    }
\keyword{datasets}

\examples{
data(Bessinaudes)
head(bessinaudes.fragments)
comment(bessinaudes.fragments)
}

