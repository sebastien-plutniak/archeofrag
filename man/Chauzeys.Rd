\name{Chauzeys}
\docType{data}

\alias{Chauzeys}
\alias{chauzeys.connection}
\alias{chauzeys.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Chauzeys site}
\description{
   Chauzey is an open-air Palaeolithic site located in France. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{chauzeys.connection}: connection relationships between fragments.
        \item\code{chauzeys.fragments}: information about the fragments.
    }
}
\usage{data(Chauzeys)}
\format{
    \itemize{
        \item\code{chauzeys.connection} is a 1879x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{chauzeys.fragments} is a 2167x9 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item layer: stratigraphic layer where the fragment was found
            \item volume: estimated volume of the fragment computed as width by thickness by length
            \item width: width of the fragment (in millimeter)
            \item thickness: thickness  of the fragment (in millimeter)
            \item length: length of the fragment (in millimeter)
            \item x: x coordinate of the location where the lithic fragment was found (in metre)
            \item y: y coordinate of the location where the lithic fragment was found (in metre)
            \item z: z coordinate of the location where the lithic fragment was found (in metre)
        }
    }
    
}
\references{
    \itemize{
		\item Chadelle J.-P. (dir.), with M. Baillet, A. Barbeyron, M. Brenet, H. Camus, É. Claud, F. Jude, S. Kreutzer, A. Michel,  N. Mercier, M. Rabanit, S. Save, F. Sellami, A. Vaughan-Williams. 2021. Chauzeys, Saint-Médard-de-Mussidan (Dordogne, Nouvelle-Aquitaine), Rapport final d'opération archéologique, Périgueux, Conseil départemental de la Dordogne, Service départemental d'archéologie.
        }
    }
\keyword{datasets}

\examples{
data(Chauzeys)
head(chauzeys.fragments)
}

