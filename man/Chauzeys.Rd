\name{Chauzeys}
\docType{data}

\alias{Chauzeys}
\alias{chauzeys.connection}
\alias{chauzeys.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Chauzeys site}
\description{
   Chauzeys is an open-air Palaeolithic site located in France. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{chauzeys.connection}: connection relationships between fragments.
        \item\code{chauzeys.fragments}: information about the fragments.
    }
    Metadata are accessible through the \code{comment()} function.
}
\usage{data(Chauzeys)}
\format{
    \itemize{
        \item\code{chauzeys.connection} is a 1846x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{chauzeys.fragments} is a 2119x9 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item layer:  chronocultural unit associated to the fragment was found. Values are numbered in expected chrono-stratigraphic order
            \item volume: estimated volume of the fragment computed as width by thickness by length (in cubic centimetres)
            \item width: width of the fragment (in millimeters)
            \item thickness: thickness  of the fragment (in millimeters)
            \item length: length of the fragment (in millimeters)
            \item x: x coordinate of the location where the lithic fragment was found (in metres)
            \item y: y coordinate of the location where the lithic fragment was found (in metres)
            \item z: z coordinate of the location where the lithic fragment was found (in metres)
        }
    }
    
}
\references{
    \itemize{
        \item Chadelle, J.-P. 2026. Archaeological data from the Chauzeys (France) Palaeolithic occupation (lithic material description and refits). https://doi.org/10.5281/zenodo.22814035.
		\item Chadelle J.-P. (dir.), with M. Baillet, A. Barbeyron, M. Brenet, H. Camus, É. Claud, F. Jude, S. Kreutzer, A. Michel,  N. Mercier, M. Rabanit, S. Save, F. Sellami, A. Vaughan-Williams. 2021. Chauzeys, Saint-Médard-de-Mussidan (Dordogne, Nouvelle-Aquitaine), Rapport final d'opération archéologique, Périgueux, Conseil départemental de la Dordogne, Service départemental d'archéologie.
        }
    }
\keyword{datasets}

\examples{
data(Chauzeys)
head(chauzeys.fragments)
comment(chauzeys.fragments)
}

