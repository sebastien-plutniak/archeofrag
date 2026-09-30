\name{Villetoureix}
\docType{data}

\alias{Villetoureix}
\alias{villetoureix.connection}
\alias{villetoureix.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Villetoureix site}
\description{
   Villetoureix is an open-air Upper Paleolithic (Magdalenian) site located in France. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. The material was associated to only one spatial unit.
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{villetoureix.connection}: connection relationships between fragments.
        \item\code{villetoureix.fragments}: information about the fragments.
    }
    Metadata are accessible through the \code{comment()} function.
}
\usage{data(Villetoureix)}
\format{
    \itemize{
        \item\code{villetoureix.connection} is a 522x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{villetoureix.fragments} is a 535x10 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item material: type of raw lithic material
            \item volume: estimated volume of the fragment computed as width by thickness by length (in cubic centimetres)
            \item width: width of the fragment (in millimeter)
            \item thickness: thickness  of the fragment (in millimetres)
            \item length: length of the fragment (in millimetres)
            \item weight: weight of the fragment (in gramms)
            \item x: x coordinate of the location where the lithic fragment was found (in metres)
            \item y: y coordinate of the location where the lithic fragment was found (in metres)
            \item z: z coordinate of the location where the lithic fragment was found (in metres)
        }
    }
    
}
\references{
    \itemize{
      \item   Chadelle J.-P. 2026. Archaeological data from the Villetoureix (France) Upper Palaeolithic occupation (lithic material description and refits). https://doi.org/10.5281/zenodo.22813846.
      \item   Michel A. (ed.), with D. Aoustin, M. Baillet, P. Bertan, K. Charlier, I. Couchoud, J.-C. Castel, J.-P. Chadelle, C. Fat Cheung, C. Hatté, N. Limondin-Lazouet, H. Pradier, A. Queffelec. 2021. Chez Thuilet, secteur Magdalénien (Villetoureix, Dordogne). Rapport d’aide à la publication. Périgueux, Conseil départemental de la Dordogne, Service départemental d'archéologie / Ministère de la Culture.
   }
}
\keyword{datasets}

\examples{
data(Villetoureix)
head(villetoureix.fragments)
comment(villetoureix.fragments)
}

