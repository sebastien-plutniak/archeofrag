\name{CovaGran7P}
\docType{data}

\alias{CovaGran7P}
\alias{cova.gran7p.connection}
\alias{cova.gran7p.fragments}

\title{Dataset: Refitting relationships between lithic and bone fragments from the Cova Gran site, level 7P}
\description{
   Cova Gran is a prehistoric site located in Spain. This dataset describes the refitting 'connection' relationships between lithic and bone fragments found during the excavation of the Magdalenian 7P level. Connection relationship refers to physical connection between two bone fragments that were part of the same object. 
    
    The dataset includes two tables:
    \itemize{
        \item\code{cova.gran7p.connection}: connection relationships between fragments.
        \item\code{cova.gran7p.fragments}: information about the fragments.
    }
    Metadata are accessible through the \code{comment()} function.
}
\usage{data(CovaGran7P)}
\format{
    \itemize{
        \item\code{cova.gran7p.connection} is a 339x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{cova.gran7p.fragments} is 311x11 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item level: stratigraphic level
            \item material: raw material
            \item length: length of the fragment (in millimetre)
            \item width: width of the fragment (in millimetre)
            \item surface: approximated surface of the fragment, product of the thickness by the length (in square centimetre)
            \item volume: approximated volume of the fragment, product of the length by the thickness by the length (in cubic centimetre)
            \item weight: weight of the fragment (in gramme)
            \item x: x coordinate of the grid square where the bone fragment was found (in centimetre)
            \item y: y coordinate of the grid square where the bone fragment was found (in centimetre)
            \item z: z coordinate of the grid square where the bone fragment was found (in centimetre)
        }
    }
    
}
\references{
    \itemize{
		\item  Sanchez-Martinez J. 2026. Needle-thin: reconstructing on-edge microblade production and contextual integrity through lithic refitting in the Magdalenian of Cova Gran de Santa Linya (SE Pyrenees). OSF. https://doi.org/10.17605/osf.io/zu4e3
        \item Sanchez-Martinez J., J. Martinez-Moreno, A. Benito-Calvo. 2026. Needle-thin: reconstructing on-edge microblade production and contextual integrity through lithic refitting in the Magdalenian of Cova Gran de Santa Linya (SE Pyrenees). Journal of Archaeological Science: Reports. 76. https://doi.org/10.1016/j.jasrep.2026.106129
        }
    }
\keyword{datasets}

\examples{
data(CovaGran7P)
head(cova.gran7p.fragments)
comment(cova.gran7p.fragments)
}

