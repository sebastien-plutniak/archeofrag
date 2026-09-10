\name{RueAugusteIsaac35}
\docType{data}

\alias{RueAugusteIsaac35}
\alias{rueAugusteIsaac35.connection}
\alias{rueAugusteIsaac35.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Rue Auguste Isaac 35 site}
\description{
    Rue Auguste Isaac 35 is an open-air Epipalaeolithic and Mesolithic site located in Lyon (France). This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. The material was associated to only one archaeologically relevant spatial unit.
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{rueAugusteIsaac35.connection}: connection relationships between fragments.
        \item\code{rueAugusteIsaac35.fragments}: information about the fragments.
    }
    Metadata are accessible through the \code{comment()} function.
}

\usage{data(RueAugusteIsaac35)}

\format{
    \itemize{
        \item\code{rueAugusteIsaac35.connection} is a 645x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in columns 'id1' and 'id2', respectively. 
        \item\code{rueAugusteIsaac35.fragments} is a 1063x12 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item square: square of the excavation grid where the fragment was found
            \item square.spit: archaeological spit within the square
            \item period: chronological period attributed to the lithic object 
            \item state: surface state of the object (burnt, heated, thermofracted)
            \item raw.material: raw material of the lithic fragment
            \item volume: estimated volume of the fragment computed as width by thickness by length (in cubic centimetres)
            \item width: width of the fragment (in millimeters)
            \item length: length of the fragment (in millimeters)
            \item thickness: thickness of the fragment (in millimeters)
            \item x: x coordinate of the location where the lithic fragment was found (in metres)
            \item y: y coordinate of the location where the lithic fragment was found (in metres)
            \item z: z coordinate of the location where the lithic fragment was found (in metres)
        }
    }
    
}
\references{
    \itemize{
	  	\item Pasty J.-F. 2026. Fouille du 35 rue Auguste Isaac (Lyon, France). Données lithiques des occupations épipaléolithiques et mésolithiques, https://doi.org/10.5281/zenodo.22663886
	  	\item Treffort J.-M. (eds). 2017. Lyon 9e (69), 35 rue Auguste Isaac - tranche 3, Rapport de fouille. Bron: INRAP ARA.
        }
    }
\keyword{datasets}

\examples{
data(RueAugusteIsaac35)
head(rueAugusteIsaac35.fragments)
comment(rueAugusteIsaac35.fragments)
}

