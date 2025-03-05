\name{CuzoulSouth}
\docType{data}

\alias{CuzoulSouth}
\alias{cuzoul.south.connection}
\alias{cuzoul.south.fragments}

\title{Dataset: Refitting relationships between fauna fragments from the Cuzoul de Gramat site}
\description{
   The Cuzoul de Gramat site is a prehistoric site located in France. This dataset describes the refitting 'connection' relationships between bone fragments found during the excavation of Mesolithic layers from the 'South' sector of the site. Connection relationship refers to physical connection between two bone fragments that were part of the same object. 
    
    The dataset includes two tables:
    \itemize{
        \item\code{cuzoul.south.connection}: connection relationships between fragments.
        \item\code{cuzoul.south.fragments}: information about the fragments.
    }
}
\usage{data(CuzoulSouth)}
\format{
    \itemize{
        \item\code{cuzoul.south.connection} is a 135x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{cuzoul.south.fragments} is 233x9 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item layer: stratigraphic layer
            \item sub.layer: stratigraphic sub-layer
            \item thickness: thickness of the fragment (in centimetre)
            \item length: length of the fragment (in centimetre)
            \item surface: approximated surface of the fragment, product of the thickness by the length (in square centimeter)
            \item x: x coordinate of the grid square where the bone fragment was found (in centimetre)
            \item y: y coordinate of the grid square where the bone fragment was found (in centimetre)
            \item z: approximated z coordinate of the location where the bone fragment was found (in centimetre)
        }
    }
    
}
\references{
    \itemize{
		\item  Gardeur M. 2025. Bone refits from the Cuzoul de Gramat Mesolithic layers (archaeological site, France). Zenodo. https://doi.org/10.5281/zenodo.14975910
        }
    }
\keyword{datasets}

\examples{
data(CuzoulSouth)
head(cuzoul.south.fragments)
}

