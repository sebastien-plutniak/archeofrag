\name{Geelbek}
\docType{data}

\alias{Geelbek}
\alias{geelbek.connection}
\alias{geelbek.fragments}

\title{Dataset: Refitting relationships between lithic fragments from Geelbek dunes}
\description{
   Geelbek dunes is a Middle Stone Age open air site in South Africa. This dataset describes the refitting 'connection' relationships between bone, lithic, pottery, shell, and eggshell fragments found during surveys. A connection relationship refers to a physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{geelbek.connection}: connection relationships between fragments.
        \item\code{geelbek.fragments}: information about the fragments.
    }
}
\usage{data(Geelbek)}
\format{
    \itemize{
        \item\code{geelbek.connection} is a 1081x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in columns 'id1' and 'id2', respectively. 
        \item\code{geelbek.fragments} is a 1458x6 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item sector: locality in the dunes. 
            \item material: type of objects (bone, lithic, pottery, shell, eggshell)
            \item x: x coordinate of the location where the fragment was found (in metre)
            \item y: y coordinate of the location where the fragment was found (in metre)
            \item z: z coordinate of the location where the fragment was found (in metre)
        }
    }
    
}
\references{
    \itemize{
		   \item Nicholas J. Conard, Andrew W. Kandel,  Sébastien Plutniak. 2025. Refitting archaeological objects from the Geelbek Dunes Middle Stone Age site (South Africa). Zenodo, https://doi.org/10.5281/zenodo.15803288
		   \item Andrew W. Kandel, Nicholas J. Conard. 2012. Settlement patterns during the Earlier and Middle Stone Age around Langebaan Lagoon, Western Cape (South Africa). Quaternary International, 270, 15-29, http://doi.org/10.1016/j.quaint.2011.06.038
}
    }
\keyword{datasets}

\examples{
data(Geelbek)
head(geelbek.fragments)
}

