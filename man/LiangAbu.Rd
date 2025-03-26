\name{LiangAbu}
\docType{data}

\alias{LiangAbu}
\alias{liangabu.connection}
\alias{liangabu.similarity}
\alias{liangabu.fragments}

\title{Dataset: Archeological relationships between pottery fragments in Liang Abu}
\description{
    Liang Abu is an archaeological site in East Kalimantan, Indonesia. This dataset describes the relationships between pottery fragments found during   excavations (2009-2012). Two types of relationships are defined.
    \itemize{
        \item A connection relationship refers to a physical connection between two fragments that were part of the same object. 
        \item A similarity relationship between fragments is defined if there is an acceptable likelihood that those fragments were part of the same object.
    }
    
    The dataset is composed of three tables:
    \itemize{
        \item\code{liangabu.connection}: "connection" relationships between fragments.
        \item\code{liangabu.similarity}: "similarity" relationships between fragments.
        \item\code{liangabu.fragments}: information about the fragments.
    }
}
\usage{data(LiangAbu)}
\format{
    \itemize{
        \item\code{liangabu.connection} is a 56x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id', respectively. 
        \item\code{liangabu.similarity} is a 147x2 matrix. Column 'id' gives a fragment unique identifier, column 'su.id' gives a unique identifier for the group of similar fragments it belongs to (similarity unit).
        \item\code{liangabu.fragments} is 177x11 data frame:
        \itemize{
            \item id: fragment unique identifier
            \item layer:  stratigraphic layer
            \item zmin: minimal depth in centimetres where the fragment was found
            \item zmax: maximal depth in centimetres where the fragment was found
            \item square: grid square where the fragment was found
            \item square.x: x coordinate in centimetres of the middle of the grid square
            \item square.y: y coordinate in centimetres of the middle of the grid square
            \item thickness: thickness of the fragments in millimetres
            \item length: length of the fragments in millimetres
            \item thickness.by.length: product of the thickness by the length
            \item sherd.type: type of pottery sherd
        }
    }
    
}
\references{
    \itemize{
		\item Plutniak, Sebastien, "The Strength of Parthood Ties. Modelling Spatial Units and Fragmented Objects with the TSAR Method - Topological Study of Archaeological Refitting", Journal of Archaeological Science, vol. 136, 105501, doi: 10.1016/j.jas.2021.105501.
        \item Plutniak, Sebastien, "Refitting pottery fragments from the Liang Abu rockshelter, Borneo", Zenodo, doi: 10.5281/zenodo.4719578.
        \item Plutniak, Sebastien, Astolfo Araujo, Simon Puaud, Jean-Georges Ferrie, Adhi Agus Oktaviana, Bambang Sugiyanto, Jean-Michel Chazine et Francois-Xavier Ricaut. 2015. "Borneo as a half empty pot: Pottery assemblage from Liang Abu, East Kalimantan, Quaternary International, doi: 10.1016/j.quaint.2015.11.080.
        }
\keyword{datasets}

\examples{
data(LiangAbu)
head(liangabu.fragments)
}

