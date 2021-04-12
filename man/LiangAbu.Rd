\name{LiangAbu}
\docType{data}

\alias{LiangAbu}
\alias{df.cr}
\alias{df.sr}
\alias{fragments.info}



\title{Dataset: Archeological relationships between pottery fragments in Liang Abu}
\description{
    Liang Abu is an archaeological site in East Kalimantan, Indonesia. This data set describes the relations between pottery fragments found during the excavations (2009-2012). Two types of relations are defined.
    \itemize{
        \item A connection relation refers to a physical connection between two fragments that were part of the same object. 
        \item A similarity relation between fragments is defined if there is an acceptable likelihood that those fragments were part of the same object.
    }
    
    The data set is composed of three tables, \code{cr.df}, \code{sr.df}, \code{fragments.info}.
    \itemize{
        \item\code{cr.df}: "connection" relations between fragments.
        \item\code{sr.df}: "similarity" relations between fragments.
        \item\code{fragments.info}: contextual informations concerning each fragment.
    }
}
\usage{data(LiangAbu)}
\format{
    \itemize{
        \item\code{cr.df} is a 56x2 matrix. Each line describes a connection relationship between two fragments. There respective unique identifiers are given in column "frg_id1" and in column "frg_id2". 
        \item\code{sr.df} is a 147x2 matrix. Column "frg_id" gives a fragment unique identifier, column "su_id" gives a unique identifier for the group of similar fragments it belongs (similarity unit).
        \item\code{fragments.info} is 177x8 data frame:
        \itemize{
            \item frg_id: fragment unique identifier 
            \item layer:  stratigraphic layer
            \item zmin: minimal depth in centimeter where the fragment was found
            \item zmax: maximal depth in centimeter where the fragment was found
            \item square: square where the fragment was found
            \item sherd.type: type of pottery sherd
            \item thickness: thickness of the fragments in millimeter
            \item length: length of the fragments in millimeter
        }
    }
    
}
\references{Plutniak, Sebastien, Astolfo Araujo, Simon Puaud, Jean-Georges Ferrie, Adhi Agus Oktaviana, Bambang Sugiyanto, Jean-Michel Chazine et Francois-Xavier Ricaut. 2015. "Borneo as a half empty pot: Pottery assemblage from Liang Abu, East Kalimantan, Quaternary International, doi: 10.1016/j.quaint.2015.11.080.}
\keyword{datasets}

\examples{
data(LiangAbu)
head(fragments.info)
}

