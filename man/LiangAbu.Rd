\name{LiangAbu}
\docType{data}

\alias{LiangAbu}
\alias{df.cr}
\alias{df.sr}
\alias{fragments.info}



\title{Dataset: Archeological relationships between pottery fragments in Liang Abu}
\description{
    Liang Abu is an archaeological site in East Kalimantan, Indonesia. This dataset describes the relationships between pottery fragments found during   excavations (2009-2012). Two types of relationships are defined.
    \itemize{
        \item A connection relationship refers to a physical connection between two fragments that were part of the same object. 
        \item A similarity relationship between fragments is defined if there is an acceptable likelihood that those fragments were part of the same object.
    }
    
    The dataset is composed of three tables, \code{cr.df}, \code{sr.df}, \code{fragments.info}.
    \itemize{
        \item\code{cr.df}: "connection" relationships between fragments.
        \item\code{sr.df}: "similarity" relationships between fragments.
        \item\code{fragments.info}: contextual information concerning each fragment.
    }
}
\usage{data(LiangAbu)}
\format{
    \itemize{
        \item\code{cr.df} is a 56x2 matrix. Each line describes a connection relationship between two fragments. There respective unique identifiers are given in column "frg_id1" and in column "frg_id2". 
        \item\code{sr.df} is a 147x2 matrix. Column "frg_id" gives a fragment unique identifier, column "su_id" gives a unique identifier for the group of similar fragments it belongs to (similarity unit).
        \item\code{fragments.info} is 177x8 data frame:
        \itemize{
            \item frg_id: unique fragment identifier 
            \item layer:  stratigraphic layer
            \item zmin: minimal depth in centimetres where the fragment was found
            \item zmax: maximal depth in centimetres where the fragment was found
            \item square: square where the fragment was found
            \item sherd.type: type of pottery sherd
            \item thickness: thickness of the fragments in millimetres
            \item length: length of the fragments in millimetres
        }
    }
    
}
\references{
    \itemize{
        \item Plutniak, Sebastien, "Refitting pottery fragments from the Liang Abu rockshelter, Borneo", Zenodo, doi: 10.5281/zenodo.4719578.
        \item Plutniak, Sebastien, Astolfo Araujo, Simon Puaud, Jean-Georges Ferrie, Adhi Agus Oktaviana, Bambang Sugiyanto, Jean-Michel Chazine et Francois-Xavier Ricaut. 2015. "Borneo as a half empty pot: Pottery assemblage from Liang Abu, East Kalimantan, Quaternary International, doi: 10.1016/j.quaint.2015.11.080.}
        }
\keyword{datasets}

\examples{
data(LiangAbu)
head(fragments.info)
}

