\name{StCesaire1987}
\docType{data}

\alias{StCesaire1987}
\alias{st.cesaire.1987.connection}
\alias{st.cesaire.1987.fragments}

\title{Dataset: Refitting relationships between bone fragments from the Saint Césaire site, Palaeolithic levels}
\description{
La Roche a Pierrot is a Palaeolithic rock shelter located in Saint Césaire, France. This dataset describes the refitting 'connection' relationships between bone fragments found during the excavation of the Middle and Upper Palaeolithic levels led by Francois Leveque from 1976 to 1987. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset includes two tables:
    \itemize{
        \item\code{st.cesaire.1987.connection}: connection relationships between fragments.
        \item\code{st.cesaire.1987.fragments}: information about the fragments.
    }
}
\usage{data(StCesaire1987)}
\format{
    \itemize{
        \item\code{st.cesaire.1987.connection} is a 1188x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{st.cesaire.1987.fragments} is a 2006x9 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item layer: archaeological layer where the fragment was found, after revision
            \item layer.uncorrected: layer  where the fragment was found, as attributed by F. Leveque
            \item spit: minimal and maximal depth of the excavation spit in which the bone fragment was found 
            \item x: x coordinate of the location where the bone fragment was found (in centimetre)
            \item y: y coordinate of the location where the bone fragment was found (in centimetre)
            \item z: z coordinate of the location where the bone fragment was found (in centimetre)
            \item square: square where the fragment was found
            \item length: length of the fragment (in millimetre)
        }
    }
}
\details{
Refits were recorded by set of refitting fragments (and not by pair). Consequently, for the sets including more than two fragments, it is impossible to determine the exact distribution of the refitting relationships (if fragment A refits with fragments B and C, whether fragments B and C also refits together). So, in this dataset, the refits for those sets are parsimoniously modeled as chains (by consequence, the number of relationships might be underestimated).  
}

\references{
    \itemize{
		\item Morin E., S. Plutniak. 2025. 'Middle and Upper Palaeolithic Bone Refitting data from La Roche a Pierrot site (Saint-Cesaire, France), Excavations 1976-1987'. Zenodo. https://doi.org/10.5281/zenodo.15638561
        }
    }
\keyword{datasets}

\examples{
data(StCesaire1987)
head(st.cesaire.1987.fragments)
}


