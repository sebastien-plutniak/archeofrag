\name{StCesaire2024}
\docType{data}

\alias{StCesaire2024}
\alias{st.cesaire.2024.connection}
\alias{st.cesaire.2024.fragments}

\title{Dataset: Refitting relationships between bone fragments from the Saint Césaire site, Palaeolithic levels}
\description{
La Roche a Pierrot is a Palaeolithic rock shelter located in Saint Césaire, France. This dataset describes the refitting 'connection' relationships between bone fragments found during the excavation of the Middle and Upper Palaeolithic levels carried out since 2013. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset includes two tables:
    \itemize{
        \item\code{st.cesaire.2024.connection}: connection relationships between fragments.
        \item\code{st.cesaire.2024.fragments}: information about the fragments.
    }
}
\usage{data(StCesaire2024)}
\format{
    \itemize{
        \item\code{st.cesaire.2024.connection} is a 116x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{st.cesaire.2024.fragments} is a 218x9 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item layer: archaeological layer where the fragment was found, after revision
            \item layer.uncorrected: layer  where the fragment was found
            \item spit: excavation spit in which the bone fragment was found 
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
		\item Morin E., S. Plutniak. 2025. 'Middle and Upper Palaeolithic Bone Refitting data from La Roche a Pierrot site (Saint-Cesaire, France), Excavations 2013-2024'. Zenodo. https://doi.org/10.5281/zenodo.15638691
        }
    }
\keyword{datasets}

\examples{
data(StCesaire2024)
head(st.cesaire.2024.fragments)
}


