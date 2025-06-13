\name{Grotte16}
\docType{data}

\alias{Grotte16}
\alias{grotte16.connection}
\alias{grotte16.fragments}

\title{Dataset: Refitting relationships between bone fragments from the Grotte 16 site, Palaeolithic levels}
\description{
Grotte 16 is a Palaeolithic site located in Cénac-et-Saint-Julien, France. This dataset describes the refitting 'connection' relationships between bone fragments found during the excavation of the Middle and Upper Palaeolithic levels led by Francois Leveque from 1976 to 1987. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset includes two tables:
    \itemize{
        \item\code{grotte16.connection}: connection relationships between fragments.
        \item\code{grotte16.fragments}: information about the fragments.
    }
}
\usage{data(Grotte16)}
\format{
    \itemize{
        \item\code{grotte16.connection} is a 102x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{grotte16.fragments} is a 164x11 data frame:
        \itemize{
            \item id: fragment unique identifier 
            \item layers.at.excavation: archaeological layer 
            \item layer.uncorrected: layer where the fragment was found, as 
            \item layers.summarized: layer where the fragment was found, as attributed by F. Leveque
            \item layer.simplified: layer where the fragment was found, as attributed by F. Leveque
            \item square: square of the excavation grid where the bone fragment was found 
            \item square.x: X label of the excavation grid's square where the bone fragment was found 
            \item square.y: Y label of the excavation grid's square where the bone fragment was found 
            \item x: x coordinate of the location where the bone fragment was found (in centimetre)
            \item y: y coordinate of the location where the bone fragment was found (in centimetre)
            \item z: z coordinate of the location where the bone fragment was found (in centimetre)
            \item size: size class (in centimetre)
        }
    }
}


\details{
Refits were recorded by set of refitting fragments (and not by pair). Consequently, for the sets including more than two fragments, it is impossible to determine the exact distribution of the refitting relationships (if fragment A refits with fragments B and C, whether fragments B and C also refits together). So, in this dataset, the refits for those sets are parsimoniously modeled as chains (by consequence, the number of relationships might be underestimated).  
}

\references{
    \itemize{
        \item Dancette C., E. Discamps, S. Plutniak. 2025. 'Bone refits from the Grotte XVI Pleistocene Faunal Assemblage (Cénac-et-Saint-Julien, France)'. Zenodo. https://doi.org/10.5281/zenodo.15655628
        }
    }
\keyword{datasets}

\examples{
data(Grotte16)
head(grotte16.fragments)
}


