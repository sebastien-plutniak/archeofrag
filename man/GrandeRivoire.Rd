\name{GrandeRivoire}
\docType{data}

\alias{GrandeRivoire}
\alias{grande.rivoire.connection}
\alias{grande.rivoire.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Grande Rivoire site}
\description{
   Grande Rivoire is a Mesolithic rock shelter located in France. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. 
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{grande.rivoire.connection}: connection relationships between fragments.
        \item\code{grande.rivoire.fragments}: information about the fragments.
    }
}
\usage{data(GrandeRivoire)}
\format{
    \itemize{
        \item\code{grande.rivoire.connection} is a 71x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{grande.rivoire.fragments} is a 91x3 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item layer: sedimentary unit where the fragment was found.  Values are numbered following the approximately expected chrono-stratigraphic order (i.e. some units have no strict above/below relationships valid for everywhere in the site)
            \item spit: excavation spit where the fragment was found
        }
    }
    
}
\references{
    \itemize{
		\item  Angelin A., A. Bridault, J.-L. Brochier L. Chaix, L. Chesnaux, B. Marquebielle, L. Martin, P.-Y. Nicod, R. Picavet, D. Vannieuwenhuyse. 2016. 'The First Mesolithic in the French Alps: New data from La Grande Rivoire rockshelter (Vercors range, Isère, France)', Quaternary International, vol. 423, p. 193‑212, doi: 10.1016/j.quaint.2015.06.027 
        }
    }
\keyword{datasets}

\examples{
data(GrandeRivoire)
head(grande.rivoire.fragments)
}

