\name{ChampParel3}
\docType{data}

\alias{ChampParel3}
\alias{champParel3.connection}
\alias{champParel3.fragments}

\title{Dataset: Refitting relationships between lithic fragments from the Champ Parel 3 site}
\description{
   Champ Parel 3 is an open-air Palaeolithic site located in France. This dataset describes the refitting 'connection' relationships between lithic fragments found during excavations. Connection relationship refers to physical connection between two fragments that were part of the same object. The material was associated to only one spatial unit.
    
    
    The dataset is composed of two tables:
    \itemize{
        \item\code{champParel3.connection}: connection relationships between fragments.
        \item\code{champParel3.fragments}: information about the fragments.
    }
    Metadata are accessible through the \code{comment()} function.
}
\usage{data(ChampParel3)}

\format{
    \itemize{
        \item\code{champParel3.connection} is a 2408x2 matrix. Each line describes a connection relationship between two fragments. Their unique identifiers are given in column 'id1' and in column 'id2', respectively. 
        \item\code{champParel3.fragments} is a 1747x8 data frame:
        \itemize{
            \item id:  fragment unique identifier 
            \item surface: estimated surface of the fragment computed as width by length (in square centimetres)
            \item weight: weight of the fragment (in gramms)
            \item width: width of the fragment (in millimeters)
            \item length: length of the fragment (in millimeters)
            \item x: x coordinate of the location where the lithic fragment was found (in metres)
            \item y: y coordinate of the location where the lithic fragment was found (in metres)
            \item z: z coordinate of the location where the lithic fragment was found (in metres)
        }
    }
    
}
\references{
    \itemize{
  		\item   Chadelle, J.-P. 2026. Archaeological data from the Champ Parel 3 (France) Upper Palaeolithic occupation (lithic material description and refits). https://doi.org/10.5281/zenodo.22813987.
  		\item   Chadelle, J.-P. 2000. Le gisement de Champ-Parel 3 à Bergerac (Dordogne, France). Observations taphonomiques sur un atelier de taille aurignacien en plein-air. Paléo, 12, p. 409-412. https://doi.org/10.3406/pal.2000.1612.
        \item Anderson, L. 2019. Essai de paléosociologie aurignacienne. Gestion des équipements lithiques et transmission des savoir-faire parmi les communautés établies dans le sud de la France. PhD thesis, Université de Toulouse-Mirail. https://doi.org/10.70675/9244f694zbd6cz4385zac49z458b02a759cd.
        }
    }
\keyword{datasets}

\examples{
data(ChampParel3)
head(champParel3.fragments)
comment(champParel3.fragments)
}

