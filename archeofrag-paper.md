---
title: 'Archeofrag: an R package for refitting and spatial analysis in archaeology'
tags:
  - R
  - archaeology
  - archeology
  - refitting
  - refits
  - stratigraphy
  - stratigraphic analysis
  - spatial analysis
authors:
  - name: Sébastien Plutniak
    orcid: 0000-0002-6674-3806
    affiliation: 1
affiliations:
 - name: TRACES Laboratory, Toulouse, France
   index: 1
date: 8 March 2022 
bibliography: archeofrag-paper.bib
output: 
  pdf_document: 
    toc: yes
    toc_depth: 4
    number_sections: yes
---



# Summary

Distinguishing between spatial entities is fundamental in archaeology since archaeologists deal with spatial phenomena at multiple scales of analysis. During an excavation, objects are discovered within various types of spatial units e.g., stratigraphic layers, pits, hearths and houses. Spatial units are far from being raw data, and the identification and determination of their boundaries is the result of conjoint lines and methods of investigation, to name only a few: field observations, geoarchaeology, sedimentology, and the study of archaeological “refits”.
Refitting fragments belonged to the same object at some moment in the past. 
More precisely, archaeologists deduce former connection relationships from the symmetry and the possibility of contact of significantly large surface areas from two fragments, which can be physically adjusted (the fragments “refit”).
Here, “connection” is used as a shorthand to refer to the connection relationship that existed in the past between two areas of an object before they were broken into fragments.
Archaeological refitting analysis has several aims:  

1. to reconstruct objects, 
2. to determine technical sequences (e.g., stone tool manufacture), and 
3. to determine the reliability of spatial units and their possible admixture due to pre- and post-depositional processes. 

This analysis has long been used for the latter aim, benefiting from multiple methodological improvements (for an overview, see @CzieslaEtal1990, @SchurmansDebie2007). 
These methods have relied on the comparison between the number of refits between different spatial units and within these units. 
However, it has been demonstrated that considering the number of refits without considering their topology can lead to misleading interpretations.
A method, coined TSAR “Topological Study of Archaeological Refitting”, was developed to overcome this issue using  graph theory to model the topology of the relations between fragments (@Plutniak2021jas, @Plutniak2022bspf). 
This renewed approach distinguishes between ambiguous cases (\autoref{fig:scheme}), and is much more robust and less sensitive to the lack of information than count-based methods, thus resulting in a more accurate evaluation of the reliability of the boundaries between spatial units.
`Archeofrag` is an R package [@Rcoreteam2021] implementing the TSAR method [@Plutniak2022archeofrag].

![**Three examples (a-c) of two layers with internal refitting (n=6) and inter-layer refitting (n=2).** Although the numbers of relationships are equal in all examples, their archaeological interpretation are very different: relevant distinction between the two layers in (a); relevant distinction with higher confidence about the fragmented objects’ initial location in (b); doubtful distinction between layers in (c).\label{fig:scheme}](archeofrag-paper.png)

# Statement of need
The use of R in archaeology has increased slowly albeit constantly, during the last two decades. However, the development of R packages for specific archaeological needs is an even more recent phenomenon. Only a few packages are available for spatial analysis in archaeology, notably for stratigraphic analysis:

* [`stratigraphr`](https://github.com/joeroe/stratigraphr): package in its early development phase to visualise and analyse stratigraphies as directed graphs (Harris matrices).
* [`tabula`](https://github.com/nfrerebeau/tabula): generic package to visualise remain counts, which can also be used to compare layers [@Frerebeau2019].
* [`recexcavAAR`](https://github.com/ISAAKiel/recexcavAAR): package for 3D reconstruction and analysis of excavations [@SchmidSerbe2017].
* [`archaeoPhases`](https://doi.org/10.18637/jss.v093.c01): package for Bayesian analysis of archaeological chronology to define stratigraphic phases [@PhilippeVibet2020].

`Archeofrag` complements this series of packages with a specific focus on refitting.

# Package overview
`Archeofrag` mainly uses the `igraph` library for graph analysis [@CsardiNepusz2006] and also relies on some functions from the `RBGL` package [@CareyEtal2020]. 
It comes with an example data set [@Plutniak2021aburefits] containing refitting data on the pottery found during excavations at Liang Abu rock shelter, in Borneo [@Plutniak2016QI].

`Archeofrag` has six main sets of functions:

1. **Data management**: create, check, and transform fragmentation graphs 
(including edge weighting based on the topological properties of the vertices and optionally, on the size of the fragments and the distance between the locations where they were discovered during excavation)
2. **Visualisation**: represent fragmentation graphs as node-and-edge diagrams
3. **Boundary-related statistics**: count the relationships within and between two spatial units, measure their cohesion and the admixture values
4. **Spatial unit-related statistics**: characterise the topology of a specific set of refitting relationships (e.g., a layer) with several measurements
5. **Simulation**: generate simulated fragmentation graphs, simulate their alteration (missing data); compare an empirical graph with similar simulated graphs and return results in a convenient way
6. **Similarity analysis**: in addition to the topological analysis of refits, functions are available to analyse similarity relationships, which are determined between fragments considered as sharing enough common features (motif, clay, inclusions, etc.) to state they are (and were) parts of the same initial object.

## Data management
`Archeofrag` is intended to be used with two sources of data, namely the user’s empirical data and artificially generated data using its simulation function. User’s data must be split into different tables:

* **Fragment table**: each line contains the unique identifier of a fragment, its spatial unit, and optionally additional information.
* **Connection table**: an edge list with the identifiers of two connected fragments by line (e.g., \autoref{tab:record}).
* **Similarity table** (optionally): each line includes the unique identifier of a fragment and the identifier of a set of similar fragments it belongs to.


| id | fragment 1 | fragment 2 |
|----|------------|------------|
| 1  | A          | B          |
| 2  | C          | D          |
| 3  | D          | E          |
| 4  | E          | C          |

: Recording of connection relationships between fragments (illustrated by the examples given in  \autoref{fig:scheme}).\label{tab:record}

The package includes functions to generate summary statistics about the fragmentation graph and extract specific sub-graphs (by layer, by component size, etc.).

## Visualisation of fragmentation graphs

The fragmentation graphs are visualised as node-and-edge diagrams. 
For graphs with only two spatial units and connection relationships, the location of the nodes in the upper and the lower part of the plot are based on their spatial unit.

## Boundary-related statistics: measuring the cohesion and admixture of two spatial units
Evaluating the consistency of spatial units and their division from refits is the first aim of the `Archeofrag` package. Evaluation follows a three-step procedure, implemented in three related functions:

1. Weighting the edges. Three parameters can be combined: the topology of the connection relationships (mandatory), the size of the two connected fragments (optional), and the spatial distance between the location where they were found (optional). The optional parameters implement the hypothesis that two large fragments found far from each other suggest more disturbance in the site than two small fragments found very close together [see an application and details in @CaroEtal2022].
2. Measuring the internal cohesion of each spatial unit (intuitively, how they are “self-adherent” to themselves)
3. Measuring the admixture of two spatial units (a summary statistic based on two cohesion values)

Cohesion and admixture values are computed by pairs of spatial units.
Results for cohesion measurements range between [0;1], with values towards 0 for low cohesion and towards 1 for high cohesion, with  their sum never being superior to 1 for a pair of layers.  Results for admixture measurements range between [0;1].

## Spatial unit-related statistics: fragmentation patterns, technology, human behaviour
The second aim of the TSAR method implemented in `Archeofrag` is to characterise spatial units based on the topological properties of the connection relationships between the fragments they contain.
Several functions are provided for this purpose, selected for their relevance in the archaeological context, namely cycle count, path length, and component diameter (a component is related to an initial object).

The archaeological interpretation of the numerical values depends on the type of object (lithic, pottery, etc.) and their completeness or incompleteness. These values can suggest specific behaviours related to the production or use of the objects (intentional breaking), and post-depositional processes (natural breaking, scattering). This aspect of the TSAR method will be further developed in the future.
 
## Simulation of fragmentation graphs
The simulation function generates connected fragments scattered within one or two spatial units (see @Plutniak2021jas for details). It can be set with multiple parameters:

* Number of initial objects or fragments.
* Final number of fragments.
* Final number of connection relationships.
* Proportion of fragments in each spatial unit, before post-depositional processes.
* Proportion of components in each spatial unit.
* Proportion of fragments likely to move from a spatial unit to the other one.
* Whether to only disturb the fragments from spatial unit 1 or spatial unit 2.
* When applying fragmentation, increase the likelihood for objects with more fragments being selected.
* Number of initial hypothetical spatial unit.
* Whether generating only planar graphs.

Generating only planar graphs is interesting since this corresponds to the fragmentation observed in some specific archaeological contexts (e.g., pottery with simple shapes, small sets of refits). However, the run time of this function is doubled when this constraint is used.

# Resources and examples
`Archeofrag` is available on [CRAN](https://cran.r-project.org/package=archeofrag) and the code of the development version is available on [Github](https://github.com/sebastien-plutniak/archeofrag/). 
A [Vignette](https://cran.r-project.org/web/packages/archeofrag/vignettes/archeofrag-vignette.html) and a [Shiny application](https://analytics.huma-num.fr/Sebastien.Plutniak/archeofrag/) demonstrate the package.

At the time of this submission, `Archeofrag` has been applied to two archaeological sites:

* Liang Abu, Indonesia (@Plutniak2021jas, @PlutniakEtal2022jica)
* Taï cave, France (@CaroEtal2022)


# Acknowledgements
I thank Luce Prignano, Claire Manen, Joséphine Caro, and Oliver Nakoinz for their valuable comments during the development of this package, which was finalised at the *Institut für Ur- und Frühgeschichte of Kiel*, with the support of a “Short-term research grant” from the *Deutscher Akademischer Austausch Dienst* (DAAD).


# References
