---
title: 'Archeofrag: an R package for refitting and stratigraphic analysis in archaeology'
tags:
  - R
  - archaeology
  - archeology
  - refitting
  - refits
  - stratigraphy
  - stratigraphic analysis
authors:
  - name: Sébastien Plutniak
    orcid: 0000-0002-6674-3806
    affiliation: 1
affiliations:
 - name: TRACES Laboratory, Toulouse, France
   index: 1
date: 30 November 2020
bibliography: archeofrag-paper.bib
---

# Summary

The study of archaeological stratigraphy involves multiple disciplines collaborating to identify relevant discontinuities along an archaeological sequence: field observation, geoarchaeology, and sedimentology, for example. The study of archaeological “refits”, namely the identification of material fragments belonging to the same object, is also involved in this collaborative study. Refitting analysis has several aims: 1) to reconstruct objects; 2) to determine technical sequences (e.g. stone tool manufacture); and 3) to determine the reliability of stratigraphic layers and their possible admixture due to post-depositional processes. This analysis has long been used for the latter aim, benefiting from multiple methodological improvements (for an overview, see @CzieslaEtal1990, @SchurmansDebie2007). 
These methods have relied on the comparison between the number of refits between different layers and within these layers. `Archeofrag` is an R package [@Rcoreteam2020]  implementing graph theory to model the topology of the relations between fragments [@Plutniak2020archeofrag]. This renewed approach distinguishes between ambiguous cases (\autoref{fig:scheme}) and results in a more accurate evaluation of the reliability of stratigraphic layers.

![**Three examples (a-c) of two layers with internal refitting (n=6) and inter-layers refitting (n=2).** Although the numbers of relations are equal in all examples, their archaeological interpretation would be very different. The distinction between the two layers would be considered as relevant in (a); relevant with higher confidence about the initial locations of objects in (b) and; doubtful in (c).\label{fig:scheme}](archeofrag-paper.png)

The package has three main sets of functions, to:

1. Read, manipulate, and simulate fragmentation graphs
2. Measure the cohesion and admixture of layers
3. Characterize the topology of a specific set of refitting relationships

# Statement of need
The use of R in archaeology has increased slowly, albeit constantly, during the last two decades. However, the development of R packages for specific archaeological needs is an even more recent phenomenon. Only a few packages are available for stratigraphic analysis in archaeology:

* [`stratigraphr`](https://github.com/joeroe/stratigraphr): package in its early development phase to visualize and analyze stratigraphies as directed graphs (Harris matrices)
* [`tabula`](https://github.com/nfrerebeau/tabula): generic package to visualize remain counts which can also be used to compare layers [@Frerebeau2019]
* [`recexcavAAR`](https://github.com/ISAAKiel/recexcavAAR): package for 3D reconstruction and analysis of excavations [@SchmidSerbe2017]
* `ArchaeoPhases`: package for Bayesian analysis of archaeological chronology to define stratigraphic phases  [@PhilippeVibet2020]

`Archeofrag` complements this series of packages for stratigraphic analysis with a specific focus on refitting.

# Features
Archeofrag mainly draws on the `igraph` library for graph analysis [@CsardiNepusz2006] and also relies on some functions from the `RBGL` package [@CareyEtal2020]. It comes with an example data set containing refitting data on the pottery found during excavations at Liang Abu rock shelter, in Borneo [@Plutniak2016QI].

## Reading, manipulating, and simulating fragmentation graphs
`Archeofrag` is intended to be used with two sources of data, namely the user’s empirical data and artificially generated data using its simulation function. User’s data must be formatted into two tables. The first table contains the identifier of each fragment, its layer, and additional information. The second table is an edge list where a line contains two identifiers denoting a relationship between two fragments.

Other functions generate summary statistics about the fragmentation graph and extract specific sub-graphs (by layer, by component size, etc.).

The simulation function generates connected fragments scattered within two layers. It can be set with multiple parameters (number of initial objects, number of fragments, number of relationships, number of fragments and relationships, the balance between layers, etc.). It can also be constrained to generate only planar graphs since this corresponds to the fragmentation in some specific archaeological contexts (e.g. simple shapes pottery, small sets of refits). However, the run time of this function is doubled when this constraint is used.


## Measuring the cohesion and admixture of two layers
Measuring layer admixture from refits between and within layers is the first main function of the `Archeofrag` package. It follows a three-step procedure, implemented in three related functions:

1. Weighting the edges
2. Measuring the internal cohesion of each layer
3. Measuring the admixture of the two layers

Cohesion and admixture values range between 0 and 1, with 0 for low layer reliability (internal cohesion) or low admixture.

## Fragmentation patterns, technology, human behavior
The second main function of `Archeofrag` is to characterize layers from the topological properties of the refitting relationship network they contain. Several functions are provided for this purpose, selected for their relevance in the archaeological context, namely: cycle count, path length, and component diameter (a component is related to an initial object).

The archaeological interpretation of the numerical values depends on the type of object (lithic, pottery, etc.) and their completeness or incompleteness. These values can suggest specific behaviors related to the production or use of the objects (intentional breaking), and post-depositional processes (natural breaking, scattering). This aspect of the `Archeofrag` method will be further developed.

# Testing hypotheses: layer formation 
Combining the functions offered by `Archeofrag` can perform simulation-based hypotheses testing. The simulation function can be used with two modes to generate pairs of layers with fragmented objects within them. In the first mode, the fragmentation process is simulated assuming that all the objects were originally buried in a single layer and that two clusters were formed (i.e. layers) due to fragmentation and displacement. The second mode assumes that the objects were buried in two different layers before fragmentation and displacement. Testing each hypothesis against observational data determines the most likely site formation scenario.

# Acknowledgements
I thank Luce Prignano and Oliver Nakoinz for their valuable comments during the development of this package, which was finalised at the *Institut für Ur- und Frühgeschichte of Kiel*, with the support of a “Short-term research grant” from the *Deutscher Akademischer Austausch Dienst* (DAAD).

# Resources
Archeofrag is available on [CRAN](https://cran.r-project.org/package=archeofrag) and the code of the development version is available on  [Github](https://github.com/sebastien-plutniak/archeofrag/). A [Shiny application](https://analytics.huma-num.fr/Sebastien.Plutniak/archeofrag/)  demonstrates the package.


# References
