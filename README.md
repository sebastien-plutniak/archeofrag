
# Archeofrag

An R package for refitting and spatial analysis in archeology.

`Archeofrag` includes methods to analyse spatial units in archaeology from the relationships between refitting fragmented objects scattered in these units (e.g. stratigraphic layers). Graphs are used to model archaeological observations. The package is mainly based on the `igraph` package for graph analysis. Functions can: 1) create, manipulate, visualise, and simulate fragmentation graphs, 2) measure the cohesion and admixture of archaeological spatial units, and 3) characterise the topology of a specific set of refitting relationships. A series of published empirical datasets is included. This package is complemented by the `archeofrag.gui` R package, a companion GUI application available at <https://analytics.huma-num.fr/Sebastien.Plutniak/archeofrag/>.

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build
status](https://github.com/sebastien-plutniak/archeofrag/workflows/R-CMD-check/badge.svg)](https://github.com/sebastien-plutniak/archeofrag/actions)
[![codecov](https://codecov.io/gh/sebastien-plutniak/archeofrag/branch/master/graph/badge.svg)](https://app.codecov.io/gh/sebastien-plutniak/archeofrag)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4271900.svg)](https://doi.org/10.5281/zenodo.4271900)
[![SWH](https://archive.softwareheritage.org/badge/origin/https://github.com/cran/archeofrag/)](https://archive.softwareheritage.org/browse/origin/?origin_url=https://github.com/cran/archeofrag)
[![r-universe](https://sebastien-plutniak.r-universe.dev/badges/archeofrag)](https://sebastien-plutniak.r-universe.dev/ui#package:archeofrag)
[![CRAN
status](https://www.r-pkg.org/badges/version/archeofrag)](https://CRAN.R-project.org/package=archeofrag)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/archeofrag?color=brightgreen&.svg)](https://cran.r-project.org/package=archeofrag)
[![license](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.r-project.org/Licenses/GPL-3)
[![status](https://joss.theoj.org/papers/ff2007d87bd4c8460b265c69dc403316/status.svg)](https://joss.theoj.org/papers/ff2007d87bd4c8460b265c69dc403316)

- [**References**](#references)
- [**Installation**](#installation)
  - [R package](#r-package)
  - [GUI version](#gui-version)
- [**Community guidelines**](#community-guidelines)
  - [Reporting bugs](#reporting-bugs)
  - [Suggesting changes](#suggesting-changes)
- [**Building fragmentation graphs**](#building-the-fragmentation-graph)
- [**Alterating fragmentation
  graphs**](##alterating-fragmentation-graphs)
- [**Edge weighting, cohesion and admixture
  computation**](#edge-weighting-cohesion-and-admixture-computation)
- [**Testing layer formation hypotheses using simulated
  data**](#testing-layer-formation-hypotheses-using-simulated-data)
  - [Generating artificial fragmentation
    graphs](#generating-artificial-fragmentation-graphs)
  - [Testing hypotheses](#testing-hypotheses)
- [**Assessing spatial unit boundaries using similarity
  relationships**](#assessing-spatial-unit-boundaries-using-similarity-relationships)
- [**Characterising spatial units from their
  fragmentation**](#characterising-spatial-units-from-their-fragmentation)

# References

Details about the *archeofrag* / TSAR method were published in:

- Plutniak, S. 2022. “Archeofrag: an R package for Refitting and Spatial
  Analysis in Archaeology”, *Journal of Open Source Software*, 7 (75),
  p. 4335. DOI:
  [10.21105/joss.04335](https://doi.org/10.21105/joss.04335).
- Plutniak, S. 2022. “[Archeofrag: un package R pour les remontages et
  l’analyse spatiale en
  archéologie](https://rzine.gitpages.huma-num.fr/site/ressources/20220811_archeofrag_joss/)”,
  *Rzine*.
- Plutniak, S. 2022. “[L’analyse topologique des remontages
  archéologiques : la méthode TSAR et le package R
  archeofrag](http://www.prehistoire.org/offres/doc_inline_src/515/0-BSPF_2022_1_2e_partie_Correspondance_PLUTNIAK.pdf)”,
  *Bulletin de la Société préhistorique française*, 119 (1), p. 110–113.
- Plutniak, S. 2021. “[The Strength of Parthood Ties. Modelling Spatial
  Units and Fragmented Objects with the TSAR Method – Topological Study
  of Archaeological
  Refitting](https://hal.archives-ouvertes.fr/hal-03419952)” *Journal of
  Archaeological Science*, 136, p. 105501. DOI:
  [10.1016/j.jas.2021.105501](https://doi.org/10.1016/j.jas.2021.105501).

Use cases were published in:

- Plutniak, S., J. Caro, C. Manen 2023. “Four Problems for
  Archaeological Fragmentation Studies. Discussion and Application to
  the Taï Cave’s Neolithic Pottery Material (France)”, in A. Sörman et
  al., *Broken Bodies, Places and Objects. New Perspectives on
  Fragmentation in Archaeology*, London: Routledge, DOI:
  [10.4324/9781003350026-10](https://doi.org/10.4324/9781003350026-10).
- Plutniak, S. 2021. “[The Strength of Parthood Ties. Modelling Spatial
  Units and Fragmented Objects with the TSAR Method – Topological Study
  of Archaeological
  Refitting](https://hal.archives-ouvertes.fr/hal-03419952)” *Journal of
  Archaeological Science*, 136, p. 105501. DOI:
  [10.1016/j.jas.2021.105501](https://doi.org/10.1016/j.jas.2021.105501).

# Installation

## R package

The package can be installed from CRAN with:

``` r
install.packages("archeofrag")
```

The development version is available on GitHub and can be installed
with:

``` r
# install.packages("remotes")
remotes::install_github("sebastien-plutniak/archeofrag")
```

The (optional) determination of graph planarity in *Archeofrag* requires
the *RBGL* package, available through *Bioconductor*:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("RBGL")
```

## GUI version

The `archeofrag` package is complemented by a graphic-user interface
version, `archeofrag.gui` (also distributed as an R package). See a
[live demo
here](https://analytics.huma-num.fr/Sebastien.Plutniak/archeofrag/).
This GUI version, focusing on connection relationships only
(i.e. “proper refits”), make it easy to explore quickly and efficiently
a refitting dataset.

# Community guidelines

## Reporting bugs

If you encounter a bug, please fill an
[issue](https://github.com/sebastien-plutniak/archeofrag/issues) with
all the details needed to reproduce it.

## Suggesting changes

Suggestions of changes to `Archeofrag` are very welcome. These requests
may concern additional functions, changes to documentation, additional
examples, new features, etc. They can be made by filling an
[issue](https://github.com/sebastien-plutniak/archeofrag/issues) and,
even better, using pull requests and the [GitHub Fork and Pull
model](https://help.github.com/articles/about-pull-requests).

# Building the fragmentation graph

The `Archeofrag` package comes with several example data sets. One of
them, called “Liang Abu”, related to the pottery fragments found on the
surface and in the first two layers of the Liang Abu rock shelter,
Borneo, Indonesia. The data set contains three data frames:

- a table with information about the fragments (a unique identifier, the
  layer, their length and width, etc.),
- a table with the connection relationships between these fragments
  (each row contains the unique identifiers of two refitting fragments),
- a table with the similarity relationships between these fragments (two
  fragments are termed “similar” if they seem to come from the same
  object but do not have connecting edges.

The `make_frag_object` function builds objects with the class “frag”.
Frag objects are not required by the other `Archeofrag` functions,
however, using them ensures that the data are suitable for the next
steps of the analysis. The `make_cr_graph` function takes a frag object
and generates an `igraph` graph object representing the connection
relationships.

``` r
library(archeofrag)
abu.frag <- make_frag_object(cr=liangabu.connection, fragments=liangabu.fragments)
abu.g <- make_cr_graph(abu.frag)
```

# Visualisation and subgraph extraction

Several `Archeofrag` functions aims at making easy first examination of
the data. The `frag.relations.by.layers` function returns a matrix with
the number of relationships within and between spatial units (e.g.,
stratigraphic layer).

``` r
frag.relations.by.layers(abu.g, "layer")
#>    
#>      0  1  2
#>   0  4      
#>   1  0 18   
#>   2  0  3 31
```

The diagonal of the matrix gives the number of intra-layer
relationships, and the other values refer to inter-layer relationships.
Here, for example, there are 31 connection relationships within Layer 2,
and 3 connection relationships between Layers 1 and 2. No connection
relationship was found between the surface (“0”) and Layer 2.

The `frag.graph.plot` function generates a visual representation of the
graph:

``` r
par(mar=c(1, 0, 2, 0))
frag.graph.plot(abu.g, layer.attr="layer", main="All layers")
```

<img src="man/figures/README-manipulate-plot-abu-1.png" style="display: block; margin: auto;" />

The fragments are coloured by layer and the three inter-layer
relationships can be observed.

Let us now focus on layers 1 and 2. The `frag.get.layers.pair` function
allows the user to extract a pair of layers.

``` r
abu.g12 <- frag.get.layers.pair(abu.g, layer.attr="layer",
                                sel.layers=c("1", "2"))
```

This subgraph is drawn with the `frag.graph.plot` function:

``` r
par(mar=c(1, 0, 2, 0))
frag.graph.plot(abu.g12, layer.attr="layer", main="Layers 1 and 2")
```

<img src="man/figures/README-manipulate-plot-abu2-1.png" style="display: block; margin: auto;" />

The function has a different behaviour if applied to a fragmentation
graph with only two spatial units: the nodes are vertically localised to
reflect their location in the two spatial units. In addition, note that
standard `plot` arguments can be passed to the `frag.graph.plot`
function, e.g., the `main` argument to define the plot’s title.

The `frag.get.layers.pair` function has additional parameters to set the
minimum size of the connected fragments sets (`size.mini`) and to
extract only the sets of connected fragments which include relationships
between the two spatial units (`mixed.components.only`).

``` r
frag.get.layers.pair(abu.g, layer.attr="layer", sel.layers=c("1", "2"),
                     size.mini=2, mixed.components.only=TRUE)
#> IGRAPH bed61b7 UN-- 19 22 -- 
#> + attr: frag_type (g/c), name (v/c), layer (v/n), zmin (v/n), zmax (v/n), square (v/c), square.x (v/n), square.y (v/n), thickness
#> | (v/n), length (v/n), thickness.by.length (v/n), sherd.type (v/c), membership (v/n), type_relation (e/c)
#> + edges from bed61b7 (vertex names):
#>  [1] 27 --28  28 --835 835--836 25 --8   27 --366 27 --367 28 --367 366--367 27 --371 332--371 366--371 187--188 165--195 25 --195 195--196
#> [16] 195--197 196--198 195--204 196--204 197--204 198--204 188--250
```

Additionally, the `frag.get.layers` function can extract a set of
specified spatial unit(s), e.g., the refits within the first layer at
Liang Abu:

``` r
frag.get.layers(abu.g, layer.attr="layer", sel.layers="1")
#> $`1`
#> IGRAPH 2e63535 UN-- 23 18 -- 
#> + attr: frag_type (g/c), name (v/c), layer (v/n), zmin (v/n), zmax (v/n), square (v/c), square.x (v/n), square.y (v/n), thickness
#> | (v/n), length (v/n), thickness.by.length (v/n), sherd.type (v/c), type_relation (e/c)
#> + edges from 2e63535 (vertex names):
#>  [1] 392--408  123--124  301--302  313--314  435--441  477--478  25 --8    435--9999 441--9999 187--188  25 --195  195--196  195--197  196--198 
#> [15] 195--204  196--204  197--204  198--204
```

# Alterating fragmentation graphs

Two functions aimed at modifying a fragmentation graph, under
constraints specific to the analysis of archaeological fragmentation
graphs: \* `frag.observer.failure`, to remove relationships (edges) \*
`frag.graph.reduce` to remove fragments (nodes)

The `frag.observer.failure` function takes a fragmentation graph and
randomly removes a given proportion of edges. As suggested by its name,
the main purpose of this function is to simulate the failure of an
observer to determine relationships between fragments (a common
situation, considering the difficulty and time-consuming aspect of the
search for refits).

``` r
frag.observer.failure(abu.g12, likelihood=0.2)
```

The `frag.graph.reduce` function randomly removes a given number of
fragments (nodes) from a fragmentation graph. The `conserve.objects.nr`
parameter, if TRUE, ensures that the number of objects (i.e. connected
components) is the same in the resulting graph.

``` r
c(fragments = igraph::gorder(abu.g12), objects = igraph::components(abu.g12)$no)
abu.g12.red <- frag.graph.reduce(abu.g12, n.frag.to.remove = 5, conserve.objects.nr = TRUE)
c(fragments = igraph::gorder(abu.g12.red), objects = igraph::components(abu.g12.red)$no)
```

# Edge weighting, cohesion and admixture computation

Weighting the edges is a crucial step in the TSAR / `Archeofrag`
approach because it integrates the topological properties of the
fragmentation graph. The `frag.edges.weighting` function assigns a value
to each edge based on the topological properties of the vertices this
edge connects.

``` r
abu.g12 <- frag.edges.weighting(abu.g12, layer.attr="layer")
```

Then, the `frag.layers.cohesion` function is used to calculate the
cohesion value of each layer.

``` r
frag.layers.cohesion(abu.g12, layer.attr="layer")
#>     cohesion1 cohesion2
#> 1/2    0.3978    0.5928
```

These values determine the cohesion (self-adherence) of the spatial
units (here, layers) based on the distribution of the refitting
relationships. Note that the weighting of the edges is mandatory for the
computation of cohesion. Using the `frag.layers.cohesion` function on a
non-weighted fragmentation graph will give an error.

In addition to topological properties, the computation of edge weights
can optionally include other parameters, namely the morphometry of the
fragments and the distance between the location where they were found.
In the following example, the length of the pottery sherds is used as a
morphometric proxy:

``` r
abu.g12morpho <- frag.edges.weighting(abu.g12,
                                      layer.attr="layer",
                                      morphometry="length")
```

Using the morphometry parameter results, Layer 2 is more cohesive than
Layer 1:

``` r
frag.layers.cohesion(abu.g12morpho, layer.attr="layer")
#>     cohesion1 cohesion2
#> 1/2    0.3285    0.6646
```

In addition, the `frag.layers.admixture` function returns a value
quantifying the admixture between the two layers. Let us compare the
results obtained when the morphometry is used or not:

``` r
# topology-based weighting:
frag.layers.admixture(abu.g12, layer.attr="layer")
#> admixture 
#>    0.0094
# topology + morphometry weighting:
frag.layers.admixture(abu.g12morpho, layer.attr="layer")
#> admixture 
#>    0.0069
```

In this case, using the morphometry in the computation lowers the
admixture between layers 1 and 2 at Liang Abu.

# Testing layer formation hypotheses using simulated data

Simulation-based hypotheses can be tested by combining the functions
offered by `Archeofrag`.

## Generating artificial fragmentation graphs

The `frag.simul.process` function generates a pair of spatial units
containing fragmented objects with connection relationships within and
between these units. The next command creates two spatial units
populated with 20 initial objects (corresponding to the “connected
components” of a graph) which are fragmented into 50 pieces.

``` r
simul.g <- frag.simul.process(n.components=20, vertices=50)
```

This illustrates the simplest use of the `frag.simul.process` function,
which has several other parameters to control the features of the
simulation.

The number of initial spatial units is a crucial parameter, set using
the `initial.layers` parameter with “1” or “2”. This parameter
determines the method used to construct the graph and, accordingly, the
underlying formation process hypothesis.

If `initial.layers` is “1”, the fragmentation process is simulated
assuming that all the objects were originally buried in a single spatial
unit. The two clusters observed at the end of the process are due to
fragmentation and displacement.

1.  A single spatial unit is populated with the initial objects,
2.  the fragmentation process is applied,
3.  spatial units are assigned to the fragments,
4.  some fragments are moved as determined by the value of the
    `disturbance` parameter.

If `initial.layers` is “2”, it assumes that the objects were buried in
two different spatial units, which were later partially mixed due to
fragmentation and displacement:

1.  two spatial units are populated with the initial objects
    (components),
2.  the fragmentation process is applied,
3.  disturbance is applied.

The `vertices` and `edges` parameters are related: at least one of them
must be set, or both (only if `initial.layers` is set to 1). Note that
using both parameters at the same time increases the constraints and
reduces the number of possible solutions to generate the graph. When
there is no solution, an error occurs and a message suggests how to
change the parameters.

The `balance` argument determines the number of fragments in the smaller
spatial unit (**before** the application of the disturbance process).
The `components.balance` also determines the contents of the two spatial
units by affecting the distribution of the initial objects (components).
Note that this argument is used only when `initial.layers` is set to 2.

The `aggreg.factor` parameter affects the distribution of the sizes of
the components: this distribution tends to be more unequal when
`aggreg.factor` has values close to 1.

By default, fragments from two spatial units can be disturbed and moved
to another other spatial unit. However, the `asymmetric.transport.from`
can be used to move fragments from only one given spatial unit.

Finally, the `planar` argument determines if the generated graph has to
be planar or not (a graph is planar when it can be drawn on a plane,
without edges crossing). Note that the use of this argument requires the
RBGL package to be installed.

An example of a complete configuration of the function is:

``` r
frag.simul.process(initial.layers=1,
                   n.components=20,
                   vertices=50,
                   edges=40,
                   balance=.4,
                   components.balance=.4,
                   disturbance=.1,
                   aggreg.factor=0,
                   planar=TRUE,
                   asymmetric.transport.from="1")
```

## Testing hypotheses

The versatile `frag.simul.process` function can generate fragmentation
graphs under multiple hypotheses about the initial conditions (number of
initial objects, number of initial spatial units, etc.). Testing
measurements on observed empirical data against measurements made under
these hypotheses can determine the most likely initial conditions and
fragmentation process.

Here, this is illustrated by comparing measurements from Liang Abu
layers 1 and 2 with measurements from simulated data under two
hypotheses about the number of initial spatial units (e.g., layers),
using the `initial.layers` parameter with two values, namely one or two
initial layers.

A fragmentation graph is generated for each `initial.layers` value,
using the parameters observed in the Liang Abu layers 1 and 2
fragmentation graph. Setting the simulator is made easier by using the
`frag.get.parameters` function, which takes a graph and computes a
series of parameters that are returned as a list.

``` r
params <- frag.get.parameters(abu.g12, layer.attr="layer")
params
#> $n.components
#> [1] 28
#> 
#> $vertices
#> [1] 72
#> 
#> $edges
#> [1] 52
#> 
#> $balance
#> [1] 0.32
#> 
#> $components.balance
#> [1] 0.29
#> 
#> $disturbance
#> [1] 0.04
#> 
#> $aggreg.factor
#> [1] 0.7
#> 
#> $planar
#> [1] TRUE
```

``` r
# for H2:
test.2layers.g <- frag.simul.process(initial.layers=2,
                                    n.components=params$n.components,
                                    vertices=params$vertices,
                                    disturbance=params$disturbance,
                                    aggreg.factor=params$aggreg.factor,
                                    planar=params$planar)
# for H1:
test.1layer.g <- frag.simul.process(initial.layers=1,
                                   n.components=params$n.components,
                                   vertices=params$vertices,
                                   disturbance=params$disturbance,
                                   aggreg.factor=params$aggreg.factor,
                                   planar=params$planar)
```

Let us now generate not only one graph, but a large number of graphs to
statistically compare measurements in the empirical and simulated
graphs. The `frag.simul.process` function is set for the “two initial
layers” hypothesis and embedded into an *ad hoc* function:

``` r
run.test2 <- function(x){
  frag.simul.process(initial.layers=2, # note the different value
                     n.components=params$n.components,
                     vertices=params$vertices,
                     disturbance=params$disturbance,
                     aggreg.factor=params$aggreg.factor,
                     planar=params$planar)
}
```

The function is then executed a sufficient number of times:

``` r
test2.results <- lapply(1:100, run.test2)
```

The empirical values observed for Liang Abu layers 1 and 2 (red line)
can now be compared to the values measured in the simulated graph
generated under the hypothesis of two initial layers. This shows, for
example, that the empirical admixture value is slightly lower than the
simulated admixture values:

``` r
edges.res <- sapply(test2.results,
                    function(g) frag.get.parameters(g, "layer")$edges)
plot(density(edges.res), main="Edges")
abline(v=params$edges, col="red")
```

<img src="man/figures/README-simulator-test2-edges-1.png" style="display: block; margin: auto;" />

Similarly, the empirical admixture value is lower than the simulated
admixture values:

``` r
admix.res <- sapply(test2.results,
                    function(g) frag.layers.admixture(g, "layer"))
plot(density(admix.res), main="Admixture")
abline(v=frag.layers.admixture(abu.g12, "layer"), col="red")
```

<img src="man/figures/README-simulator-test2-admix-1.png" style="display: block; margin: auto;" />

Two functions (`frag.simul.compare` and `frag.simul.summarise`)
facilitate the execution of the analytical process described above on
the initial number of spatial units. The `frag.simul.compare` function
takes an observed fragmentation graph, generates two series of simulated
graphs corresponding to two hypotheses on the number of initial spatial
units (H1 for 1 initial spatial unit and H2 for two initial spatial
units), and returns a data frame of measurements made on each series
(including the edge count, weights sum, balance value, disturbance
value, admixture value, and cohesion values of the two spatial units).

``` r
compare.res <- frag.simul.compare(abu.g12, layer.attr="layer",
                                  iter=30, summarise=FALSE)
head(compare.res$h1.data)
#>   edges weightsum balance components.balance disturbance admixture cohesion1 cohesion2
#> 1    53  186.2886    0.32               0.32        0.04    0.0384    0.2964    0.6652
#> 2    56  271.7788    0.29               0.33        0.04    0.0142    0.1958    0.7900
#> 3    51  180.1365    0.30               0.27        0.04    0.0194    0.3071    0.6735
#> 4    53  223.0447    0.33               0.35        0.04    0.0166    0.3015    0.6819
#> 5    51  172.1320    0.35               0.36        0.01    0.0190    0.3476    0.6334
#> 6    52  218.8517    0.33               0.37        0.01    0.0012    0.2466    0.7522
```

For each of these parameters, the `frag.simul.summarise` function
facilitates the comparison between empirical observed values and
simulated values generated for H1 and H2.

``` r
frag.simul.summarise(abu.g12, layer.attr="layer",
                     compare.res$h1.data,
                     compare.res$h2.data)
#>                    H1 != H2? p.value Obs. value/H1 Obs. value/H2
#> edges                  FALSE    0.19        within        within
#> weightsum              FALSE    0.15        within        within
#> balance                FALSE    0.53        within        within
#> components.balance      TRUE       0         lower        within
#> disturbance            FALSE    0.16        within        within
#> admixture              FALSE    0.94         lower         lower
#> cohesion1               TRUE       0        higher        higher
#> cohesion2               TRUE       0         lower        within
```

This function returns a data frame with four columns, containing, for
each parameter studied:

1.  whether the series of H1 values are statistically different to the
    H2 series (Boolean),
2.  the p-value of the Wilcoxon test (numerical),
3.  whether the observed value is “within”, “higher”, or “lower” to the
    interquartile range of values for H1,
4.  whether the observed value is “within”, “higher”, or “lower” to the
    interquartile range of values for H2.

Note that the `frag.simul.compare` function can optionally be set to
execute and return the results of the `frag.simul.summarise` function.

# Assessing spatial unit boundaries using similarity relationships

Similarity relationships are, by construction, not part of the TSAR
method, which is based on the topological properties of connection
networks. However, since similarity relationships are more frequent in
archaeological empirical studies, the `Archeofrag` package includes
various functions to handle them. This section illustrates a method to
use similarity relationships using `Archeofrag` and R generic functions.

The `make_sr_graph` function takes a “frag” object and generates an
`igraph` similarity network.

``` r
# make a frag object and generate a similarity graph:
abu.frag <- make_frag_object(sr=liangabu.similarity, fragments=liangabu.fragments)
abu.sr <- make_sr_graph(abu.frag)
```

The `frag.relations.by.layers` function returns a table with the number
of similarity relationships in and between spatial units, e.g., in the
three top layers at Liang Abu:

``` r
# count of similarity relationships in and between layers:
simil.by.layers.df <- frag.relations.by.layers(abu.sr, "layer")
simil.by.layers.df
#>    
#>       0   1   2
#>   0  15        
#>   1   0 234    
#>   2   1  61 173
```

These values can be observed as percentages:

``` r
# percentage of similarity relationships in and between layers:
round(simil.by.layers.df / sum(simil.by.layers.df, na.rm=T) * 100, 0)
#>    
#>      0  1  2
#>   0  3      
#>   1  0 48   
#>   2  0 13 36
```

The count of similarity relationships can be converted into a measure of
dissimilarity: the dissimilarity between spatial units A and B is
calculated as 1 - admixture(A, B).

The higher the dissimilarity value, the more likely it is that these two
archaeological units correspond to different depositional units.
Theoretically, a spatial unit is expected to be more similar to those
near it.

In the case of stratigraphic layers, a layer is expected to be more
related to the layers directly above and below it. Considering a
stratigraphic sequence, adjacent and close layers in the sequence must
have lower statistical distances than distant layers. Consequently, it
is expected that the result of a hierarchical clustering computed on
this dissimilarity table would reflect the order of the layers. The
branches of the dendrogram are ordered alphanumerically according to
their label (following the stratigraphic order of the layers). Anomalies
are revealed when, despite this ordering constraint, the expected order
of superposition is not observed in the result.

This expected result is observed for Liang Abu surface and the first two
layers, suggesting an absence of significant disturbance and admixture.

``` r
# turn similarity into distance:
simil.dist <- max(c(simil.by.layers.df), na.rm=T) - simil.by.layers.df
simil.dist <- as.dist(simil.dist)
# hierarchical clustering:
clust.res <- hclust(simil.dist, method="ward.D2")
```

``` r
clust.res$labels <- as.character(factor(clust.res$labels, 
                     levels=c("0", "1", "2"),
                     labels=c("layer 0", "layer 1", "layer 2")))
plot(clust.res, hang=-1, axes=F, ann=F)
```

<div class="figure" style="text-align: center">

<img src="man/figures/README-similarity-dendr-fig-1.png" alt="Hierarchical clustering of the pottery layers in Liang Abu (distance: based on the number of similarity relationships; clustering method: Ward)."  />
<p class="caption">
Hierarchical clustering of the pottery layers in Liang Abu (distance:
based on the number of similarity relationships; clustering method:
Ward).
</p>

</div>

# Characterising spatial units from their fragmentation

The second aim of the TSAR method implemented in `Archeofrag` is to
characterise spatial units based on the topological properties of the
connection relationships between the fragments they contain. Although
this aspect is still a work in progress, some functions are already
implemented and will be illustrated using simulated data. The
archaeological interpretation of numerical values depends on the type of
material (lithic, pottery, etc.) and the completeness or incompleteness
of the objects under study and is not discussed here.

``` r
# simulate a fragmentation graph:
simul.g <- frag.simul.process(initial.layers=2,
                              n.components=20,
                              vertices=70,
                              balance=.45)
# extract the subgraph of each spatial unit:
simul.g1 <- frag.get.layers(simul.g, layer.attr="layer", sel.layers="1")[[1]]
simul.g2 <- frag.get.layers(simul.g, layer.attr="layer", sel.layers="2")[[1]]
```

In a graph, a cycle is a path in which only the first and last vertices
are repeated. The `frag.cycles` function searches for cycles in a graph
and returns the number of cycles found for different cycle lengths. The
`kmax` parameter determines the maximal length of the cycles to search
for. Let us compare the cycles found in the two spatial units of the
artificial graph:

``` r
rbind(
  "unit1" = frag.cycles(simul.g1, kmax=5),
  "unit2" = frag.cycles(simul.g2, kmax=5))
#>       3-cycles 4-cycles 5-cycles
#> unit1        8        2        0
#> unit2       19        8        1
```

The `frag.path.lengths` function returns the distribution of the path
lengths in the graph (i.e., the number of edges between each pair of
vertices). This function returns a vector whose first element is the
frequency of the paths of length 1, the second element is the frequency
of the paths of length 2, etc. If the `cumulative` parameter is set to
`TRUE`, the function returns the cumulative relative frequency of the
path lengths.

``` r
frag.path.lengths(simul.g1)
#> [1] 30  6
frag.path.lengths(simul.g2)
#> [1] 45 13
frag.path.lengths(simul.g2, cumulative=T)
#> [1] 1.0000000 0.2888889
```

In a graph, the shortest path between two vertices is the path including
the least number of edges. The diameter of a graph is its longest
shortest path. The `frag.diameters` function calculates the diameter of
each component of the graph and returns the frequency of the values. If
the `cumulative` parameter is set to `TRUE`, the function returns the
cumulative relative frequency of the diameters.

``` r
frag.diameters(simul.g1)
#> 1 2 
#> 4 6
frag.diameters(simul.g2)
#> 1 2 
#> 4 6
```
