
# archeofrag 1.2.1
Released: 

* Add 'Grande Rivoire 1st Meso' and 'Grande Rivoire 2nd Meso' datasets. Remove the 'Grande Rivoire' dataset.
* Code optimisation: add internal sub-functions `.fragments.balance()` and `.components.balance()`. The first is now used both in `frag.get.parameters()` and `frag.graph.reduce()` (in the last case, to avoid calling `frag.get.parameters()` in `frag.graph.reduce()`).

# archeofrag 1.2.0
Released: 2025-03-27

* Add 'Grotta di Fumane' and 'Eaton' datasets.
* Add comments to fragments dataset including the following metadata: site name, type of material, and chronological period.
* In `frag.graph.reduce()` add parameters to: preserve the proportion of fragments in the first and second spatial units, preserve a part of the connection relationships between fragments from different spatial units, and to control the verbosity of the function.

# archeofrag 1.1.0
Released: 2025-03-07

* Add datasets: CuzoulCave, CuzoulSouth. The former Tai dataset is splited into TaiCave and TaiSouth.
* In `frag.get.parameters()` if the vertices 'layer' attribute of the graph do not have two values, the variables 'balance', 'components.balance', and 'disturbance' are not computed and NAs are returned.
* In `frag.get.parameters()` the output variable 'weightsum' is renamed 'edge.weights.sum', and addition of two output variables: 'edge.weights.median' and 'edge.weights.median.abs.dev.'.
* In `frag.simul.compare()` add support for 'edge.weights.sum', 'edge.weights.median', and 'edge.weights.median.abs.dev.' variables.
* In `frag.simulprocess()` the 'asymmetric.transport.from' parameter now admits value 0 (handled as NULL values and disabling the asymmetric.transport feature).
* Revision of `frag.graph.reduce()`: to preserve the number of connected components, the use of igraph::articulation points is introduced. A bug in the while loops is fixed with a new control through the 'is.reducible' variable.
* Corrections in the documentation.
* Change default colors in `frag.graph.plot()`.

# archeofrag 1.0.0
Released: 2024-12-16

Fourth major release.

* Fix in the `frag.layers.admixture()` and `frag.layers.cohesion()` functions to handle pair of layers when only one layer includes fragments.
* In the `frag.layers.cohesion()` and `frag.layers.admixture()` functions, new optionnal parameters ("morphometry", "x", "y", "z") to pass to the `frag.edges.weighting()` function when applied to multiple pairs of layers.
* New `frag.graph.reduce()` function to reduce the number of fragments of a fragmentation graph while preserving the number of objects (i.e. connected components).
* Fix the `frag.relations.by.layers()` to return a square matrix.
* Values returned by `frag.layers.admixture()` and `frag.layers.cohesion()` are now rounded to four digits (to avoid negative admixture values).
* Fix the ordering of the pairs of layers returned by `frag.layers.admixture()` to correspond with the same order returned by `frag.layers.cohesion()`.
* Revising and optimising `frag.edges.weighting()`.
* Add verbose parameter to `frag.edges.weighting()`, `frag.get.layers.pair()`, `frag.get.parameters()`, `frag.layers.admixture()`, `frag.layers.cohesion()`.
* Add datasets: Chauzey, Le Bout des Vergnes, Grande Rivoire, Tai, Font-Juvenal.
* Add dependency to R (>= 3.5.0) to use serialized datasets (RData format).
* Balance and components balance computation in `frag.get.parameters()` were fixed to measure the value of the spatial unit with less fragments or components, respectively. This fix might result in values different from the value get before with this function.
* Add a 'components.balance' variable in the results of `frag.simul.compare`.

# archeofrag 0.8.3
Released: 2022-11-16

* add an option to remove the vertices in the `frag.observer.failure()` function.

# archeofrag 0.8.2
Released: 2022-07-27

* adding unit tests with testthat and test coverage analysis with covr.
* fix the `frag.simul.process()` function.

# archeofrag 0.8.1
Released: 2022-07-13

* all functions from other packages are tagged with their package.
* the RBGL is moved to the 'suggested' packages list, the functionalities related to planarity are now optional.
* igraph functions' names are updated.
* creation of a utils.R file, including internal functions to check the fragmentation graph and the "layer" argument.

# archeofrag 0.8.0
Released: 2022-03-08

Third major release.

* add license.
* complete update of the vignette.

# archeofrag 0.7.1
Released: 2022-02-17

* when using `frag.weight.edges()` with xyz coordinates, the function now returns a graph with an edge attribute with the spatial distance between fragments
* `frag.layers.summarise()` has three new parameters with default values to define the names of the colums with the cohesion and admixture values
* the balance, disturbance, and aggreg.factor values returned by `frag.get.parameters()` are now rounded with two digits.
* an error has been corrected in the `asymetric.transport.from` parameter of the `frag.simul.process()` function.

# archeofrag 0.7.0
Released: 2021-04-26

Second major release, after complete revision of the documentation.

# archeofrag 0.6.7
Released: 2021-04-12

* new function `frag.layers.compare()` to generate simulated graphs for two deposition hypotheses, from the parameters of an observed graph. 
* new function `frag.layers.summarise()` for post-processing the results of the `frag.layers.compare()` function.
* the `frag.weight.edges()` function now enables to weight the edges with morphometric and spatial distance values.

# archeofrag 0.6.6
Released: 2021-03-30

* the `frag.simul.process()` function has a new `asymetric.transport.from` parameter.
* new features in `frag.graph.plot()`: if the graph has two layers, the fragments are distributed by layers in two different regions of the plot.
* new `make_sr_graph()` function, to generate a connection graph from a `frag.object`.
* `frag.get.admixture()` can now be applied to a graph with more than 2 layers.
* `frag.get.cohesion()` can now be applied to a graph with more than 2 layers.
* The edges of the graphs generated with `frag.simul.process()` are now internally weighted with the `frag.edges.weighting()` function.
* `frag.observer.failure()`: new function to simulate the inaccuracy of an observer in determining the relationships between fragments.

# archeofrag 0.6.0
Released: 2020-11-27

This is the first public release of the archeofrag package.
