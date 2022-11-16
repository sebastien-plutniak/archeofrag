
# archeofrag 0.8.3
Released: 2022-0?-??

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

# archeofrag 0.8
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

# archeofrag 0.7
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

# archeofrag 0.6
Released: 2020-11-27

This is the first public release of the archeofrag package.
