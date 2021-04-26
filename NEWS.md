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
