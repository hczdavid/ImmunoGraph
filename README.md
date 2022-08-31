# ImmunoGraph

This repository keeps the core R code for the VSARgraph R package and Shiny App. VSARgraph is designed to draw RCDC and forest plot, which are commonly used to visualize the immunogenecity results. 

The R function “makeForest” in “VSARgraph” package is designed to support multi-block (e.g. top and bottom blocks) and multi-panel (e.g. more than one dataset) forest plot. Users can input multiple datasets and choose up to 2 blocks for each dataset. The data table attached to the right of the forest plot can be turned on or turned off. Three steps are included to make a final forest graph.

The R function “makeRCDC” in the package is the designed to support multi-panel (e.g. multiple serotypes) RCDC plot. Users can define the by variable to determine how many step lines will be drawn in one plot and define the filter variable to determine what data is used. R package “ggplot2” is used to draw Lower connection step plot. 

Currently VSARgraph is in a private repository and can be accessed by request.
