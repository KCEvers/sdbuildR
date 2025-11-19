# Plot stock-and-flow diagram

Visualize a stock-and-flow diagram using the R package DiagrammeR.
Stocks are represented as boxes. Flows are represented as arrows between
stocks and/or double circles, where the latter represent what it outside
of the model boundary. Thin grey edges indicate dependencies between
variables. By default, constants (indicated by italic labels) are not
shown. Hover over the variables to see their equations.

## Usage

``` r
# S3 method for class 'sdbuildR_xmile'
plot(
  x,
  vars = NULL,
  format_label = TRUE,
  wrap_width = 20,
  font_size = 18,
  font_family = "Times New Roman",
  stock_col = "#83d3d4",
  flow_col = "#f48153",
  dependency_col = "#999999",
  show_dependencies = TRUE,
  show_constants = FALSE,
  show_aux = TRUE,
  minlen = 2,
  ...
)
```

## Arguments

- x:

  A stock-and-flow model object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

- vars:

  Variables to plot. Defaults to NULL to plot all variables.

- format_label:

  If TRUE, apply default formatting (removing periods and underscores)
  to labels if labels are the same as variable names.

- wrap_width:

  Width of text wrapping for labels. Must be an integer. Defaults to 20.

- font_size:

  Font size. Defaults to 18.

- font_family:

  Font name. Defaults to "Times New Roman".

- stock_col:

  Colour of stocks. Defaults to "#83d3d4".

- flow_col:

  Colour of flows. Defaults to "#f48153".

- dependency_col:

  Colour of dependency arrows. Defaults to "#999999".

- show_dependencies:

  If TRUE, show dependencies between variables. Defaults to TRUE.

- show_constants:

  If TRUE, show constants. Defaults to FALSE.

- show_aux:

  If TRUE, show auxiliary variables. Defaults to TRUE.

- minlen:

  Minimum length of edges; must be an integer. Defaults to 2.

- ...:

  Optional arguments

## Value

Stock-and-flow diagram

## See also

[`insightmaker_to_sfm()`](https://kcevers.github.io/sdbuildR/reference/insightmaker_to_sfm.md),
[`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md),
[`plot.sdbuildR_sim()`](https://kcevers.github.io/sdbuildR/reference/plot.sdbuildR_sim.md)

## Examples

``` r
sfm <- xmile("SIR")
plot(sfm)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=.1, nodesep= .3]\n\n      # Rank groupings\n            {rank=same; \"Lambda\"; \"Infection_Rate\" }\n\n      # Define stock nodes\n      \"Recovered\" [id=\"Recovered\",label=\"Recovered\",tooltip = \"eqn = 0.0\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\t\t\"Infected\" [id=\"Infected\",label=\"Infected\",tooltip = \"eqn = 1\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\t\t\"Susceptible\" [id=\"Susceptible\",label=\"Susceptible\",tooltip = \"eqn = 99999\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      \"Infection_Rate\" [id=\"Infection_Rate\",label=\"Infection Rate\", tooltip = \"eqn = Susceptible * Lambda\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\t\t\"Recovery_Rate\" [id=\"Recovery_Rate\",label=\"Recovery Rate\", tooltip = \"eqn = Infected / Delay\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\n      # Define external cloud nodes\n      \n\n      # Define flow edges (stock -> flow_node -> stock)\n      \"Susceptible\" -> \"Infection_Rate\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"Infection_Rate\" -> \"Infected\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\t\t\"Infected\" -> \"Recovery_Rate\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"Recovery_Rate\" -> \"Recovered\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\n      # Define dependency edges\n      \"Infected\" -> \"Lambda\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"Susceptible\" -> \"Infection_Rate\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"Lambda\" -> \"Infection_Rate\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"Infected\" -> \"Recovery_Rate\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\n      # Define auxiliary nodes\n      \"Lambda\" [id=\"Lambda\",label=\"Lambda\",tooltip = \"eqn = Beta * Infected\",shape=plaintext,fontsize=16,fontname=\"Times New Roman\", width=0.6, height=0.3]\n\n      # Define constant nodes\n      \n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Don't show constants or auxiliaries
plot(sfm, show_constants = FALSE, show_aux = FALSE)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=.1, nodesep= .3]\n\n      # Rank groupings\n      \n\n      # Define stock nodes\n      \"Recovered\" [id=\"Recovered\",label=\"Recovered\",tooltip = \"eqn = 0.0\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\t\t\"Infected\" [id=\"Infected\",label=\"Infected\",tooltip = \"eqn = 1\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\t\t\"Susceptible\" [id=\"Susceptible\",label=\"Susceptible\",tooltip = \"eqn = 99999\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      \"Infection_Rate\" [id=\"Infection_Rate\",label=\"Infection Rate\", tooltip = \"eqn = Susceptible * Lambda\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\t\t\"Recovery_Rate\" [id=\"Recovery_Rate\",label=\"Recovery Rate\", tooltip = \"eqn = Infected / Delay\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\n      # Define external cloud nodes\n      \n\n      # Define flow edges (stock -> flow_node -> stock)\n      \"Susceptible\" -> \"Infection_Rate\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"Infection_Rate\" -> \"Infected\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\t\t\"Infected\" -> \"Recovery_Rate\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"Recovery_Rate\" -> \"Recovered\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\n      # Define dependency edges\n      \"Susceptible\" -> \"Infection_Rate\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"Infected\" -> \"Recovery_Rate\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\n      # Define auxiliary nodes\n      \n\n      # Define constant nodes\n      \n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Only show specific variables
plot(sfm, vars = "Susceptible")

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=.1, nodesep= .3]\n\n      # Rank groupings\n      \n\n      # Define stock nodes\n      \"Susceptible\" [id=\"Susceptible\",label=\"Susceptible\",tooltip = \"eqn = 99999\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      \n\n      # Define external cloud nodes\n      \n\n      # Define flow edges (stock -> flow_node -> stock)\n      \n\n      # Define dependency edges\n      \n\n      # Define auxiliary nodes\n      \n\n      # Define constant nodes\n      \n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
```
