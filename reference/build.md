# Create, modify or remove variables

Add, change, or erase variables in a stock-and-flow model. Variables may
be stocks, flows, constants, auxiliaries, or graphical functions.

## Usage

``` r
build(
  sfm,
  name,
  type,
  eqn = "0.0",
  units = "1",
  label = name,
  doc = "",
  change_name = NULL,
  change_type = NULL,
  erase = FALSE,
  to = NULL,
  from = NULL,
  non_negative = FALSE,
  xpts = NULL,
  ypts = NULL,
  source = NULL,
  interpolation = "linear",
  extrapolation = "nearest",
  df = NULL
)
```

## Arguments

- sfm:

  Stock-and-flow model, object of class
  [`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md).

- name:

  Variable name. Character vector.

- type:

  Type of building block(s); one of 'stock', 'flow', 'constant', 'aux',
  or 'gf'). Does not need to be specified to modify an existing
  variable.

- eqn:

  Equation (or initial value in the case of stocks). Defaults to "0.0".

- units:

  Unit of variable, such as 'meter'. Defaults to "1" (no units).

- label:

  Name of variable used for plotting. Defaults to the same as name.

- doc:

  Description of variable. Defaults to "" (no description).

- change_name:

  New name for variable (optional). Defaults to NULL to indicate no
  change.

- change_type:

  New type for variable (optional). Defaults to NULL to indicate no
  change.

- erase:

  If TRUE, remove variable from model. Defaults to FALSE.

- to:

  Target of flow. Must be a stock in the model. Defaults to NULL to
  indicate no target.

- from:

  Source of flow. Must be a stock in the model. Defaults to NULL to
  indicate no source.

- non_negative:

  If TRUE, variable is enforced to be non-negative (i.e. strictly 0 or
  positive). Defaults to FALSE.

- xpts:

  Only for graphical functions: vector of x-domain points. Must be of
  the same length as ypts.

- ypts:

  Only for graphical functions: vector of y-domain points. Must be of
  the same length as xpts.

- source:

  Only for graphical functions: name of the variable which will serve as
  the input to the graphical function. Necessary to specify if units are
  used. Defaults to NULL.

- interpolation:

  Only for graphical functions: interpolation method. Must be either
  "constant" or "linear". Defaults to "linear".

- extrapolation:

  Only for graphical functions: extrapolation method. Must be either
  "nearest" or "NA". Defaults to "nearest".

- df:

  A data.frame with variable properties to add and/or modify. Each row
  represents one variable to build. Required columns depend on the
  variable type being created:

  - All types require: 'type', 'name'

  - Stocks require: 'eqn' (initial value)

  - Flows require: 'eqn', and at least one of 'from' or 'to'

  - Constants require: 'eqn'

  - Auxiliaries require: 'eqn'

  - Graphical functions require: 'xpts', 'ypts'

  Optional columns for all types: 'units', 'label', 'doc',
  'non_negative' Optional columns for graphical functions: 'source',
  'interpolation', 'extrapolation'

  Columns not applicable to a variable type should be set to NA. See
  Examples for a complete demonstration.

## Value

A stock-and-flow model object of class
[`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md)

## Stocks

Stocks define the state of the system. They accumulate material or
information over time, such as people, products, or beliefs, which
creates memory and inertia in the system. As such, stocks need not be
tangible. Stocks are variables that can increase and decrease, and can
be measured at a single moment in time. The value of a stock is
increased or decreased by flows. A stock may have multiple inflows and
multiple outflows. The net change in a stock is the sum of its inflows
minus the sum of its outflows.

The obligatory properties of a stock are "name", "type", and "eqn".
Optional additional properties are "units", "label", "doc",
"non_negative".

## Flows

Flows move material and information through the system. Stocks can only
decrease or increase through flows. A flow must flow from and/or flow to
a stock. If a flow is not flowing from a stock, the source of the flow
is outside of the model boundary. Similarly, if a flow is not flowing to
a stock, the destination of the flow is outside the model boundary.
Flows are defined in units of material or information moved over time,
such as birth rates, revenue, and sales.

The obligatory properties of a flow are "name", "type", "eqn", and
either "from", "to", or both. Optional additional properties are
"units", "label", "doc", "non_negative".

## Constants

Constants are variables that do not change over the course of the
simulation - they are time-independent. These may be numbers, but also
functions. They can depend only on other constants.

The obligatory properties of a constant are "name", "type", and "eqn".
Optional additional properties are "units", "label", "doc",
"non_negative".

## Auxiliaries

Auxiliaries are dynamic variables that change over time. They are used
for intermediate calculations in the system, and can depend on other
flows, auxiliaries, constants, and stocks.

The obligatory properties of an auxiliary are "name", "type", and "eqn".
Optional additional properties are "units", "label", "doc",
"non_negative".

## Graphical functions

Graphical functions, also known as table or lookup functions, are
interpolation functions used to define the desired output (y) for a
specified input (x). They are defined by a set of x- and y-domain
points, which are used to create a piecewise linear function. The
interpolation method defines the behavior of the graphical function
between x-points ("constant" to return the value of the previous
x-point, "linear" to linearly interpolate between defined x-points), and
the extrapolation method defines the behavior outside of the x-points
("NA" to return NA values outside of defined x-points, "nearest" to
return the value of the closest x-point).

The obligatory properties of a graphical function are "name", "type",
"xpts", and "ypts". "xpts" and "ypts" must be of the same length.
Optional additional properties are "units", "label", "doc", "source",
"interpolation", "extrapolation".

## See also

[`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md)

## Examples

``` r
# First initialize an empty model
sfm <- xmile()
summary(sfm)
#> Your model contains:
#> * 0 Stocks
#> * 0 Flows
#> * 0 Constants
#> * 0 Auxiliaries
#> * 0 Graphical Functions
#> * 0 Custom model units
#> * 0 Macros
#> 
#> Simulation time: 0.0 to 100.0 seconds (dt = 0.01)
#> Simulation settings: solver euler in R

# Add two stocks. Specify their initial values in the "eqn" property
# and their plotting label.
sfm <- build(sfm, "predator", "stock", eqn = 10, label = "Predator") |>
  build("prey", "stock", eqn = 50, label = "Prey")


# Add four flows: the births and deaths of both the predators and prey. The
# "eqn" property of flows represents the rate of the flow. In addition, we
# specify which stock the flow is coming from ("from") or flowing to ("to").
sfm <- build(sfm, "predator_births", "flow",
  eqn = "delta*prey*predator",
  label = "Predator Births", to = "predator"
) |>
  build("predator_deaths", "flow",
    eqn = "gamma*predator",
    label = "Predator Deaths", from = "predator"
  ) |>
  build("prey_births", "flow",
    eqn = "alpha*prey",
    label = "Prey Births", to = "prey"
  ) |>
  build("prey_deaths", "flow",
    eqn = "beta*prey*predator",
    label = "Prey Deaths", from = "prey"
  )
plot(sfm)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=.1, nodesep= .3]\n\n      # Rank groupings\n      \n\n      # Define stock nodes\n      \"prey\" [id=\"prey\",label=\"Prey\",tooltip = \"eqn = 50\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\t\t\"predator\" [id=\"predator\",label=\"Predator\",tooltip = \"eqn = 10\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      \"predator_births\" [id=\"predator_births\",label=\"Predator Births\", tooltip = \"eqn = delta*prey*predator\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\t\t\"predator_deaths\" [id=\"predator_deaths\",label=\"Predator Deaths\", tooltip = \"eqn = gamma*predator\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\t\t\"prey_births\" [id=\"prey_births\",label=\"Prey Births\", tooltip = \"eqn = alpha*prey\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\t\t\"prey_deaths\" [id=\"prey_deaths\",label=\"Prey Deaths\", tooltip = \"eqn = beta*prey*predator\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\n      # Define external cloud nodes\n      \"Cloud1\" [label=\"\", tooltip = \"Unspecified source\",shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]\n\t\t\"Cloud2\" [label=\"\", tooltip = \"Unspecified source\",shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]\n\t\t\"Cloud3\" [label=\"\", tooltip = \"Unspecified sink\",shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]\n\t\t\"Cloud4\" [label=\"\", tooltip = \"Unspecified sink\",shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]\n\n      # Define flow edges (stock -> flow_node -> stock)\n      \"Cloud3\" -> \"predator_births\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"predator_births\" -> \"predator\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\t\t\"predator\" -> \"predator_deaths\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"predator_deaths\" -> \"Cloud1\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\t\t\"Cloud4\" -> \"prey_births\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"prey_births\" -> \"prey\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\t\t\"prey\" -> \"prey_deaths\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"prey_deaths\" -> \"Cloud2\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\n      # Define dependency edges\n      \"prey\" -> \"predator_births\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"predator\" -> \"predator_births\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"predator\" -> \"predator_deaths\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"prey\" -> \"prey_births\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"prey\" -> \"prey_deaths\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"predator\" -> \"prey_deaths\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\n      # Define auxiliary nodes\n      \n\n      # Define constant nodes\n      \n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# The flows make use of four other variables: "delta", "gamma", "alpha", and
# "beta". Define these as constants in a vectorized manner for efficiency.
sfm <- build(sfm, c("delta", "gamma", "alpha", "beta"), "constant",
  eqn = c(.025, .5, .5, .05),
  label = c("Delta", "Gamma", "Alpha", "Beta"),
  doc = c(
    "Birth rate of predators", "Death rate of predators",
    "Birth rate of prey", "Death rate of prey by predators"
  )
)

# We now have a complete predator-prey model which is ready to be simulated.
sim <- simulate(sfm)
plot(sim)

{"x":{"visdat":{"2fc0177441db":["function () ","plotlyVisDat"],"2fc031fcd0fa":["function () ","data"]},"cur_data":"2fc031fcd0fa","attrs":{"2fc031fcd0fa":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"legendgroup":{},"showlegend":true,"type":"scatter","mode":"lines","colors":["#C87A8A","#6B9D59"],"visible":true,"inherit":true}},"layout":{"margin":{"b":50,"l":50,"t":50,"r":50},"legend":{"traceorder":"reversed","font":{"size":14}},"title":"My Model","xaxis":{"domain":[0,1],"automargin":true,"title":"Time (seconds)","font":{"size":16}},"yaxis":{"domain":[0,1],"automargin":true,"title":"","font":{"size":16}},"font":{"family":"Times New Roman","size":16},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,20.5,21,21.5,22,22.5,23,23.5,24,24.5,25,25.5,26,26.5,27,27.5,28,28.5,29,29.5,30,30.5,31,31.5,32,32.5,33,33.5,34,34.5,35,35.5,36,36.5,37,37.5,38,38.5,39,39.5,40,40.5,41,41.5,42,42.5,43,43.5,44,44.5,45,45.5,46,46.5,47,47.5,48,48.5,49,49.5,50,50.5,51,51.5,52,52.5,53,53.5,54,54.5,55,55.5,56,56.5,57,57.5,58,58.5,59,59.5,60,60.5,61,61.5,62,62.5,63,63.5,64,64.5,65,65.5,66,66.5,67,67.5,68,68.5,69,69.5,70,70.5,71,71.5,72,72.5,73,73.5,74,74.5,75,75.5,76,76.5,77,77.5,78,78.5,79,79.5,80,80.5,81,81.5,82,82.5,83,83.5,84,84.5,85,85.5,86,86.5,87,87.5,88,88.5,89,89.5,90,90.5,91,91.5,92,92.5,93,93.5,94,94.5,95,95.5,96,96.5,97,97.5,98,98.5,99,99.5,100],"y":[10,14.388449806296931,19.448587952454048,23.481529320470035,25.073544825006515,24.270192007228346,22.006130120875074,19.173683700658952,16.319322605509917,13.706441423504907,11.432649657140249,9.5116245612613799,7.9190503826700374,6.616313369866436,5.5621432479153796,4.7180421459900499,4.0506118487570699,3.5324187189296037,3.1422902740297065,2.8655915071463194,2.6949250265484728,2.6317916056071819,2.6900819432242229,2.9029844851095947,3.3361675867322895,4.1116421449651996,5.4451190955933599,7.6786639760812117,11.205724312068742,16.047458896007761,21.142058026540528,24.59422772816167,25.362103480736952,23.925901163873196,21.327791034560029,18.380768066903673,15.534518132430355,12.987293457412363,10.800318256202178,8.9687704253689446,7.459717447531526,6.2311239674813255,5.2409399444568203,4.4511772522111714,3.8295366938223383,3.3499289213803696,2.9926315707266338,2.7445621228041257,2.6000987928477364,2.563031120366432,2.6506412104923238,2.9017812096029756,3.3923238177520019,4.2630232845282778,5.7616974825280893,8.271326675998381,12.184519631031904,17.35436912649816,22.393522252201318,25.344720712589844,25.509773467919921,23.649264654754706,20.839192943123194,17.824412446055156,14.988455573292475,12.487524898291044,10.35977911492971,8.5887105900665635,7.135932069235098,5.9572328955947116,5.0100636228756681,4.256758685047723,3.6657167072935888,3.2116983250971733,2.8758894474116703,2.6461668985552742,2.5179927798123174,2.4965524701690205,2.6012337568720807,2.8745297921249788,3.3991709977572429,4.3291304399089006,5.9360196906175871,8.6348751106272754,12.820817304652964,18.225909978392277,23.237726283630611,25.874130208100656,25.666314454417233,23.541866018247195,20.59326923347512,17.525562966103802,14.684428234262226,12.201738970110323,10.101945238163626,8.3612979864095678,6.9377963174435306,5.7855843235506974,4.8615112314777571,4.1278459160696803,3.5531791027578263,3.1125704176538265,2.7875329092573509,2.5662637868381331,2.4445376000638079,2.4278849151004036,2.5361932327097803,2.8129218426390832,3.3430063329906718,4.2856838881355834,5.9245465239311077,8.6972500987348145,13.021871728631988,18.60080561946917,23.703934465184602,26.278444863736116,25.927852577204888,23.66960365724491,20.627395789604236,17.502707310126148,14.629968300367663,12.131534591789457,10.025396029824947,8.2837244254431042,6.8620082915573732,5.7128583588675008,4.792184729021236,4.0616699900347326,3.4895170189737512,3.0504915754889832,2.7258270941373985,2.5033808769957941,2.3784386205097099,2.3557661680364186,2.4540125958256769,2.7146240363227605,3.2193852483776686,4.1232756309458489,5.7070252787463716,8.4180449754742561,12.722988574046651,18.41408125169572,23.7713803865324,26.586324679081518,26.341303177451696,24.075249961928503,20.974100129039861,17.779038234272885,14.841272089370754,12.288172751285169,10.137973408867476,8.3614729802605172,6.9124263743615515,5.7418003047408179,4.8040782920579872,4.0597363369407589,3.4759450509439169,3.0265451034451902,2.6918669004203069,2.4587723022286152,2.3212872672058196,2.2823689962337581,2.3578074966280171,2.5842388282385702,3.0351464143954181,3.8516347147678993,5.294972817855605,7.8022962322078113,11.899343613614001,17.587669692920905,23.338943288085115,26.736178060763969,26.898279150353584,24.781048582258435,21.665766285399521,18.386827966310175,15.346707298211946,12.695259499228717,10.45880358248542,8.6098350767199427,7.1012062845451682,5.8820715139452595,4.9049122058894952,4.1282921620434188,3.5176361985157825,3.0451822897798828,2.6897167065430905,2.4364713363427652,2.2775209871173074,2.2131474706245617,2.2550119146730672,2.4328061937985348,2.807742608532283,3.4991990731698688,4.7333871979591597,6.9099942676696866,10.590777660370057,16.057680108364309,22.217991937724271,26.541603943175335,27.507542336488171],"legendgroup":"Predator","showlegend":true,"type":"scatter","mode":"lines","visible":true,"name":"Predator","marker":{"color":"rgba(200,122,138,1)","line":{"color":"rgba(200,122,138,1)"}},"textfont":{"color":"rgba(200,122,138,1)"},"error_y":{"color":"rgba(200,122,138,1)"},"error_x":{"color":"rgba(200,122,138,1)"},"line":{"color":"rgba(200,122,138,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,20.5,21,21.5,22,22.5,23,23.5,24,24.5,25,25.5,26,26.5,27,27.5,28,28.5,29,29.5,30,30.5,31,31.5,32,32.5,33,33.5,34,34.5,35,35.5,36,36.5,37,37.5,38,38.5,39,39.5,40,40.5,41,41.5,42,42.5,43,43.5,44,44.5,45,45.5,46,46.5,47,47.5,48,48.5,49,49.5,50,50.5,51,51.5,52,52.5,53,53.5,54,54.5,55,55.5,56,56.5,57,57.5,58,58.5,59,59.5,60,60.5,61,61.5,62,62.5,63,63.5,64,64.5,65,65.5,66,66.5,67,67.5,68,68.5,69,69.5,70,70.5,71,71.5,72,72.5,73,73.5,74,74.5,75,75.5,76,76.5,77,77.5,78,78.5,79,79.5,80,80.5,81,81.5,82,82.5,83,83.5,84,84.5,85,85.5,86,86.5,87,87.5,88,88.5,89,89.5,90,90.5,91,91.5,92,92.5,93,93.5,94,94.5,95,95.5,96,96.5,97,97.5,98,98.5,99,99.5,100],"y":[50,47.502299106183855,39.991115243728295,29.90740656001158,20.79372899100926,14.326874622537101,10.278171747514813,7.8724537097013005,6.4813199181759087,5.716957820900765,5.3619479790875815,5.300273493583167,5.4745573144001192,5.8626757988482128,6.4655708178917202,7.3011382430430158,8.4013699885668629,9.8111808038436852,11.587914559303458,13.800637227211825,16.528042765884877,19.852998945682479,23.85007886191509,28.55913266248125,33.931922329853613,39.729595294395324,45.343322091202353,49.550831766077614,50.441400285830539,46.220224981202819,37.260757431859751,26.86648534464182,18.349959609545088,12.657977276650451,9.2029620391566329,7.1827158075800446,6.0327541657096893,5.4230008361222524,5.1732126911497947,5.1894748095632046,5.4276531314860792,5.8740251866584385,6.535391778334894,7.4342543156390466,8.6066884299442528,10.101584675480995,11.980353263922854,14.316202846058525,17.191717012374344,20.692487904558718,24.892572559387226,29.823635209363669,35.412515091623298,41.361444049706975,46.941891004949454,50.735858181178017,50.655411149666115,45.089473362419298,35.161897691585047,24.703140159604764,16.683146555897373,11.531216604080997,8.46763425342,6.6975031953136135,5.7041856819501335,5.1955479317196014,5.0150169768790933,5.0826951759662098,5.3629149846358084,5.8474042763879783,6.5468392761774084,7.4868393363968053,8.7063190179143248,10.257017507657617,12.203367025725203,14.621813250368275,17.598246132983604,21.221121925877224,25.565637396127894,30.659947582925838,36.416398740282318,42.499012969459471,48.096316084104743,51.648354834641189,50.935132267926434,44.478285432203791,33.921038360711407,23.411569692993709,15.67528496075971,10.830268181910236,7.9898560227389144,6.3637545883298641,5.4614355149741396,5.0115007582929811,4.8704564421462031,4.9662456365545937,5.2679760997424578,5.7704523347348857,6.4865243776242494,7.443550610589929,8.6820430568762514,10.2554012400935,12.229942295729058,14.684360791161957,17.707272629925619,21.390378744713271,25.812470337236963,31.004895098926664,36.880520051156573,43.095287768839668,48.807807616651921,52.386169139226311,51.501806435624147,44.650341783052177,33.684615903349716,22.987999815624697,15.261685983507027,10.493343864364029,7.7260401113324146,6.1530704097793079,5.2859246742309542,4.8581298533916719,4.7301372727940683,4.8325012952241524,5.1359530918523193,5.6362837697229624,6.3469452942854376,7.2956893092906308,8.5233505381573416,10.083716069041278,12.04372380991194,14.483168634818574,17.492638365781801,21.167325053701671,25.592109189846198,30.808781838694241,36.747600984006375,43.091470194458573,49.032959283716764,52.955167689319097,52.447829445566093,45.765591295271008,34.586037338335316,23.488143039433226,15.448379895402786,10.510897394212693,7.6655425827285368,6.0572028989428102,5.1718621023020912,4.7314919647167422,4.5913953230638791,4.679655490178833,4.9655919599662779,5.4439618170747313,6.1272751106109471,7.0423231836087403,8.2289451290866715,9.7399567428663261,11.641508658003328,14.013118193708967,16.946229826348286,20.539203566894471,24.884607056730257,30.040543924757849,35.969588266059674,42.414735565028643,48.667985545385463,53.230670963012685,53.68304807624115,47.856608105292366,36.775681900764134,25.066506547943806,16.332531961157507,10.932424920517544,7.8321947306537547,6.0880867286338862,5.1259908400067564,4.6362177552846067,4.4581937754784882,4.5117342105772886,4.7613995266478195,5.1987552317749692,5.8337640223723044,6.690869186484897,7.807560098037591,9.2342586225033259,11.034792537693461,13.286773428052376,16.08090857475953,19.517501321152714,23.696717098638718,28.695704336252042,34.518586839270988,40.992110183726993,47.561664026204902,52.95411045829244,54.886742894575974,50.74362334979191,40.381018054230431,28.002043955426906,18.139407458103104],"legendgroup":"Prey","showlegend":true,"type":"scatter","mode":"lines","visible":true,"name":"Prey","marker":{"color":"rgba(107,157,89,1)","line":{"color":"rgba(107,157,89,1)"}},"textfont":{"color":"rgba(107,157,89,1)"},"error_y":{"color":"rgba(107,157,89,1)"},"error_x":{"color":"rgba(107,157,89,1)"},"line":{"color":"rgba(107,157,89,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# Modify a variable - note that we no longer need to specify type
sfm <- build(sfm, "delta", eqn = .03, label = "DELTA")

# Change variable name (throughout the model)
sfm <- build(sfm, "delta", change_name = "DELTA")

# Change variable type
sfm <- build(sfm, "DELTA", change_type = "stock")

# Remove variable
sfm <- build(sfm, "prey", erase = TRUE)

# To add and/or modify variables more quickly, pass a data.frame.
# The data.frame is processed row-wise.
# For instance, to create a logistic population growth model:
df <- data.frame(
  type = c("stock", "flow", "flow", "constant", "constant"),
  name = c("X", "inflow", "outflow", "r", "K"),
  eqn = c(.01, "r * X", "r * X^2 / K", 0.1, 1),
  label = c(
    "Population size", "Births", "Deaths", "Growth rate",
    "Carrying capacity"
  ),
  to = c(NA, "X", NA, NA, NA),
  from = c(NA, NA, "X", NA, NA)
)
sfm <- build(xmile(), df = df)

# Check for errors in the model
debugger(sfm)
#> No problems detected!
```
