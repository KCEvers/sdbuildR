# Import Insight Maker model

Import a stock-and-flow model from [Insight
Maker](https://insightmaker.com/). Models may be your own or another
user's. Importing causal loop diagrams or agent-based models is not
supported.

## Usage

``` r
insightmaker_to_sfm(
  URL,
  file,
  keep_nonnegative_flow = TRUE,
  keep_nonnegative_stock = FALSE,
  keep_solver = FALSE
)
```

## Arguments

- URL:

  URL to Insight Maker model. Character.

- file:

  File path to Insight Maker model. Only used if URL is not specified.
  Needs to be a character with suffix .InsightMaker.

- keep_nonnegative_flow:

  If TRUE, keeps original non-negativity setting of flows. Defaults to
  TRUE.

- keep_nonnegative_stock:

  If TRUE, keeps original non-negativity setting of stocks Defaults to
  FALSE.

- keep_solver:

  If TRUE, keep the ODE solver as it is. If FALSE, switch to Euler
  integration in case of non-negative stocks to reproduce the Insight
  Maker data exactly. Defaults to FALSE.

## Value

A stock-and-flow model object of class
[`sdbuildR_xmile`](https://kcevers.github.io/sdbuildR/reference/xmile.md)

## Details

Insight Maker models can be imported using either a URL or an Insight
Maker file. Ensure the URL refers to a public (not private) model. To
download a model file from Insight Maker, first clone the model if it is
not your own. Then, go to "Share" (top right), "Export", and "Download
Insight Maker file".

## See also

[`build()`](https://kcevers.github.io/sdbuildR/reference/build.md),
[`xmile()`](https://kcevers.github.io/sdbuildR/reference/xmile.md)

## Examples

``` r
# Load a model from Insight Maker
sfm <- insightmaker_to_sfm(
  URL =
    "https://insightmaker.com/insight/43tz1nvUgbIiIOGSGtzIzj/Romeo-Juliet"
)
plot(sfm)

{"x":{"diagram":"\n    digraph sfm {\n\n      graph [layout = dot, rankdir = LR, center=true, outputorder=\"edgesfirst\", pad=.1, nodesep= .3]\n\n      # Rank groupings\n      \n\n      # Define stock nodes\n      \"Juliet\" [id=\"Juliet\",label=\"Juliet\",tooltip = \"eqn = .1\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\t\t\"Romeo\" [id=\"Romeo\",label=\"Romeo\",tooltip = \"eqn = .1\",shape=box,style=filled,fillcolor=\"#83d3d4\",fontsize=18,fontname=\"Times New Roman\"]\n\n      # Define flow nodes (intermediate nodes for flows)\n      \"inflow\" [id=\"inflow\",label=\"inflow\", tooltip = \"eqn = a*Romeo +b*Juliet\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\t\t\"inflow_1\" [id=\"inflow_1\",label=\"inflow 1\", tooltip = \"eqn = a_1*Romeo+ b_1*Juliet\", shape = plaintext, fontsize=16, fontname=\"Times New Roman\", width=0.6, height=0.3]\n\n      # Define external cloud nodes\n      \"Cloud1\" [label=\"\", tooltip = \"Unspecified sink\",shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]\n\t\t\"Cloud2\" [label=\"\", tooltip = \"Unspecified sink\",shape=doublecircle, fixedsize=true, width = .25, height = .25, orientation=15]\n\n      # Define flow edges (stock -> flow_node -> stock)\n      \"Cloud1\" -> \"inflow\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"inflow\" -> \"Romeo\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\t\t\"Cloud2\" -> \"inflow_1\" [arrowhead=\"none\", color=\"black:#f48153:black\", penwidth=1.1, minlen=2, splines=false]\n\t\t\"inflow_1\" -> \"Juliet\" [arrowhead=\"normal\", color=\"black:#f48153:black\", arrowsize=1.5, penwidth=1.1, minlen=2, splines=ortho]\n\n      # Define dependency edges\n      \"Romeo\" -> \"inflow\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"Juliet\" -> \"inflow\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"Romeo\" -> \"inflow_1\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\t\t\"Juliet\" -> \"inflow_1\" [color=\"#999999\", arrowsize=0.8, penwidth=1, splines=true, constraint=false, tailport=\"_\"]\n\n      # Define auxiliary nodes\n      \n\n      # Define constant nodes\n      \n    }\n          ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}

# Simulate the model
sim <- simulate(sfm)
plot(sim)

{"x":{"visdat":{"24e8f42ccc0":["function () ","plotlyVisDat"],"24e8435d1c6b":["function () ","data"]},"cur_data":"24e8435d1c6b","attrs":{"24e8435d1c6b":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"legendgroup":{},"showlegend":true,"type":"scatter","mode":"lines","colors":["#C87A8A","#6B9D59"],"visible":true,"inherit":true}},"layout":{"margin":{"b":50,"l":50,"t":50,"r":50},"legend":{"traceorder":"reversed","font":{"size":14}},"title":"Romeo & Juliet","xaxis":{"domain":[0,1],"automargin":true,"title":"Time (common months)","font":{"size":16}},"yaxis":{"domain":[0,1],"automargin":true,"title":"","font":{"size":16}},"font":{"family":"Times New Roman","size":16},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,20.5,21,21.5,22,22.5,23,23.5,24,24.5,25,25.5,26,26.5,27,27.5,28,28.5,29,29.5,30,30.5,31,31.5,32,32.5,33,33.5,34,34.5,35,35.5,36,36.5,37,37.5,38,38.5,39,39.5,40],"y":[0.10000000000000001,0.033406262956478777,-0.034633644028582711,-0.088129367849330315,-0.11600090743474534,-0.11419288155798339,-0.085950920130250352,-0.040370678614577507,0.010278075759072039,0.053712273374928309,0.08049866115122814,0.085980477010477507,0.070907894959825246,0.040748704458260468,0.0039654219490861797,-0.03024139009765784,-0.054147011836717128,-0.063161916968308765,-0.056602742039168541,-0.037479785105286961,-0.011444134101842706,0.014780293201390616,0.035065459028491001,0.045250282123597871,0.043922955012292396,0.032488256254675323,0.014572215997255597,-0.0050340982361208476,-0.021601728024675651,-0.03155972157790092,-0.033216937251538424,-0.026977178474555299,-0.015045751173689618,-0.00074421076576325221,0.012369757460759641,0.021353720076115426,0.024507509253937885,0.021651130911674935,0.014022038644960697,0.0038519297910168831,-0.0062478359796479428,-0.013930791272676506,-0.017636860600688937,-0.016881010151520225,-0.012263953221507813,-0.0052288668517309648,0.0023550271816281253,0.0086685188761789055,0.012361511565323799,0.01282294131825955,0.010252821716994261,0.0055384583756679737,-1.7690158101549974e-05,-0.0050409813062990581,-0.0084117379416299484,-0.0095018607513723662,-0.0082743904895307557,-0.00523586494102341,-0.0012665794187554486,0.0026200823369597311,0.0055264231366174065,0.0068685793233296656,0.0064826910351345607,0.0046231116736740081,0.0018633475527042251,-0.0010679356185503636,-0.0034714511045080814,-0.0048374233651564133,-0.0049463271997164824,-0.0038924144099984864,-0.002031894181772667,0.00012498915231702937,0.0020474971321707194,0.0033100050838498884,0.0036811662608683426,0.0031593257822412433,0.0019510482058812343,0.00040317506989252967,-0.0010913353766884243,-0.0021893527478549593,-0.0026727793132567132],"legendgroup":"Romeo","showlegend":true,"type":"scatter","mode":"lines","visible":true,"name":"Romeo","marker":{"color":"rgba(200,122,138,1)","line":{"color":"rgba(200,122,138,1)"}},"textfont":{"color":"rgba(200,122,138,1)"},"error_y":{"color":"rgba(200,122,138,1)"},"error_x":{"color":"rgba(200,122,138,1)"},"line":{"color":"rgba(200,122,138,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,20.5,21,21.5,22,22.5,23,23.5,24,24.5,25,25.5,26,26.5,27,27.5,28,28.5,29,29.5,30,30.5,31,31.5,32,32.5,33,33.5,34,34.5,35,35.5,36,36.5,37,37.5,38,38.5,39,39.5,40],"y":[0.10000000000000001,0.13377834162712549,0.13317301880928217,0.10159709440410639,0.049354728966043038,-0.0094153754630959055,-0.060399078991907024,-0.09245857445578376,-0.099925136784655824,-0.083399438499162704,-0.04901498788281744,-0.0064825553485162793,0.033512775678164741,0.061893862146544916,0.073153384935028251,0.066297295828884745,0.044646929364512415,0.01465449788021948,-0.015898161730442648,-0.039839221371289452,-0.05221978820171775,-0.051255415924911091,-0.038440730002535742,-0.017889148433109351,0.0048747206725061852,0.024336558879646471,0.036276148561144084,0.038627498920540786,0.031755203537505598,0.018138193161958874,0.0015909603403009868,-0.013752280711017879,-0.024431257065347857,-0.02840171810558529,-0.025376977009142106,-0.01672745262623181,-0.0050036793455155975,0.0067702486774513029,0.015846331324281853,0.020366649943786209,0.019711544765265088,0.014526154939834274,0.0064497453444766336,-0.0023608317609729043,-0.0097829201590171021,-0.014219366741133339,-0.014920636824405692,-0.012078788200708471,-0.0066931191884452238,-0.00026052159407699386,0.0056206347533247544,0.009632565754345639,0.011018404595493688,0.0097051672560522009,0.0062556773364246861,0.0016769279428313117,-0.0028568075089672519,-0.0062934900050204768,-0.0079367991676963737,-0.0075745191166778896,-0.005481914519064891,-0.00231120645479805,0.0010962616982008106,0.0039240475795672882,0.0055684644868294938,0.00575898904277607,0.0045895863123444451,0.0024621311754210854,-3.6526574871406387e-05,-0.0022888959850895221,-0.0037936310790353195,-0.0042712876793072757,-0.0037083148962203886,-0.0023349093626247189,-0.00054818576513120131,0.001196231875641897,0.0024959346926626816,0.0030904228157684096,0.0029082952914896028,0.0020658908283932194,0.00082231611278794177],"legendgroup":"Juliet","showlegend":true,"type":"scatter","mode":"lines","visible":true,"name":"Juliet","marker":{"color":"rgba(107,157,89,1)","line":{"color":"rgba(107,157,89,1)"}},"textfont":{"color":"rgba(107,157,89,1)"},"error_y":{"color":"rgba(107,157,89,1)"},"error_x":{"color":"rgba(107,157,89,1)"},"line":{"color":"rgba(107,157,89,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
