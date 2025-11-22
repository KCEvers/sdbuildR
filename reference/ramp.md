# Create ramp function

Create a ramp function that increases linearly from 0 to a specified
height at a specified start time, and stays at this height after the
specified end time.

## Usage

``` r
ramp(times, start, finish, height = 1)
```

## Arguments

- times:

  Vector of simulation times

- start:

  Start time of ramp

- finish:

  End time of ramp

- height:

  End height of ramp, defaults to 1

## Value

Ramp interpolation function

## Details

Equivalent of Ramp() in Insight Maker

## See also

[`step()`](https://kcevers.github.io/sdbuildR/reference/step.md),
[`pulse()`](https://kcevers.github.io/sdbuildR/reference/pulse.md),
[`seasonal()`](https://kcevers.github.io/sdbuildR/reference/seasonal.md)

## Examples

``` r
# Create a simple model with a ramp function
sfm <- xmile() |>
  build("a", "stock") |>
  # Specify the global variable "times" as simulation times
  build("input", "constant", eqn = "ramp(times, 20, 30, 3)") |>
  build("inflow", "flow", eqn = "input(t)", to = "a")


sim <- simulate(sfm, only_stocks = FALSE)
plot(sim)

{"x":{"visdat":{"2d573cf859d3":["function () ","plotlyVisDat"],"2d5754c7682":["function () ","data"],"2d57683c0d2":["function () ","data"]},"cur_data":"2d57683c0d2","attrs":{"2d5754c7682":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"legendgroup":{},"showlegend":true,"colors":["#C87A8A","#6B9D59"],"type":"scatter","mode":"lines","visible":"legendonly","inherit":true},"2d57683c0d2":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"legendgroup":{},"showlegend":true,"type":"scatter","mode":"lines","colors":["#C87A8A","#6B9D59"],"visible":true,"inherit":true}},"layout":{"margin":{"b":50,"l":50,"t":50,"r":50},"legend":{"traceorder":"reversed","font":{"size":14}},"title":"My Model","xaxis":{"domain":[0,1],"automargin":true,"title":"Time (seconds)","font":{"size":16}},"yaxis":{"domain":[0,1],"automargin":true,"title":"","font":{"size":16}},"font":{"family":"Times New Roman","size":16},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30000000000000004,0.60000000000000009,0.89999999999999991,1.2000000000000002,1.5,1.7999999999999998,2.0999999999999996,2.4000000000000004,2.7000000000000002,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3],"legendgroup":"inflow","showlegend":true,"type":"scatter","mode":"lines","visible":"legendonly","name":"inflow","marker":{"color":"rgba(200,122,138,1)","line":{"color":"rgba(200,122,138,1)"}},"textfont":{"color":"rgba(200,122,138,1)"},"error_y":{"color":"rgba(200,122,138,1)"},"error_x":{"color":"rgba(200,122,138,1)"},"line":{"color":"rgba(200,122,138,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.13500000000000001,0.56999999999999995,1.3049999999999997,2.3399999999999999,3.6750000000000003,5.3100000000000005,7.245000000000001,9.4800000000000004,12.015000000000002,14.85,17.849999999999998,20.849999999999998,23.849999999999998,26.849999999999998,29.849999999999998,32.850000000000001,35.850000000000001,38.850000000000001,41.850000000000001,44.850000000000001,47.850000000000001,50.850000000000001,53.850000000000001,56.850000000000001,59.850000000000001,62.850000000000001,65.850000000000023,68.85000000000008,71.850000000000136,74.850000000000193,77.85000000000025,80.850000000000307,83.850000000000364,86.850000000000421,89.850000000000477,92.850000000000534,95.850000000000591,98.850000000000648,101.8500000000007,104.85000000000076,107.85000000000082,110.85000000000088,113.85000000000093,116.85000000000099,119.85000000000099,122.85000000000099,125.85000000000099,128.85000000000096,131.85000000000096,134.85000000000096,137.85000000000096,140.85000000000096,143.85000000000096,146.85000000000096,149.85000000000096,152.85000000000096,155.85000000000099,158.85000000000099,161.85000000000099,164.85000000000099,167.85000000000099,170.85000000000099,173.85000000000099,176.85000000000099,179.85000000000099,182.85000000000099,185.85000000000099,188.85000000000099,191.85000000000099,194.85000000000099,197.85000000000099,200.85000000000099,203.85000000000099,206.85000000000099,209.85000000000099,212.85000000000099,215.85000000000099,218.85000000000099,221.85000000000099,224.85000000000099],"legendgroup":"a","showlegend":true,"type":"scatter","mode":"lines","visible":true,"name":"a","marker":{"color":"rgba(107,157,89,1)","line":{"color":"rgba(107,157,89,1)"}},"textfont":{"color":"rgba(107,157,89,1)"},"error_y":{"color":"rgba(107,157,89,1)"},"error_x":{"color":"rgba(107,157,89,1)"},"line":{"color":"rgba(107,157,89,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
# To create a decreasing ramp, set the height to a negative value
sfm <- build(sfm, "input", eqn = "ramp(times, 20, 30, -3)")

sim <- simulate(sfm, only_stocks = FALSE)
plot(sim)

{"x":{"visdat":{"2d5741c4bf15":["function () ","plotlyVisDat"],"2d5752f780fb":["function () ","data"],"2d572ba83fc4":["function () ","data"]},"cur_data":"2d572ba83fc4","attrs":{"2d5752f780fb":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"legendgroup":{},"showlegend":true,"colors":["#C87A8A","#6B9D59"],"type":"scatter","mode":"lines","visible":"legendonly","inherit":true},"2d572ba83fc4":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"legendgroup":{},"showlegend":true,"type":"scatter","mode":"lines","colors":["#C87A8A","#6B9D59"],"visible":true,"inherit":true}},"layout":{"margin":{"b":50,"l":50,"t":50,"r":50},"legend":{"traceorder":"reversed","font":{"size":14}},"title":"My Model","xaxis":{"domain":[0,1],"automargin":true,"title":"Time (seconds)","font":{"size":16}},"yaxis":{"domain":[0,1],"automargin":true,"title":"","font":{"size":16}},"font":{"family":"Times New Roman","size":16},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0.30000000000000004,-0.60000000000000009,-0.89999999999999991,-1.2000000000000002,-1.5,-1.7999999999999998,-2.0999999999999996,-2.4000000000000004,-2.7000000000000002,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3],"legendgroup":"inflow","showlegend":true,"type":"scatter","mode":"lines","visible":"legendonly","name":"inflow","marker":{"color":"rgba(200,122,138,1)","line":{"color":"rgba(200,122,138,1)"}},"textfont":{"color":"rgba(200,122,138,1)"},"error_y":{"color":"rgba(200,122,138,1)"},"error_x":{"color":"rgba(200,122,138,1)"},"line":{"color":"rgba(200,122,138,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0.13500000000000001,-0.56999999999999995,-1.3049999999999997,-2.3399999999999999,-3.6750000000000003,-5.3100000000000005,-7.245000000000001,-9.4800000000000004,-12.015000000000002,-14.85,-17.849999999999998,-20.849999999999998,-23.849999999999998,-26.849999999999998,-29.849999999999998,-32.850000000000001,-35.850000000000001,-38.850000000000001,-41.850000000000001,-44.850000000000001,-47.850000000000001,-50.850000000000001,-53.850000000000001,-56.850000000000001,-59.850000000000001,-62.850000000000001,-65.850000000000023,-68.85000000000008,-71.850000000000136,-74.850000000000193,-77.85000000000025,-80.850000000000307,-83.850000000000364,-86.850000000000421,-89.850000000000477,-92.850000000000534,-95.850000000000591,-98.850000000000648,-101.8500000000007,-104.85000000000076,-107.85000000000082,-110.85000000000088,-113.85000000000093,-116.85000000000099,-119.85000000000099,-122.85000000000099,-125.85000000000099,-128.85000000000096,-131.85000000000096,-134.85000000000096,-137.85000000000096,-140.85000000000096,-143.85000000000096,-146.85000000000096,-149.85000000000096,-152.85000000000096,-155.85000000000099,-158.85000000000099,-161.85000000000099,-164.85000000000099,-167.85000000000099,-170.85000000000099,-173.85000000000099,-176.85000000000099,-179.85000000000099,-182.85000000000099,-185.85000000000099,-188.85000000000099,-191.85000000000099,-194.85000000000099,-197.85000000000099,-200.85000000000099,-203.85000000000099,-206.85000000000099,-209.85000000000099,-212.85000000000099,-215.85000000000099,-218.85000000000099,-221.85000000000099,-224.85000000000099],"legendgroup":"a","showlegend":true,"type":"scatter","mode":"lines","visible":true,"name":"a","marker":{"color":"rgba(107,157,89,1)","line":{"color":"rgba(107,157,89,1)"}},"textfont":{"color":"rgba(107,157,89,1)"},"error_y":{"color":"rgba(107,157,89,1)"},"error_x":{"color":"rgba(107,157,89,1)"},"line":{"color":"rgba(107,157,89,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
