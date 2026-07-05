# print.simulate_stockflow() snapshot: successful simulation

    Code
      print(sim)
    Message
      
      -- Stock-and-Flow Simulation: Susceptible-Infected-Recovered (SIR) -------------
      
      -- Data (first rows) --
    Output
        time infected  recovered susceptible
      1 0.00 1.000000 0.00000000    99999.00
      2 0.01 1.019000 0.00100000    99998.98
      3 0.02 1.038361 0.00201900    99998.96
      4 0.03 1.058089 0.00305736    99998.94
      5 0.04 1.078193 0.00411545    99998.92
    Message
      
      i Access with `as.data.frame()` • Visualise with `plot()`

# print.simulate_stockflow() snapshot: named model

    Code
      print(sim)
    Message
      
      -- Stock-and-Flow Simulation: My SIR Model -------------------------------------
      
      -- Data (first rows) --
    Output
        time infected  recovered susceptible
      1 0.00 1.000000 0.00000000    99999.00
      2 0.01 1.019000 0.00100000    99998.98
      3 0.02 1.038361 0.00201900    99998.96
      4 0.03 1.058089 0.00305736    99998.94
      5 0.04 1.078193 0.00411545    99998.92
    Message
      
      i Access with `as.data.frame()` • Visualise with `plot()`

# print.simulate_stockflow() snapshot: failed simulation

    Code
      print(sim)
    Message
      
      -- Stock-and-Flow Simulation ---------------------------------------------------
      x Simulation failed
      i Inspect the error message with: `x$error_message`

