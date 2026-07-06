# summary() detects stocks not connected to flows

    Code
      print(result)
    Message
      
      -- Stock-and-Flow Model Diagnostics --------------------------------------------
      
      -- Potential problems (3) --
      * Model has no flows.
      > Add flows with `flow()` or `update()`.
      * Stocks not connected to any flow: `Stock1` and `Stock2`.
      * `Stock1` and `Stock2` have an equation of 0.

# summary() warns about zero equations

    Code
      print(result)
    Message
      
      -- Stock-and-Flow Model Diagnostics --------------------------------------------
      
      -- Potential problem (1) --
      * `Stock1` and `Flow1` have an equation of 0.

# print.summary_stockflow() shows header and 'No problems detected!' for valid model

    Code
      print(result)
    Message
      
      -- Stock-and-Flow Model Diagnostics --------------------------------------------
      v No problems detected!

# summary() warns when unit test eqn reference undefined variable

    Code
      print(result)
    Message
      
      -- Stock-and-Flow Model Diagnostics --------------------------------------------
      
      -- Potential problems (3) --
      * Model has no flows.
      > Add flows with `flow()` or `update()`.
      * Stock not connected to any flow: `S`.
      * Unit test reference undefined variable.
        [1] "valid" expr: `drain` is undefined.
      > Update the affected tests or add the missing variables.

