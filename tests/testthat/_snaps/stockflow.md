# print.stockflow() snapshot: empty model

    Code
      print(sfm)
    Message
      
      -- Stock-and-Flow Model --------------------------------------------------------
      i Empty model without any variables.
      
      -- Simulation Settings --
      Time: 0 to 100 seconds (dt = 0.01) • euler • R
      Simulation output: stocks only

# print.stockflow() snapshot: named model

    Code
      print(sfm)
    Message
      
      -- Stock-and-Flow Model: My SIR Model ------------------------------------------
      i Empty model without any variables.
      
      -- Simulation Settings --
      Time: 0 to 100 seconds (dt = 0.01) • euler • R
      Simulation output: stocks only

# print.stockflow() snapshot: SIR model

    Code
      print(sfm)
    Message
      
      -- Stock-and-Flow Model: Susceptible-Infected-Recovered (SIR) ------------------
      3 stocks • 2 flows • 4 constants
      
      -- Stock-Flow Structure --
      infected: + new_infections - new_recoveries
      recovered: + new_recoveries
      susceptible: - new_infections
      
      -- Other Variables --
      Constants: `contact_rate`, `infection_rate`, `recovery_rate`, and
      `total_population`
      
      -- Simulation Settings --
      Time: 0.0 to 20.0 weeks (dt = 0.01) • euler • R
      Simulation output: stocks only

# print.stockflow() snapshot: model with constants

    Code
      print(sfm)
    Message
      
      -- Stock-and-Flow Model --------------------------------------------------------
      1 stock • 1 flow • 1 constant
      
      -- Stock-Flow Structure --
      S: - Flow1
      
      -- Other Variables --
      Constants: `k`
      
      -- Simulation Settings --
      Time: 0 to 100 seconds (dt = 0.01) • euler • R
      Simulation output: stocks only

# print.stockflow() snapshot: default name not shown as title

    Code
      print(sfm)
    Message
      
      -- Stock-and-Flow Model --------------------------------------------------------
      i Empty model without any variables.
      
      -- Simulation Settings --
      Time: 0 to 100 seconds (dt = 0.01) • euler • R
      Simulation output: stocks only

