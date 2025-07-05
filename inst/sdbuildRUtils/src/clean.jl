# clean
module clean
using Unitful
using DataFrames
using ..custom_func: itp

# Function to save dataframe at specific times
function saveat_func(t, y, new_times)
    # Interpolate y at new_times
    itp(t, y, method = "linear", extrapolation = "nearest")(new_times)
end

function clean_df(solve_out, init_names, times, dt, saveat;
                  intermediaries=nothing, intermediary_names=nothing)
    # Always create df from solve_out and init_names
    df = Unitful.ustrip.(DataFrame(solve_out, [:time; init_names]))

    # If intermediary data was passed, process and append
    if intermediaries !== nothing && intermediary_names !== nothing
      # Error is thrown for dataframe creation of there is only one variable
      # Necessary to add first.() because otherwise the column is a list in R, causing issues in plot(sim)
      if length(intermediary_names) == 1
        intermediary_df = Unitful.ustrip.(DataFrame(:temp => first.(intermediaries.saveval)))
        rename!(intermediary_df, :temp => intermediary_names...)
      else
        intermediary_df = Unitful.ustrip.(DataFrame(intermediaries.saveval, intermediary_names))
      end

      # Remove variables in intermediary_var that are in initial_value_names
      if any(name -> name in intermediary_names, init_names)
        select!(intermediary_df, setdiff(intermediary_names, init_names))
      end

      # Merge df with intermediary values
      df = hcat(df, intermediary_df)
    end

    # Ensure correct number of rows, resample if needed
    if dt != saveat
        # Linearly interpolate to reduce stored values to saveat
    new_times = collect(times[1]:saveat:times[2]) # Create new time vector
    df = DataFrame(Dict(
    :time => new_times,
        [Symbol(col) => saveat_func(df.time, df[!, col], new_times) for col in names(df) if col != "time"]...
    ))
    elseif nrow(df) != length(times[1]:dt:times[2])
        times = Unitful.ustrip.(times)
        new_times = collect(times[1]:dt:times[2])  # use passed dt
        df = DataFrame(Dict(
            :time => new_times,
            [Symbol(col) => saveat_func(df.time, df[!, col], new_times)
             for col in names(df) if col != "time"]...
        ))
    end

    # Convert to long
    long = stack(df, Not(:time), variable_name=:variable, value_name=:value)

    return long
end

function clean_constants(constants)
    constants = (; (name => isa(val, Unitful.Quantity) ? Unitful.ustrip(val) : val for (name, val) in pairs(constants))...)

    # Find keys where values are Float64 or Vector
    valid_keys = [k for k in keys(constants) if isa(constants[k], Float64) || isa(constants[k], Vector)]

    # Convert valid_keys to a tuple for NamedTuple construction
    valid_keys_tuple = Tuple(valid_keys)

    # Reconstruct filtered named tuple
    constants = NamedTuple{valid_keys_tuple}(constants[k] for k in valid_keys)

end


function clean_init(init, init_names)
    Dict(init_names .=> Unitful.ustrip.(init))
end
end
