# ensemble
module ensemble
using Unitful
using Statistics
using DataFrames
using ..clean: clean_df
function all_timestep_stats(intermediaries, key = :saveval, qs = [0.05, 0.95])
    n_steps = length(getfield(first(intermediaries), key))  # number of time steps
    n_vars = length(first(getfield(first(intermediaries), key)))  # number of variables

    # Preallocate storage
    means   = zeros(n_vars, n_steps)
    vars_   = zeros(n_vars, n_steps)
    medians = zeros(n_vars, n_steps)
    qlows   = zeros(n_vars, n_steps)
    qhighs  = zeros(n_vars, n_steps)

    for i in 1:n_steps
        vals = [collect(getfield(sv, key)[i]) for sv in intermediaries]
        mat = reduce(hcat, vals)  # rows: variables, cols: trajectories

        means[:, i]   .= mapslices(mean, mat; dims=2)[:]
        vars_[:, i]   .= mapslices(var, mat; dims=2)[:]
        medians[:, i] .= mapslices(Statistics.median, mat; dims=2)[:]
        qlows[:, i]   .= mapslices(x -> Statistics.quantile(x, qs[1]), mat; dims=2)[:]
        qhighs[:, i]  .= mapslices(x -> Statistics.quantile(x, qs[2]), mat; dims=2)[:]
    end

    return (
        mean = means,
        var = vars_,
        median = medians,
        qlow = qlows,
        qhigh = qhighs
    )
end

function summary_to_long(stats, times, var_names)
    dfs = DataFrame[]
    # for (statname, matrix) in stats
    for statname in keys(stats)

        df = permutedims(DataFrame(getfield(stats, statname), :auto))
        rename!(df, var_names)

        if (statname == first(keys(stats)))
            df.time = times
        end

        long = stack(df, var_names; variable_name=:variable, value_name=statname)

        # Remove the variable column
        if (statname != first(keys(stats)))
            select!(long, Not(:variable))
        end

        # long.statistic .= statname
        push!(dfs, long)
    end

    hcat(dfs...)
end

function create_ensemble_summ(solve_out, init_names, intermediaries, intermediary_names, qs = [0.05, 0.95])

    stats = all_timestep_stats(solve_out, :u, qs);
    summ = summary_to_long(stats, solve_out[1].t, init_names)

    if !isnothing(intermediaries)
        stats = all_timestep_stats(intermediaries, :saveval, qs);
        summ = vcat(summ, summary_to_long(stats, intermediaries[1].t, intermediary_names))
    end

    return summ
end


function ensemble_to_df(solve_out, init_names, times, dt, saveat;
                   intermediaries=nothing, intermediary_names=nothing)
    n = length(solve_out)
    dfs = Vector{DataFrame}(undef, n)

    for i in 1:n
        df = clean_df(
            solve_out[i], init_names, times, dt, saveat;
            intermediaries = isnothing(intermediaries) ? nothing : intermediaries[i],
            intermediary_names = intermediary_names
        )
        df.simulation = fill(i, nrow(df))
        dfs[i] = df
    end

    return reduce(vcat, dfs)
end
end
