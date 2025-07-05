# unit_func
module unit_func
using Unitful

# Set or convert unit wrappers per type
function convert_u(x::Unitful.Quantity, unit_def::Unitful.Quantity)
    if Unitful.unit(x) == Unitful.unit(unit_def)
        return x  # No conversion needed
    else
        Unitful.uconvert.(Unitful.unit(unit_def), x)
    end
end

function convert_u(x::Unitful.Quantity, unit_def::Unitful.Units)
    if Unitful.unit(x) == unit_def
        return x  # No conversion needed
    else
        Unitful.uconvert.(unit_def, x)
    end
end

# If x is not a Unitful.Quantity but Float64:
function convert_u(x::Float64, unit_def::Unitful.Quantity)
    x * Unitful.unit(unit_def)
end

function convert_u(x::Float64, unit_def::Unitful.Units)
    x * unit_def
end

end
