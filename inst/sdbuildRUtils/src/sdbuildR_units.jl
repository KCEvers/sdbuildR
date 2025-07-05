# Define custom units; register after each unit as some units may be defined by other units
module sdbuildR_units
	using Unitful
	@unit common_yr "common_yr" common_yr u"365d" false
	Unitful.register(sdbuildR_units)
	@unit common_quarter "common_quarter" common_quarter u"365/4*d" false
	Unitful.register(sdbuildR_units)
	@unit common_month "common_month" common_month u"365/12*d" false
	Unitful.register(sdbuildR_units)
	@unit quarter "quarter" quarter u"1/4*yr" false
	Unitful.register(sdbuildR_units)
	@unit month "month" month u"1/12*yr" false
	Unitful.register(sdbuildR_units)
	@unit quart "quart" quart u"946.35cm^3" false
	Unitful.register(sdbuildR_units)
	@unit tonne "tonne" tonne u"1000kg" false
	Unitful.register(sdbuildR_units)
	@unit ton "ton" ton u"907.18474kg" false
	Unitful.register(sdbuildR_units)
	@unit atom "atom" atom u"1/6.02214076e23mol" false
	Unitful.register(sdbuildR_units)
	@unit molecule "molecule" molecule u"1/6.02214076e23mol" false
	Unitful.register(sdbuildR_units)
	@unit US_gal "US_gal" US_gal u"0.003785411784m^3" false
	Unitful.register(sdbuildR_units)
	@unit fluidOunce "fluidOunce" fluidOunce u"29.5735295625mL" false
	Unitful.register(sdbuildR_units)
	@unit EUR "EUR" EUR u"1" false
	Unitful.register(sdbuildR_units)
	@unit USD "USD" USD u"1" false
	Unitful.register(sdbuildR_units)
	@unit GBP "GBP" GBP u"1" false
	Unitful.register(sdbuildR_units)
	@unit deg "deg" deg u"pi/180" false
	Unitful.register(sdbuildR_units)
	@unit ohm "ohm" ohm u"1V/A" false
	Unitful.register(sdbuildR_units)
	@unit reduced_Planck_constant "reduced_Planck_constant" reduced_Planck_constant u"h/2pi" false
	Unitful.register(sdbuildR_units)
	@unit superconducting_magnetic_flux_quantum "superconducting_magnetic_flux_quantum" superconducting_magnetic_flux_quantum u"h/(2q)" false
	Unitful.register(sdbuildR_units)
	@unit degF "degF" degF u"(45967//100)Ra" false
	Unitful.register(sdbuildR_units)
	@unit degC "degC" degC u"(27315//100)K" false
	Unitful.register(sdbuildR_units)
	@unit Stefan_Boltzmann_constant "Stefan_Boltzmann_constant" Stefan_Boltzmann_constant u"pi^2*k^4/(60*reduced_Planck_constant^3*c^2)" false
	Unitful.register(sdbuildR_units)
	@unit anghertz "anghertz" anghertz u"2pi/s" false
	Unitful.register(sdbuildR_units)
	@unit magnetic_constant "magnetic_constant" magnetic_constant u"4pi*(1//10)^7*H/m" false
	Unitful.register(sdbuildR_units)
	@unit electric_constant "electric_constant" electric_constant u"1/(μ0*c^2)" false
	Unitful.register(sdbuildR_units)
	@unit Bohr_magneton "Bohr_magneton" Bohr_magneton u"q*reduced_Planck_constant/(2*me)" false
	Unitful.register(sdbuildR_units)
	@unit Rydberg_constant "Rydberg_constant" Rydberg_constant u"10_973_731.568_160/m" false
	Unitful.register(sdbuildR_units)
end


