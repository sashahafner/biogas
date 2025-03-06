# biogas 1.63.0
2025 March 6
## BUG FIXES
* `calcBgVol()` bug for wide data with separate `vol` and `comp` data frames fixed. 
Earlier versions merged on column order instead of column names.

* `calcBgVol()` bug for biogas volume given as zero.
Earlier versions replaced entered composition with zero.

# biogas 1.60.0
2024 December 4
## NEW FEATURES
* `calcBgGrav()` New function for processing gravimetric BMP data. 
Meant as a replacement for gravimetric processing with `cumBg()`
* `fitFOM()` New function for fitting first-order models

## BUG FIXES
* `summBg()` and `cumBg()` warnings about missing columns fixed (thanks to Jonas Ohlsson, @jonasoh)
* Problem found with R-devel related to `options(scipen...)` ([issue #61](https://github.com/sashahafner/biogas/issues/61))
* Other minor bug fixes

# biogas 1.23.1
2020 January
## BUG FIXES
* Problems found with R-devel with `if` statements and logical coercion.

# biogas 1.23.0
2019 November
## NEW FEATURES
* calcBgMan(), calcBgVol()
New simpler functions for BMP data processing. These are replacements
for volumetric and manometric calculations with cumBg()
* calcBgGD()
Completely new function for gas density BMP (GD-BMP) calculations.

# biogas 1.14.1
2019 April 10
## NEW FEATURES

* cumBg()
Mixed cumulative/interval volumetric calculations with argument 
empty.name and "f1" option of imethod argument.

New 'rh.resid' argument can be used for different assumptions about 
relative humidity in bottle headspace after venting in manometric
calculations.

A new 'quiet' argument suppresses messages, and is useful when 
processing a large number of data sets at once.

* summBg()
New argument 'rate.crit' allows use of gross production rates in 
relative duration calculations (e.g., following VDI).

Relative and fixed duration criteria can be combined with arguments
'when' and 'when.min' (a new argument). For example, perhaps you want
"1p3d" but at least 20 days.

Now it is possible to calculate BMP (or other summaries) for multiple
cumulative methane data frames simultaneously by using a list
for the vol argument. Useful for evaluation of uncertainty in 
cumulative methane calculations.

The vol.name argument can now be used for multiple response 
variables (e.g., both cumulative methane and biogas) in one call.


## BUG FIXES

* cumBg()
Corrected problem with error message when comp was requested for 
data.struct = "longcombo" even though it was not needed.

Other minor bug fixes in cumBg().


# biogas 1.10.3
2018 May 16 
## MINOR CHANGES
biogas package help file and startup message have been removed.

# biogas 1.10.0
2018 March 2
## BUG FIXES

* summBg()
Corrected another problem with if() check with length > 1.


# biogas 1.10.0
2018 February 5
## BUG FIXES

* summBg()
Corrected problem with if() check with length > 1.

## NEW FEATURES

* summBg()
Changes to error propagation so it is now based on standard error instead of 
standard deviation. Now results are correct even when number is replicates is
unequal for inoculum bottles, substrate bottles, or VS determination.

* planBMP()
New function for planning BMP experiments.


# biogas 1.9.0
2018 January 3
## BUG FIXES

* predBg()
Corrected error for value = "reactionc" where substrate (reactant) was omitted 
in some cases.

## NEW FEATURES

* predBg()
New ash component for mcomp argument.

* Units (applies to multiple functions)
Add mbar for pressure.

* stdVol() and functions that call it
Standard conditions are automatically converted to specified units now.

* summBg()
Added show.more argument to see additional ouput. Added show.rates argument to
facilitate checking rates for 1% etc. criterion. Changed specification of
automatic selection of BMP evaluation time based on relative rate. Now any rate
and any duration can be used, e.g., when = "1p3d" for 1% per day for 3 d. Added
quiet argument (default FALSE) to supress warnings.

## OTHER CHANGES

* predBg()
Slight change in protein composition for mcomp to match ADM1.

* summBg()
Slight change to error propagation.


# biogas 1.8.1
2017 August 11
## BUG FIXES

* Removed reference to two LaTeX packages that were causing problems in
a vignette.

# biogas 1.8.0
2017 August 9
## NEW FEATURES

* summBg()
New "0.5p" option for the when argument can be used to select those times where 
methane rate drops below 0.5% of cumulative production per day.

## BUG FIXES

* calcCOD()
Fixed an error that applied to vectorized calculations.

* cumBg()
Minor fixes.


# biogas 1.7.0
2017 February 24
## NEW FEATURES

* Loading message
When biogas is loaded a short message is displayed. Our hope to is to make more users 
aware of the mailing list and OBA, a web app interface to the biogas package.

* cumBg()
New "absolute GC" method based on molar quantity of methane. Set dat.type to "gca" 
to use it. 

Added warnings for missing observations.

* summBg()
Added warnings for missing observations.

When "1p" option is used but methane production rates exceed the 1% criterion, the 
function throws an error. To help identify those bottles that haven't met the 
criterion, set when = "1p" and show.obs = TRUE to see relative rates for all bottles 
(rrvCH4 column).

## BUG FIXES

* cumBg()
Fixed bug in dat.struct = "wide" option that caused problems when the number of 
columns exceeded the number of rows. 

* summBg()
Fixed some problems with "1p" option.

## ACKNOWLEDGEMENTS

Ilona Sárvári Horváth (University of Borås, Sweden) provided details on the 
"absolute GC" method as well as example data for testing.

Christof Holliger (École polytechnique fédérale de Lausanne, Switzerland), Konrad 
Koch (Technische Universität München, Germany), and Hélène Carrere (INRA 
Laboratoire de Biotechnologie de l'Environnement, France) provided data for testing
and suggestions related to cumBg().


# biogas 1.6.0
2016 December 16
## NEW FEATURES

* cumBg()
New dry argument (dry = FALSE by default) so that standardised gas volumes can be 
used (e.g., AMPTS II data).

If any CH4 concentration values are > 1, all are divided by 100. This allows the use
of CH4 concentrations as % vol (% mol) without division first.

* sumBg()
Standard deviation of BMP or similar summary is now calculated even with only one 
inoculum-only bottle, with a warning.

* Vignette
Slight updates to "Getting started" vignette on data structures (more details in 
cumBg() help file).

## BUG FIXES

* cumBg()
When gauge = FALSE, pres.init should now be gauge pressure (pres.amb must be absolute
pressure).

Fixed a bug in id column name for wide format.

* sumBg()
Improved when = '1p' option (1% criterion). Criterion is now: rate <1% of cumulative
CH4 per day for current observation and all following observations, and observation 
is >= 3 days from end of incubation.

Other problem with the when = '1p' option apparent only when rates varied
significantly among replicates was fixed.

* Other minor fixes

## REMOVED FUNCTIONALITY
* summBg()
Removed default value for inoc.m.name argument to avoid mistakes.


# biogas 1.5.0 
2016 November 17
## NEW FEATURES

* cumBg()
Accepts two new data structures in addition to the original "long" structure: 
"wide" and "longcombo". See data.struct argument. Five new example data sets
have been added for use in examples to demonstrate.

Missing values (NAs) for biogas composition are now OK. They will be automatically 
dropped (and replaced with interpolated values if needed).

The equation for water vapor pressure calculation has been changed. Resulting changes 
in standardized volume will change very slightly as a result (<0.07%). (Thanks to 
Izabel Kronenberg.)

New pres.amb argument for use with manometric measurements and gauge pressure.

* summBg()
Now possible to include effect of uncertainty in substrate mass (standard deviation) 
on final standard deviation in methane yield. See norm.sd.name argument.

Instead of specifying a fixed time at which to evaluate biogas production, the 
function can find the time when the production rate drops below 1% of cumulative 
production per day. This value can be different for each substrate and can be applied 
with or without inoculum subtraction. To use, set when = "1p".

## BUG FIXES

* cumBg()
Corrected calculation of initial standardized gas volume for manometric method 
(dat.type = "pres").

* Other minor fixes


# biogas 1.4.0 
2016 May 24
## NEW FEATURES

* cumBg()
New manometric method now available (use dat.type = "pres").

New option to use either cumulative or interval (default) data. See 'interval' argument.

## BUG FIXES
* summBg()
Now returns error if 'when' argument is greater than available times for all reactors.

* cumBg()
Fixed error in cmethod = "total" calculation method. Now headspace volume is correctly 
added to cumulative production.

* Other minor fixes

# biogas 1.3.0 
2016 April 14
## NEW FEATURES

* molMass()
New elements added to database. All elements with at least one stable isotope are now included. 
Examples:
```
molMass('CdSiO3')
molMass('FeSO4(H2O)7')
```

* molMass() and predBg()
Additional flexibility in chemical formula specification. Both of the following are acceptable.
molMass("H3C(CH2)5COOH")
molMass("(C6H12O6)0.25 (H3COOH)0.75")

* summBg()
New option for 'when' argument: set to "meas" to return a row for each measurement 
time (these may differ among reactors).

New 'sort' argument to sort results by reactor ID and time (default) or to match original order 
of reactor ID in setup data frame.

* predBg()
New options for 'value' argument to just return chemical reactions: use "reactionn" 
for a numeric reaction and "reactionc" for a character version.

# biogas 1.2.0 
2015 November 13
## NEW FEATURES

* vol2mol()
New function for converting measured volume of CH4, CO2, and some other pure
gases to moles.

* predBg()
The 'mcomp' argument is more flexible now. Chemical formulas can now be mixed
with macromolecule group names. See help file and predBg vignette for examples.

New option for 'value' argument: "reaction". Returns a named numeric vector with
stoichiometric coefficients for the overall reaction, normalized to 1 mol of
substrate.

The 'mass' argument will be set to the sum of 'mcomp' unless it is set separately.
Previously the default value of 1.0 g was used even if 'mcomp' did not sum to
1.0. See help file and predBg vignette for more information.

* New vignette "Predicting methane and biogas production with the biogas
package" (referred to as the "predBg vignette" above).

## BUG FIXES

* predBg()
'mcomp' is now mass-based (as originally intended). Earlier versions were
mole-based. See help file and predBg vignette for more information.

* Other minor bug fixes.

## NEW PUBLICATION

* The gravimetric method used in mass2vol() is described in detail in a new
publication in Biomass and Bioenergy: Hafner, S.D., Rennuit, C., Triolo, J.M.,
Richards, B.K. 2015. Validation of a simple gravimetric method for measuring
biogas production in laboratory experiments. Biomass and Bioenergy 83, 297-301.
Send us an email if you would like a pdf.

# biogas 1.1.0
2015 July 29
## MAILING LIST

There is now a biogas package mailing list. To subscribe, send a message with
the subject "biogas: SUBSCRIBE" to saha@kbm.sdu.dk. 

## NEW FEATURES

* New vignette "Getting started with the biogas package"

* Standard conditions and units
Standard temperature and pressure and units can now be set globally using
options().  This applies to stdVol(), cumBg(), vol2mass(), and mass2vol().
This option is strongly recommended for users not using default values (1.0
atm (101325 Pa) and 0 degrees C). Also, stdVol() (and functions that use it)
now displays the standard temperature and pressure. 

* Checking for input errors
All functions now include checks for the class and in some cases, the value,
of the arguments. Functions where a column name is specified (cumBg() and
summBg()) now check that the specified columns exist in the input data frames. 

* cumBg()
It is now possible to use a single composition value for each reactor, if it
is given in a data frame with no time column (just reactor ID and xCH4). If
this is the case but there is more than one composition value per reactor, an
error is thrown.

For the gravimetric method, the initial headspace correction can now be
applied (to the first interval only) using the headspace, vol.hs.name,
headcomp, and temp.init arguments.

* summBg()
Time can be totally ignored if there is a single observation for each reactor
and 'when' is set to NULL.

## REMOVED FUNCTIONALITY

* Measurement pressure and temperature
Pressure and temperature (pres and temp arguments) both must now be specified
in order to determine standardized volumes for any function with a pres or
temp argument, so stdVol(), cumBg(), vol2mass(), mass2vol(). Previous versions
had a default pressure of 1.0 atm. Omitting pres or temp will now throw an
error or return non-standardized results (with a message).

* Column names in data frame arguments for cumBg() and summBg()
Names of ID, time, volume, composition, and other columns in input data frame
must now be specified using the `*.name` arguments. The functions will no
longer match based on position. This change is meant to avoid errors caused by
incorrect positional matching when the user tried to use `*.name`.

## BUG FIXES

* predBg()
Fixed incorrect calculation of mass when mol argument was used for input.

## OTHER CHANGES

* molMass()
The function now returns all digits--no rouding is done. 

* cumBg()
Calculation of methane volume in the volumetric method (default) now corrects
for the (very small, < 0.4%) difference in molar volume between CH4 and biogas
(this was already the case for the gravimetric method).

# biogas 1.0.1
2015 May 28 

summBg 
* Output data frame now includes value of 'when' argument that was
specified in the call.

* Will now work when only a single response ('vol.name') value is available,
but only if 'when' exactly matches the time or if 'extrap' is set to TRUE.

* The 'when' argument can now be a vector, i.e., a single call can return
results for different times.

* Separate contributions of substrate and inoculum are returned when 'show.obs'
is set to TRUE.



