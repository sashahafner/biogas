# Overview
The biogas package is an R package for biogas research. 
It provides tools for processing biogas data, predicting biogas production, making conversions, and planning experiments. 
For example, the `calcBgVol()` function can be used for calculating biochemical methane potential from original measurements. 
The `calcCOD()` function can calculate the chemical oxygen demand of a compound based on a chemical formula.
And the `planBMP()` function can suggest substrate or inoculum quantities for a biochemical methane potential (BMP) experiment based on multiple constraints.

# An example
Much of the biogas package is dedicated to processing of laboratory measurements, but those functions are difficult to demonstrate in a short example.
So, an alternative is the `predBg()` microbial stoichiometry function.
With it, the theoretical maximum BMP of, say, cellulose, could be calculated with

```
> predBg("C6H10O5")
[1] 413.7274
```

and we could get a lot more information, including an estimate of biogas composition as affected by pH, for a 5:1 mixture of waste paper and waste oil with 

```
> predBg(mcomp = c(C6H10O5 = 5/6, C54H100O7 = 1/6), mass = 1,
fd = 0.8, fs = 0.1, conc.sub = 50, pH = c(6.5, 7, 8.5),
temp = 35, value = "all")
                      form mass mol.mass      moles      COD  fs  fe  fd
1 C1.52618H2.61516O1.00000    1  36.9655 0.02705225 1.454303 0.1 0.9 0.8
2 C1.52618H2.61516O1.00000    1  36.9655 0.02705225 1.454303 0.1 0.9 0.8
3 C1.52618H2.61516O1.00000    1  36.9655 0.02705225 1.454303 0.1 0.9 0.8
  conc.sub temp  pH      hydro      fCH4      xCH4     vCH4      vCO2      vBg
1       50   35 6.5 0.09279292 0.5566161 0.5689900 365.8395 275.91411 641.7536
2       50   35 7.0 0.09279292 0.5566161 0.5864017 365.8395 256.90636 622.7459
3       50   35 8.5 0.09279292 0.5566161 0.8921576 365.8395  44.02904 409.8686
       mCH4      mCO2     mCO2Bg   mCO2.sol       cTIC      m.bio      N.req
1 0.2624617 0.5735654 0.54543300 0.02813242 0.03196140 0.08225028 0.01018521
2 0.2624617 0.5735654 0.50785807 0.06570735 0.07465048 0.08225028 0.01018521
3 0.2624617 0.5735654 0.08703757 0.48652785 0.55274693 0.08225028 0.01018521
```

See the vignettes and help files for more details.

# Getting started
The biogas package is available from CRAN, and so can be installed directly in R with:

```
install.packages("biogas")
```

To see available vignettes, use

```
vignette(package = 'biogas')
```

and then, e.g., 

```
vignette('calcBgVol_function')
```

or 

```
vignette('predBg_function')
```

Alternatively, download the vignettes from the [CRAN page](https://cran.r-project.org/package=biogas).

# More information
* Open-access paper describing the package: <https://doi.org/10.1016/j.softx.2018.06.005>
* Web application interface to the package (OBA): <https://biotransformers.shinyapps.io/oba1/>
* YouTube playlist for OBA (shows some of what the biogas package can do): <https://www.youtube.com/playlist?list=PLt5lRUaaL8JMKDyuRIrKkuloRQk6YmYhn>
* Mailing list: sasha.hafner at bce.au.dk

