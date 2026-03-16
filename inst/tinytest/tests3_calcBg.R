# Tests for calcBg* family and summBg
# Expected values taken from help file examples run against current code.

library(biogas)

# Standard conditions used throughout
options(temp.std = 0, pres.std = 1, unit.temp = 'C', unit.pres = 'atm')

# calcBgVol ---------------------------------------------------------------
# From help file example: s3lcombo, longcombo structure, extrap = TRUE

data('s3lcombo')
cbg.vol <- calcBgVol(s3lcombo, temp = 25, pres = 1,
                     id.name = 'id', time.name = 'time.d',
                     vol.name = 'vol.ml', comp.name = 'xCH4',
                     extrap = TRUE, quiet = TRUE)

final.vol <- tapply(cbg.vol$cvCH4, cbg.vol$id, tail, 1)

expect_equal(final.vol[['D']], 607.4877, tolerance = 0.001)
expect_equal(final.vol[['E']], 578.8239, tolerance = 0.001)
expect_equal(final.vol[['F']], 604.1903, tolerance = 0.001)

# calcBgMan ---------------------------------------------------------------
# From help file example: sludgeTwoBiogas, gauge pressure, longcombo

data('sludgeTwoBiogas')
data('sludgeTwoSetup')
cbg.man <- calcBgMan(sludgeTwoBiogas, temp = 30,
                     id.name = 'id', time.name = 'time.d',
                     pres.name = 'pres', comp.name = 'xCH4n',
                     temp.init = 30, pres.resid = 0, pres.init = 0,
                     headspace = sludgeTwoSetup, vol.hs.name = 'vol.hs',
                     pres.amb = 1013, absolute = FALSE, unit.pres = 'mbar',
                     quiet = TRUE)

final.man <- tapply(cbg.man$cvCH4, cbg.man$id, tail, 1)

expect_equal(final.man[['1']],  166829.5, tolerance = 0.1)
expect_equal(final.man[['7']],  262494.3, tolerance = 0.1)
expect_equal(final.man[['14']], 445180.1, tolerance = 0.1)

# calcBgGrav --------------------------------------------------------------
# From help file example: UQGravBiogas, longcombo structure

data('UQGravBiogas')
cbg.grav <- calcBgGrav(UQGravBiogas, temp = 35, pres = 1.5,
                       id.name = 'id', time.name = 'day',
                       mass.name = 'mass.final', xCH4.name = 'xCH4',
                       quiet = TRUE)

final.grav <- tapply(cbg.grav$cvCH4, cbg.grav$id, tail, 1)

expect_equal(final.grav[['C1']], 494.4483, tolerance = 0.001)
expect_equal(final.grav[['C2']], 497.9162, tolerance = 0.001)
expect_equal(final.grav[['I1']],  97.9465, tolerance = 0.001)

# calcBgGD ----------------------------------------------------------------
# From help file example: UQGDBiogas
# Note: comp.sub = 'lim' required because t=0 produces xCH4 = 0 which is
# at the boundary of the default comp.lim = c(0, 1)

data('UQGDBiogas')
cbg.gd <- calcBgGD(UQGDBiogas,
                   temp.vol = 20, pres.vol = 1013.25,
                   temp.grav = 30, pres.grav = 1500,
                   id.name = 'id', vol.name = 'vol',
                   m.pre.name = 'mass.init', m.post.name = 'mass.final',
                   time.name = 'time.d', unit.pres = 'mbar',
                   comp.sub = 'lim')

final.gd <- tapply(cbg.gd$cvCH4, cbg.gd$id, tail, 1)

expect_equal(final.gd[['C1']], 814442.4, tolerance = 1)
expect_equal(final.gd[['C2']], 811869.2, tolerance = 1)
expect_equal(final.gd[['I1']], 163111.7, tolerance = 1)

# summBg ------------------------------------------------------------------
# From help file example: vol/comp/setup datasets, inoculum subtraction,
# normalization by VS, evaluated at day 30

data('vol')
data('comp')
data('setup')

cbg <- calcBgVol(vol, comp = comp, temp = 20, pres = 1, 
                 data.struct = 'long',
                 id.name = 'id', time.name = 'days', comp.name = 'xCH4', 
                 vol.name = 'vol', extrap = TRUE)

s <- summBg(vol = cbg, setup = setup, time.name = 'days',
            inoc.name = 'inoc', inoc.m.name = 'minoc',
            when = 30, norm.name = 'mvs.sub', quiet = TRUE)

expect_equal(s[s$descrip == 'A',     'mean'],  157.5845, tolerance = 0.001)
expect_equal(s[s$descrip == 'B',     'mean'],  124.7890, tolerance = 0.001)
expect_equal(s[s$descrip == 'cellu', 'mean'],  417.9794, tolerance = 0.001)

# xCH4 argument tests --------------------------------------------------------

# calcBgVol: wide + xCH4 = 1 at STP with dry = TRUE, so cvCH4 must equal cvBg exactly
data('feedVol')
cbg.vol.xCH4.wide <- calcBgVol(feedVol, xCH4 = 1, temp = 0, pres = 1,
                                interval = FALSE, data.struct = 'wide',
                                id.name = 'id', time.name = 'time.d', vol.name = '1',
                                dry = TRUE, quiet = TRUE)
expect_equal(cbg.vol.xCH4.wide$cvCH4, cbg.vol.xCH4.wide$cvBg)

# calcBgVol: error when both comp and xCH4 provided
expect_error(calcBgVol(s3lcombo, comp = s3lcombo, xCH4 = 0.65,
                       temp = 25, pres = 1,
                       id.name = 'id', time.name = 'time.d', vol.name = 'vol.ml'))

# calcBgVol: longcombo + xCH4 gives same result as equivalent constant xCH4 column
dat.const.xCH4 <- s3lcombo
dat.const.xCH4$xCH4 <- 0.65
cbg.vol.xCH4.lc <- calcBgVol(s3lcombo[, !names(s3lcombo) %in% 'xCH4'],
                              xCH4 = 0.65, temp = 25, pres = 1,
                              id.name = 'id', time.name = 'time.d',
                              vol.name = 'vol.ml', quiet = TRUE)
cbg.vol.col.lc  <- calcBgVol(dat.const.xCH4, temp = 25, pres = 1,
                              id.name = 'id', time.name = 'time.d',
                              vol.name = 'vol.ml', comp.name = 'xCH4', quiet = TRUE)
expect_equal(cbg.vol.xCH4.lc$cvCH4, cbg.vol.col.lc$cvCH4)

# calcBgGrav: xCH4 arg gives same result as equivalent constant xCH4 column
dat.no.xCH4.grav <- UQGravBiogas[, !names(UQGravBiogas) %in% 'xCH4']
dat.const.xCH4.grav <- UQGravBiogas
dat.const.xCH4.grav$xCH4 <- 0.65
cbg.grav.xCH4 <- calcBgGrav(dat.no.xCH4.grav, xCH4 = 0.65, temp = 35, pres = 1.5,
                             id.name = 'id', time.name = 'day',
                             mass.name = 'mass.final', quiet = TRUE)
cbg.grav.col  <- calcBgGrav(dat.const.xCH4.grav, temp = 35, pres = 1.5,
                             id.name = 'id', time.name = 'day',
                             mass.name = 'mass.final', xCH4.name = 'xCH4', quiet = TRUE)
expect_equal(cbg.grav.xCH4$cvCH4, cbg.grav.col$cvCH4)
