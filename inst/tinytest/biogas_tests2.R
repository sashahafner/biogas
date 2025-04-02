# test values form the high level functions

# predBg
# default methane prediction from COD is stable', {
expect_equal(round(predBg(COD = 1),4), 349.3842)

# default methane prediction from COD is stable with fs argument', {
expect_equal(signif(predBg(COD = 1, fs = 0.1), 6), signif(349.3842*0.9, 6))

# default methane prediction from COD is stable with fd argument', {
expect_equal(signif(predBg(COD = 1, fd = 0.9), 6), signif(349.3842*0.9, 6))

# default methane prediction from COD is stable', {
expect_equal(round(predBg(COD = 1),4), 349.3842)

# default methane prediction from predBg using a formula is stable', {
expect_equal(predBg('C6H10O6'), calcCOD('C6H10O6')*predBg(COD = 1) )


# cumBg.R
# NTS: for the moment everything is tested once, it could be good to separatae tests for - cH4 volume vs biogas volume and rates
# NTS: might also be good to store the test data frames somewhere

# volume molar values:
vmch4 <- 22360.588
vmco2 <- 22263.009

# cumulative sum and rates
# volumetric
# cumulative sum and rates are corrrectly calculated with default values", {
test.vol <- data.frame(id = rep(paste0('R_',1),5), time= c(2, 4, 5, 1, 3), vol = rep(20, 5))

res <- data.frame(id = rep('R_1', 6), time = c(0:5), vol = c(NA, rep(20, 5)), vBg = c(0, rep(20,5)), cvBg = c(0, 20, 40, 60, 80, 100), rvBg = c(NA,rep(20,5)))

expect_equal( cumBg(dat = test.vol), res)


# cumulative sum and rates are corrrectly calculated with default values , except addt0 = FALSE", {
test.vol <- data.frame(id = rep(paste0('R_',1),5), time= c(2, 4, 5, 1, 3), vol = rep(20, 5))
res <- data.frame(id = rep('R_1', 5), time = c(1:5), vol = rep(20, 5), vBg = rep(20,5), cvBg = c(20, 40, 60, 80, 100), rvBg = c(NA,rep(20,4)))
  
expect_equal( cumBg(dat = test.vol, addt0 = FALSE), res)



# cumulative sum, rates and methane volume are corrrectly calculated using one value for comp", {
test.vol <- data.frame(id = rep(paste0('R_',1),5), time= c(2, 4, 5, 1, 3), vol = rep(20, 5))
# wanted result : calculation for each column from data in test.mass
res <- data.frame(id = rep('R_1', 6), time = c(0:5), vol = c(NA, rep(20, 5)), xCH4 = c(NA, rep(0.6, 5)), temperature = c(NA, rep(35, 5)), pressure = c(NA, rep(1, 5)))
res$vBg <- stdVol(c(0, rep(20,5)), temp = 35, pres = 1)
res$vCH4 <- res$vBg*0.6
res$cvBg <- rep(0,6)

for(i in 2:6){
  res[i, 'cvBg'] = res[i, 'vBg'] + res[i-1, 'cvBg'] 
}

res$cvCH4 <- res$cvBg*0.6
res$rvBg <- c(NA, res[2:6, 'vBg']) # because time interval = 1
res$rvCH4 <- c(NA, res[2:6, 'vCH4'])
  
expect_equal(cumBg(dat = test.vol, comp = 0.6, temp = 35, pres = 1 ), res)

# gravimetric
# cumulative sum, rates and methane volume are corrrectly calculated using mass + one required value for comp, temp and pres", {
  # set up data frame to test the function

test.mass <- data.frame(id = rep(paste0('R_',1),6), time= c(2, 4, 5, 1, 3, 0), mass = c(95, 85, 80, 100, 90, 105))
# wanted result : calculation for each column from data in test.mass
res <- data.frame(id = rep('R_1',6) , time = c(0:5), mass = c(105, 100, 95, 90, 85, 80), xCH4 = rep(0.6, 6),  
                  temperature = 35, pressure = 1,
                  mass.tot = c(0,rep(5,5)), cmass.tot = c(0, 5, 10, 15, 20, 25))
res <- data.frame( res, mass2vol(res$mass.tot, xCH4 = 0.6, temp = 35 , pres = 1, value= 'all'))
res <- res[,-match(c('vCO2', 'vN2'), names(res))]
res$cvBg <- rep(0,6)
res$cvBg <- cumsum(res$vBg)

res$cvCH4 <- res$cvBg*0.6
res$rvBg <- c(NA, res[2:6, 'vBg']) # because time interval = 1
res$rvCH4 <- c(NA, res[2:6, 'vCH4'])
  
expect_equal(calcBgGrav(dat = test.mass, data.struct = "long", mass.name = 'mass', comp = 0.6, temp = 35 , pres = 1), res,
               tolerance = 0.002)

