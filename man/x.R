
library(biogas)
data('strawMass')
data('strawComp')
data('strawSetup')

cbg0 <- calcBgGrav(strawMass,
                   comp = strawComp, temp = 35, pres = 1.5,
                   data.struct = 'long', id.name = 'bottle',
                   time.name = 'time', mass.name = 'mass',
                   xCH4.name = 'xCH4')
warnings()

cbg <- calcBgGrav(strawMass,
                  comp = strawComp, temp = 35, pres = 1.5,
                  data.struct = 'long', id.name = 'bottle',
                  time.name = 'time', mass.name = 'mass',
                  xCH4.name = 'xCH4', extrap = TRUE)

head(cbg)

