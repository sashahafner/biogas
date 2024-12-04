# Create Rda files

UQGravSetup <- read.csv('UQ_grav_setup.csv')
UQGravMass <- read.csv('UQ_grav_mass.csv')

save(UQGravSetup, file = '../data/UQGravSetup.rda')
save(UQGravMass, file = '../data/UQGravMass.rda')
