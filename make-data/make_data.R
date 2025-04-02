# Create Rda files

UQGravSetup <- read.csv('UQ_grav_setup.csv')
UQGravBiogas <- read.csv('UQ_grav_mass.csv')

save(UQGravSetup, file = '../data/UQGravSetup.rda')
save(UQGravBiogas, file = '../data/UQGravBiogas.rda')
