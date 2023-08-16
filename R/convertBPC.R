# Convert BPC AMPTS II files to something that works well for OBA

# Examples
# infile <- 'report_2023-06-18_1525.csv'
# indir <- 'inputs'
# convertBPC(indir = 'inputs')

convertBPC <- function(infile = NULL, indir = NULL, outname = gsub('\\.csv|.+\\/|.+\\\\', '', infile)) {

  if (!is.null(indir)) {

    ff <- list.files(indir, full.names = TRUE)

    for (i in ff) {
      convertBPC(infile = i)
    }

    return(invisible(NULL))

  }

  lns <- readLines(infile)
  # Volume data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- grep('Day|Hour', lns)

  vol <- read.csv(infile, header = TRUE, skip = i - 1)

  # Remove rate columns
  vol <- vol[, !grepl('Flow..Nml.day', names(vol))]

  # Fix column names
  names(vol) <- gsub('\\.Volume\\.+Nml.', '', names(vol))
  names(vol) <- gsub('\\.+', '_', names(vol))
  names(vol) <- gsub('_$', '', names(vol))

  # Remove X
  vol <- vol[, names(vol) != 'X']

  # Setup data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- grep('Flow Cell nr\\.', lns)

  setup <- read.csv(infile, header = TRUE, skip = i, nrows = 5)

  # Fix column names
  names(setup) <- gsub('\\.Volume\\.+Nml.', '', names(setup))
  names(setup) <- gsub('\\.+', '_', names(setup))
  names(setup) <- gsub('_$', '', names(setup))

  # Remove X
  setup <- setup[, names(setup) != 'X']
  m <- t(setup)[-1,]

  setup <- data.frame(ID = rownames(m), descrip = gsub('_1$|_2$|_3$', '', rownames(m)), 
                      inoc_VS_mass = as.numeric(m[, 2]), 
                      sub_VS_mass = as.numeric(m[, 1]), mass_type = m[, 3], 
                      headspace = as.numeric(m[, 4]))

  #openxlsx::write.xlsx(list(Setup = setup, Biogas = vol), file = paste0(outname, '_combined.xlsx'))
  write.csv(setup, paste0(outname, '_setup.csv'), row.names = FALSE)
  write.csv(vol, paste0(outname, '_biogas.csv'), row.names = FALSE)

  return(invisible(NULL))
}
