# R function to import to NHANES and format it

get_nhanes <- function(years="2011", sections=NULL, files=NULL) {

  # load file with lists of available datasets
  load("nhanes_files.rda")
  
  ### DAMICO
  # set the number of digits shown in all output
  
  options( digits = 15 )
  
  # remove the # in order to run this install.packages line only once
  # install.packages( "survey" )
  
  library(foreign) # load foreign package (converts data files into R)
  library(survey)  # load survey package (analyzes complex design surveys)
  
  # set R to produce conservative standard errors instead of crashing
  # http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
  options( survey.lonely.psu = "adjust" )
  # this setting matches the MISSUNIT option in SUDAAN
  ## / DAMICO
  
  # loop through year and create demographics URLS
  # also need to change the letter after demo_ based on the year
  # for now, can just specify one year and create URL
  
  # loop through years and create year.str vector
  year.str <- NULL
  for (i in 1:length(years)) {
  year.str[i] <- switch(years[i],
                     "2011" = "2011-2012",
                     "2009" = "2009-2010",
                     "2007" = "2007-2008",
                     "2005" = "2005-2006",
                     "2003" = "2003-2004",
                     "2001" = "2001-2002",
                     "1999" = "1999-2000"
                     )
  }
  
  # create correct ending for variables based on year
  var_end <- NULL
  for (i in 1:length(years)) {
    var_end[i] <- switch(years[i],
                          "2011" = "_g.xpt",
                          "2009" = "_f.xpt",
                          "2007" = "_e.xpt",
                          "2005" = "_d.xpt",
                          "2003" = "_c.xpt",
                          "2001" = "_b.xpt",
                          "1999" = ".xpt"
    )
  }
  
  # URL for demographic file
  NHANES.demog.file.loc <- NULL
  
  for (i in 1:length(years)) {
  NHANES.demog.file.loc[i] <- 
    paste0("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/nhanes/", year.str[i], "/demo", var_end[i])
  }
  
  # URLs for other file locations
  
  selected_files <- as.character(nhanes_files_nosubsets[(nhanes_files_nosubsets$section %in% sections) | 
                                             (nhanes_files_nosubsets$filename %in% files), 3])
  
  # Generate URLs
  URLs <- matrix(data = NA, nrow=length(selected_files), ncol=length(years))
  filenames <- matrix(data = NA, nrow=length(selected_files), ncol=length(years))
  for (i in 1:length(years)) {
    for (j in 1:length(selected_files)) {
      URLs[j, i] <- paste0("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/nhanes/", year.str[i], "/", selected_files[j], var_end[i])
      filenames[j, i] <- paste0("NHANES.", years[i], ".", selected_files[j], ".df")
    }
  }
  
  # DAMICO
  # download and importation function
  download.and.import.any.nhanes.file <-  	# this line gives the function a name
    function( ftp.filepath ){ 		# this line specifies the input values for the function
      
      
      # create a temporary file
      # for downloading file to the local drive
      tf <- tempfile()
      
      
      # download the file using the ftp.filepath specified
      download.file( 
        # download the file stored in the location designated above
        ftp.filepath ,
        # save the file as the temporary file assigned above
        tf , 
        # download this as a binary file type
        mode = "wb"
      )
      
      # the variable 'tf' now contains the full file path on the local computer to the specified file
      
      read.xport( tf )			# the last line of a function contains what it *returns*
      # so by putting read.xport as the final line,
      # this function will return an r data frame
    }
  # /DAMICO 
  
  # download all files
  
  # download demographic file
  name <- NULL
  for (i in 1:length(NHANES.demog.file.loc)) {
    name[i] <- paste0("NHANES.", years[i], ".demog.df")
    assign(name[i], download.and.import.any.nhanes.file( NHANES.demog.file.loc[i] ))
  } 

  # download all other files
  URLs <- as.vector(URLs)
  filenames <- as.vector(filenames)
  
  for (i in 1:length(URLs)) {
    assign(filenames[i], download.and.import.any.nhanes.file(URLs[i]))
  }

}