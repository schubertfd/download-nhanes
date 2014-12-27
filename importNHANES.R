# R function to import to NHANES and format it

get_nhanes <- function(years="2011", sections="demog", files=NULL) {
  
  # fix setInternet so files will download
  setInternet2(use=FALSE)
  
  # load file to switch year input to create URLs
  years_switch <- read.csv("inst/extdata/years.switch.csv")
  
  # load file with lists of available data sets
  nhanes_files_nosubsets <- read.csv("inst/extdata/cdc_nhanes_files2.csv")
  
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
  
  # make reference table with selected years
  selected_years <- years_switch[years_switch$years %in% years, ]
  
  # URLs for other file locations
  
  selected_files <- as.character(nhanes_files_nosubsets[(nhanes_files_nosubsets$section %in% sections) | 
                                             (nhanes_files_nosubsets$filename %in% files), 3])
  
  # Generate URLs
  URLs <- matrix(data = NA, nrow=length(selected_files), ncol=length(years))
  filenames <- matrix(data = NA, nrow=length(selected_files), ncol=length(years))
  for (i in 1:length(years)) {
    for (j in 1:length(selected_files)) {
      URLs[j, i] <- paste0("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/nhanes/", selected_years$year.str[i], "/", selected_files[j], selected_years$var_end[i])
      filenames[j, i] <- paste0("NHANES.", years[i], ".", selected_files[j], ".df")
    }
  }
  
 
# function to download NHANES files into folders by year
download.nhanes.file <- function(URL, filename, year) {

  # create file name
  xptfile <- paste0(year, "/", filename, ".xpt")
  
  # create year folder if it doesn't exist
  if(!file.exists(year)) {dir.create(year)}
  
  # download the file using the URL specified
  download.file( 
    # download the file stored in the location designated above
    URL ,
    # save the file as the file assigned above
    xptfile , 
    # download this as a binary file type
    mode = "wb"
  ) 
}

# run function for all years specified
for (i in 1:length(years)) {
  for (j in 1:nrow(URLs)) {
    download.nhanes.file(URL=URLs[j,i], filename=filenames[j, i], year=years[i])
  }
}


# function to combine files by year (merge on ID number)
# each year's merged file is saved as .rda to the working directory
combine.nhanes.files <- function(year) {

  available.files <- list.files(year)
  
  for (i in 2:length(available.files)) {
    if (!exists("merged1")) {merged1 <- read.xport(paste0(year, "/", available.files[i-1]))}
    temp <- read.xport(paste0(year, "/", available.files[i]))
    merged1 <- merge(merged1, temp, all=F)
  }

  # return merged file for specified year
  save(merged1, file=paste0("merged", year, ".rda" ))
}

# run function for all years specified
for (i in length(years)) {
  combine.nhanes.files(years[i])
}  

# TO DO:
# merge all years of data
# compute multiyear weights
# create and return survey object

  nhanes.tsl.design <- 
    svydesign(
      id = ~SDMVPSU , 
      strata = ~SDMVSTRA ,
      nest = TRUE ,
      weights = ~WTINT2YR ,
      data = NHANES.2007.demog.df
    )
  
  return(nhanes.tsl.design)

}