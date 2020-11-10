#----# INFO #----# ####
# Script: DAH R Utils
# Description: Loads R utilities for DAH channel work
# Contributors: Kyle Simpson
#----------------# ####

#----# Load Required Libraries #----# ####
if(Sys.info()['sysname'][1]=='Linux'){
  # Load library if it exists
  if(dir.exists(paste0('/ihme/homes/', Sys.info()['user'][1], '/rlibs/readstata13'))) {
    suppressMessages(require('readstata13', lib.loc=paste0('/ihme/homes/', Sys.info()['user'][1], '/rlibs')))
  } else {
    # If the /rlibs folder doesn't exist, create it; then install package
    if (!dir.exists(paste0('/ihme/homes/', Sys.info()['user'][1], '/rlibs'))) {
      dir.create(paste0('/ihme/homes/', Sys.info()['user'][1], '/rlibs'))
    }
    install.packages('readstata13', lib=paste0('/ihme/homes/', Sys.info()['user'][1], '/rlibs'))
  }
}

pacman::p_load(yaml, readxl, foreign, crayon, reticulate, stringi, plotly, haven, scales, chron, tidyverse, data.table, rjson, feather, bit64, openxlsx, RColorBrewer, gridExtra)

if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "H:/repos/fgh/", paste0("/ihme/homes/", Sys.info()['user'][1], "/repos/fgh/")))
}
#-----------------------------------# ####



#----# Data Manipulation & Calculation Functions #----# ####
## Create 'not in' statement
`%ni%` <- Negate(`%in%`)


## Collapse sum function
collapse <- function(dataset, agg_function, group_cols, calc_cols, na.rm = T) {
  #' Function to mirror Stata's collapse function
  #' @param dataset [data.frame/data.table] Dataset intended to be collapsed
  #' @param agg_function [str] The name of the aggregate function wished to be performed
  #' @param group_cols [vector] A list of the column names you wish to group by
  #' @param calc_cols [vector] A list of the column names you wish to collapse
  #' @param na.rm [logical] Optional boolean for whether you'd like to ignore NA's in your calculation. Default TRUE
  
  # Convert to data.table
  dataset <- setDT(copy(dataset))
  
  # Lapply the aggregate function over the specified columns, by any grouping
  dataset <- dataset[, lapply(.SD, get(agg_function), na.rm=na.rm), by=c(group_cols), .SDcols=c(calc_cols)]
  
  # Return the dataset
  return(dataset)
}


## Rowtotal function
rowtotal <- function(dataset, new_colname, rowtotal_cols, na.rm=T) {
  #' Function to mirror Stata's rowtotal() function
  #' @param dataset [data.frame/data.table] Dataset intended to be collapsed
  #' @param new_colname [str] The name of the new column to be calculated
  #' @param rowtotal_cols [vector] A list of the column names you wish to sum
  #' @param na.rm [logical] Optional boolean for whether you'd like to ignore NA's in the rowtotal. Default TRUE
  
  # Convert to data.table
  dataset <- setDT(copy(dataset))
  
  # rowSums the specified columns
  dataset[, eval(new_colname) := rowSums(dataset[, c(rowtotal_cols), with=F], na.rm=na.rm)]
  
  # Return dataset
  return(dataset)
  
}


## String cleaning function
string_clean <- function(dataset, col_to_clean) {
  #' Function to clean special characters, remove punctuation, and add spacing
  #' to the specified col_to_clean
  #' @param dataset [data.frame/data.table] Dataset to clean strings in
  #' @param col_to_clean [str] Name of the column containing strings to clean
  
  # Cast dataset as a data.table
  dataset <- setDT(copy(dataset))
  
  # Handle user errors
  if (col_to_clean %ni% names(dataset)) {
    cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [col_to_clean]!! ENSURE [col_to_clean] IS ONE OF THE FOLLOWING:'), paste(names(dataset), collapse=', '), '\n')
    return(dataset)
  }
  
  # Prep dataset
  new_col <- paste0('upper_', col_to_clean)
  dataset[, eval(new_col) := get(col_to_clean)]
  
  # Clean strings
  dataset[, eval(new_col) := gsub('ÿ', "Y",  get(new_col))]
  dataset[, eval(new_col) := gsub('Ÿ', "Y",  get(new_col))]
  dataset[, eval(new_col) := gsub('æ', "AE", get(new_col))]
  dataset[, eval(new_col) := gsub('Æ', "AE", get(new_col))]
  dataset[, eval(new_col) := gsub('œ', "OE", get(new_col))]
  dataset[, eval(new_col) := gsub('Œ', "OE", get(new_col))]
  dataset[, eval(new_col) := gsub('ç', "C",  get(new_col))]
  dataset[, eval(new_col) := gsub('Ç', "C",  get(new_col))]
  dataset[, eval(new_col) := gsub('ñ', "N",  get(new_col))]
  dataset[, eval(new_col) := gsub('Ñ', "N",  get(new_col))]
  dataset[, eval(new_col) := gsub('ß', "SS", get(new_col))]
  
  dataset[, eval(new_col) := gsub(':', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('&', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('%', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('#', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('@', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('\\$', " ", get(new_col))]
  dataset[, eval(new_col) := gsub(',', " ", get(new_col))]
  dataset[, eval(new_col) := gsub(';', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('.', " ", get(new_col), fixed = T)]
  dataset[, eval(new_col) := gsub('-', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('‐', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('(', " ", get(new_col), fixed = T)]
  dataset[, eval(new_col) := gsub(')', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('\"'," ", get(new_col))]
  dataset[, eval(new_col) := gsub("'", " ", get(new_col))]
  dataset[, eval(new_col) := gsub("’", " ", get(new_col))]
  dataset[, eval(new_col) := gsub("`", " ", get(new_col))]
  dataset[, eval(new_col) := gsub('«', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('»', " ", get(new_col))]
  dataset[, eval(new_col) := gsub('·', " ", get(new_col))]
  
  # Replace A's
  letters <- c("á", "Á", "à", "À", "ã", "Ã", "â", "Â", "å", "Å", "ä", "Ä")
  for (l in 1:length(letters)) {
    letter <- letters[l]
    dataset[, eval(new_col) := gsub(letter, "A", get(new_col))]
  }
  
  # Replace E's
  letters <- c("é", "É", "ê", "Ê", "è", "È", "ë", "Ë")
  for (l in 1:length(letters)) {
    letter <- letters[l]
    dataset[, eval(new_col) := gsub(letter, "E", get(new_col))]
  }
  
  # Replace I's
  letters <- c("í", "Í", "ì", "Ì", "î", "Î", "ï", "Ï")
  for (l in 1:length(letters)) {
    letter <- letters[l]
    dataset[, eval(new_col) := gsub(letter, "I", get(new_col))]
  }
  
  # Replace O's
  letters <- c("ó", "Ó", "ò", "Ò", "õ", "Õ", "ô", "Ô", "ø", "Ø", "ö", "Ö")
  for (l in 1:length(letters)) {
    letter <- letters[l]
    dataset[, eval(new_col) := gsub(letter, "O", get(new_col))]
  }
  
  # Replace U's
  letters <- c("ú", "Ú", "ù", "Ù", "û", "Û", "ü", "Ü")
  for (l in 1:length(letters)) {
    letter <- letters[l]
    dataset[, eval(new_col) := gsub(letter, "U", get(new_col))]
  }
  
  dataset[, eval(new_col) := gsub("\\\xa0", 'A', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xa1", ' ', get(new_col))] # ¡ (inverted exclamation mark)
  dataset[, eval(new_col) := gsub("\\\xa2", ' ', get(new_col))] # ¢
  dataset[, eval(new_col) := gsub("\\\xa3", ' ', get(new_col))] # euro symbol - not in stata
  dataset[, eval(new_col) := gsub("\\\xa4", '',  get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xa6", ' ', get(new_col))] # "..." or other symbol - not in stata
  dataset[, eval(new_col) := gsub("\\\xa7", 'SS',get(new_col))] #unclear if this should be a C; special character not in original code
  dataset[, eval(new_col) := gsub("\\\xa8", ' ', get(new_col))] # " 
  dataset[, eval(new_col) := gsub("\\\xaa", ' ', get(new_col))] # "superscript a" - not in stata
  dataset[, eval(new_col) := gsub("\\\xab", ' ', get(new_col))] # "<<" 
  dataset[, eval(new_col) := gsub("\\\xac", '',  get(new_col))] # ¬
  dataset[, eval(new_col) := gsub("\\\xad", ' ', get(new_col))] # "-"
  dataset[, eval(new_col) := gsub("\\\xae", ' ', get(new_col))] # "small R in circle" -not in stata
  dataset[, eval(new_col) := gsub("\\\xaf", ' ', get(new_col))] # "ssuperscript dash" -not in stata
  
  dataset[, eval(new_col) := gsub("\\\xb0", ' ', get(new_col))] # degree symbol - not in stata
  dataset[, eval(new_col) := gsub("\\\xb1", ' ', get(new_col))] # plus/minus symbol - not in stata
  dataset[, eval(new_col) := gsub("\\\xb2", ' ', get(new_col))] # ² (superscript 2)
  dataset[, eval(new_col) := gsub("\\\xb3", '',  get(new_col))] # ³ (seems to only be placed after special character As so replaced with no space)
  dataset[, eval(new_col) := gsub("\\\xb5", ' ', get(new_col))] # mu - not in stata
  dataset[, eval(new_col) := gsub("\\\xb6", ' ', get(new_col))] # paragraph symbol (pilcrow) - not in stata
  dataset[, eval(new_col) := gsub("\\\xb7", ' ', get(new_col))] # floating decimal
  dataset[, eval(new_col) := gsub("\\\xba", ' ', get(new_col))] # superscript o with underline - not in stata
  dataset[, eval(new_col) := gsub("\\\xbb", ' ', get(new_col))] # ">>"
  dataset[, eval(new_col) := gsub("\\\xbc", '',  get(new_col))] # ¼ (seems to only be placed after special character As so replaced with no space)
  dataset[, eval(new_col) := gsub("\\\xbd", ' ', get(new_col))] # ½
  dataset[, eval(new_col) := gsub("\\\xbe", ' ', get(new_col))] # ¾
  dataset[, eval(new_col) := gsub("\\\xbf", 'A', get(new_col))] # "Â"
  
  dataset[, eval(new_col) := gsub("\\\xc0", 'A', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xc1", 'A', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xc2", 'A', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xc3", 'A', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xc4", 'A', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xc5", 'A', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xc6", 'AE',get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xc7", 'C', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xc8", 'E', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xc9", 'E', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xca", 'E', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xcb", 'E', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xcc", 'I', get(new_col))] # Ì
  dataset[, eval(new_col) := gsub("\\\xcd", 'I', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xce", 'I', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xcf", 'I', get(new_col))]
  
  dataset[, eval(new_col) := gsub("\\\xd1", 'N', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xd2", 'O', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xd3", 'O', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xd4", 'O', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xd5", 'O', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xd6", 'O', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xd8", 'O', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xd9", 'U', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xda", 'U', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\xdb", 'U', get(new_col))] # Û
  dataset[, eval(new_col) := gsub("\\\xdc", 'U', get(new_col))] 
  dataset[, eval(new_col) := gsub("\\\xdf", 'SS',get(new_col))] # ß
  
  dataset[, eval(new_col) := gsub("\\\xe0", 'A', get(new_col))] # à
  dataset[, eval(new_col) := gsub("\\\xe1", 'A', get(new_col))] # Á
  dataset[, eval(new_col) := gsub("\\\xe2", 'A', get(new_col))] # Â
  dataset[, eval(new_col) := gsub("\\\xe3", 'A', get(new_col))] # Ã
  dataset[, eval(new_col) := gsub("\\\xe4", 'A', get(new_col))] # Ä
  dataset[, eval(new_col) := gsub("\\\xe5", 'A', get(new_col))] # Å
  dataset[, eval(new_col) := gsub("\\\xe6", 'AE',get(new_col))] # Æ
  dataset[, eval(new_col) := gsub("\\\xe7", 'C', get(new_col))] # Ç
  dataset[, eval(new_col) := gsub("\\\xe8", 'E', get(new_col))] # È
  dataset[, eval(new_col) := gsub("\\\xe9", 'E', get(new_col))] # É é
  dataset[, eval(new_col) := gsub("\\\xea", 'E', get(new_col))] # Ê
  dataset[, eval(new_col) := gsub("\\\xeb", 'E', get(new_col))] # ë
  dataset[, eval(new_col) := gsub("\\\xec", 'I', get(new_col))] # í
  dataset[, eval(new_col) := gsub("\\\xed", 'I', get(new_col))] # í
  dataset[, eval(new_col) := gsub("\\\xee", 'I', get(new_col))] # Î
  dataset[, eval(new_col) := gsub("\\\xef", 'I', get(new_col))] # Ï
  
  dataset[, eval(new_col) := gsub("\\\xf1", 'N', get(new_col))] # Ñ
  dataset[, eval(new_col) := gsub("\\\xf2", 'O', get(new_col))] # Ò
  dataset[, eval(new_col) := gsub("\\\xf3", 'O', get(new_col))] # Ó
  dataset[, eval(new_col) := gsub("\\\xf4", 'O', get(new_col))] # Ô
  dataset[, eval(new_col) := gsub("\\\xf5", 'O', get(new_col))] # Õ
  dataset[, eval(new_col) := gsub("\\\xf6", 'O', get(new_col))] # Ö
  dataset[, eval(new_col) := gsub("\\\xf8", 'O', get(new_col))] # Ø
  dataset[, eval(new_col) := gsub("\\\xf9", 'U', get(new_col))] # Ù 
  dataset[, eval(new_col) := gsub("\\\xfa", 'U', get(new_col))] # Ú
  dataset[, eval(new_col) := gsub("\\\xfb", 'U', get(new_col))] # Û
  dataset[, eval(new_col) := gsub("\\\xfc", 'U', get(new_col))] # Ü
  
  # inconsistent encodings - left as blank but read in by stata
  dataset[, eval(new_col) := gsub("\\\x80", '', get(new_col))] # €
  dataset[, eval(new_col) := gsub("\\\x81", '', get(new_col))] # A or U depending on spanish or german
  dataset[, eval(new_col) := gsub("\\\x82", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x83", '', get(new_col))] # ƒ
  dataset[, eval(new_col) := gsub("\\\x85", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x86", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x87", '', get(new_col))] # ‡
  dataset[, eval(new_col) := gsub("\\\x88", '', get(new_col))] # blank space
  dataset[, eval(new_col) := gsub("\\\x89", '', get(new_col))] # %
  dataset[, eval(new_col) := gsub("\\\x8a", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x8c", '', get(new_col))] # Œ
  dataset[, eval(new_col) := gsub("\\\x8d", '', get(new_col))] # À -
  dataset[, eval(new_col) := gsub("\\\x8e", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x8f", '', get(new_col))] # - 
  
  dataset[, eval(new_col) := gsub("\\\x90", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x93", '', get(new_col))] # <93>
  dataset[, eval(new_col) := gsub("\\\x94", '', get(new_col))] # <94>
  dataset[, eval(new_col) := gsub("\\\x95", ' ',get(new_col))] # •
  dataset[, eval(new_col) := gsub("\\\x96", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x97", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x98", '', get(new_col))] # ˜
  dataset[, eval(new_col) := gsub("\\\x99", '', get(new_col))] # ™
  dataset[, eval(new_col) := gsub("\\\x9a", '', get(new_col))] # Š
  dataset[, eval(new_col) := gsub("\\\x9b", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x9c", '', get(new_col))]
  dataset[, eval(new_col) := gsub("\\\x9f", 'Y',get(new_col))] # Ÿ
  
  # Fix other symbols
  dataset[, eval(new_col) := gsub("\\?", ' ', get(new_col))]
  dataset[, eval(new_col) := gsub("'", ' ',   get(new_col))]
  dataset[, eval(new_col) := gsub(":", ' ',   get(new_col))]
  dataset[, eval(new_col) := gsub(";", ' ',   get(new_col))]
  dataset[, eval(new_col) := gsub("\\.", ' ', get(new_col))]
  dataset[, eval(new_col) := gsub(",", ' ',   get(new_col))]
  dataset[, eval(new_col) := gsub("\\(", ' ', get(new_col))]
  dataset[, eval(new_col) := gsub("\\)", ' ', get(new_col))]
  dataset[, eval(new_col) := gsub(",", ' ',   get(new_col))]
  dataset[, eval(new_col) := gsub("/", ' ',   get(new_col))]
  dataset[, eval(new_col) := gsub("\\\\", ' ',get(new_col))] # single backslash
  dataset[, eval(new_col) := gsub("\\[", ' ', get(new_col))]
  dataset[, eval(new_col) := gsub("\\]", ' ', get(new_col))]
  dataset[, eval(new_col) := gsub("-", ' ',   get(new_col))]
  dataset[, eval(new_col) := gsub("_", ' ',   get(new_col))]
  
  # Add trailing spaces and cast uppercase
  dataset[, eval(new_col) := paste0(' ', str_squish(get(new_col)), ' ')]
  dataset[, eval(new_col) := toupper(get(new_col))]
  
  # Return dataset
  return(dataset)
}
#-----------------------------------------------------# ####

#----# Root, Path, and Saving Helpers #----# ####
## Clean filepath extracted from YAML files
clean_filepath <- function(filepath) {
  #' Function to remove any variables written into filepaths
  #' @param filepath [str] String to have items removed from
  
  # Clean file paths
  filepath <- str_replace(filepath, "\\[j\\]", chr(get_root("j")))
  filepath <- str_replace(filepath, "\\[share\\]", chr(get_root("share")))
  
  # Clean DAH Parameters
  filepath <- str_replace(filepath, "\\[report_year\\]", chr(get_root("report_year")))
  filepath <- str_replace(filepath, "\\[prev_report_year\\]", chr(get_root("prev_report_year")))
  filepath <- str_replace(filepath, "\\[abrv_year\\]", chr(get_root("abrv_year")))
  filepath <- str_replace(filepath, "\\[prev_abrv_year\\]", chr(get_root("prev_abrv_year")))
  filepath <- str_replace(filepath, "\\[CRS_update_MMYY\\]", chr(get_dah_param('CRS', 'update_MMYY')))
  
  # Return
  return(filepath)
}


## Return the requested DAH parameter from YAML file
get_dah_param <- function(param_name, sub_key="") {
  # Read dah_parameters.yml
  if(sub_key == "") {
    param_dict <- yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_parameters.yml"))[[param_name]]
  } else {
    param_dict <- yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_parameters.yml"))[[param_name]][[sub_key]]
  }
  
  # Return
  return(param_dict)
}


## Create dah.roots variable
set_roots <- function() {
  # Get Sys info
  sysinf <- Sys.info()
  os.name <- sysinf['sysname']
  # Set J, H, K
  j <- ifelse(os.name == "Windows", "J:/", "/home/j/")
  h <- ifelse(os.name == "Windows", "H:/", 
              ifelse(os.name == "Darwin", paste0("/Volumes/", Sys.info()['user'][1], "/"), 
                     paste0("/ihme/homes/", Sys.info()['user'][1], "/")))
  k <- ifelse(os.name == "Windows", "K:/", "/ihme/cc_resources/")
  share <- file.path("/share/resource_tracking/DAH/")
  # Create list
  roots = list(
    "j"=unname(j),
    "h"=unname(h),
    "k"=unname(k),
    "share"=unname(share),
    "report_year"=get_dah_param('report_year'),
    "abrv_year"=get_dah_param('abrv_year'),
    "prev_report_year"=get_dah_param('prev_report_year'),
    "prev_abrv_year"=get_dah_param('prev_abrv_year'),
    "defl_MMYY"=get_dah_param('defl_MMYY'),
    "prev_defl_MMYY"=get_dah_param('prev_defl_MMYY'),
    "dah_year"=paste0('DAH_', get_dah_param('abrv_year'))
  )
  dah.roots <<- roots # keep double arrow
  # Return
  return(dah.roots)
}


## Helper function to pull file roots / shared references
get_root <- function(root_name) {
  # Pull roots
  roots = set_roots()
  
  if (root_name %in% names(roots)) { # If the name exists
    return(roots[[root_name]])
  } else if (root_name == "list roots") { # If requesting root names
    return(names(roots))
  } else { # Throw error
    return("incorrect root name requested")
  }
}


## Return filepath to specified location
get_path <- function(channel, stage, is_backup=F) {
  #' Function to return the filepath for a specified channel & stage
  #' @param channel [str] Primary string key used to filter dah_paths
  #' @param stage [str] Secondary string key used to filter specific filepath
  #' @param is_backup [bool] Typically for internal use only in conjunction with `save_dataset``, specifies primary or backup file location
  
  # Collect all dah_paths
  path_dict <- yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_paths.yml"))
  
  # Locate if primary or backup desired
  if (is_backup) {
    path_dict <- path_dict[['backup_location']]
  } else {
    path_dict <- path_dict[['primary_location']]
  }
  
  # Subset to paths that are related to the given process
  path_dict <- path_dict[[channel]][[stage]]
  # Character cleaning 
  path_dict <- clean_filepath(path_dict)
  
  # Return
  return(path_dict)
}


## Recursively ensure that `filepath` exists
ensure_dir <- function(filepath) {
  # If filepath doesn't exist, create it (recursively)
  if (!dir.exists(filepath)) {
    dir.create(filepath, recursive = T)
    cat(paste0('  Directory created at: ', filepath, '\n'))
  } 
  
  # If directory has some strange read/write permissions, update it
  tryCatch({
    # Try writing a file
    test <- data.table('test' = 1)
    fwrite(test, paste0(filepath, 'test.csv'))
  }, warning = function(warning_condition) {
    cat(warning_condition)
  }, error = function() {
    # If unable to write the file, change the directory permissions
    system(paste0("chmod -R 0777 ", filepath))
  }, finally = {
    # Remove the test write file if it was created
    unlink(paste0(filepath, 'test.csv'))
  })
}


## Save dataset to appropriate location
save_dataset <- function(dataset, filename, channel, stage, write_dta = F) {
  #' Function to save specified dataset - Calls upon get_path, ensure_dir, anc check_col_reqs.
  #' @param dataset [data.frame/data.table] Dataset to be saved.
  #' @param filename [str] Name of file desired to be written. No need to specify `.csv` suffix
  #' @param channel [str] Used in call to get_path, see get_path documentation for more details
  #' @param stage [str] Used in call to get_path, see get_path documentation for more details
  #' @param write_dta [logical] Default false, specify true if output data should be in .dta format.
  
  # Ensure input dataset is a data.table
  dataset <- setDT(copy(dataset))
  date <- format(Sys.Date(), "%b_%d_%Y")
  
  # Ensure all required columns are present
  # check_col_reqs(dataset = dataset, stage = stage, create_missing = F)
  
  # Check that columns contain only valid observations if saving a final dataset
  # if (stage == 'fin') {
  #   for (col in names(dataset)) {
  #     if (toupper(col) %in% names(yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_parameters.yml"))$valid_observations)) {
  #       check_col_obs(dataset, toupper(col), col)
  #     }
  #   }
  # }
  
  
  # Get appropriate location using get_path
  primary_location <- get_path(channel, stage)
  backup_location <- get_path(channel, stage, is_backup=T)
  
  # Ensure valid output locations
  ensure_dir(filepath = primary_location)
  ensure_dir(filepath = backup_location)
  
  if (write_dta) {
    # Save to primary location
    options(warn=-1) # Remove warnings for mismatching length of variable labels
    save.dta13(dataset, file=paste0(primary_location, filename, '.dta'))
    cat(green(paste0('  ', filename, '.dta saved to: '))); cat(paste0(primary_location, '\n'))
    # Save to backup location
    save.dta13(dataset, file=paste0(backup_location, filename, '_', date, '.dta'))
    cat(blue(paste0('  Backup saved to: '))); cat(paste0(backup_location, '\n'))
    options(warn=0)
  } else {
    # Save to primary location
    fwrite(dataset, file=paste0(primary_location, filename, '.csv'))
    cat(green(paste0('  ', filename, '.csv saved to: '))); cat(paste0(primary_location, '\n'))
    # Save to backup location
    fwrite(dataset, file=paste0(backup_location, filename, '_', date, '.csv'))
    cat(blue(paste0('  Backup saved to: '))); cat(paste0(backup_location, '\n'))
  }
}
#------------------------------------------# ####

#----# Health_ADO and TT_smooth Launching Functions #----# ####
## Create TT_smooth config file
create_TT_config <- function(data_path, channel_name, total_colname, year_colname, num_yrs_forecast, is_test, hfa_list) {
  #' Function to create the config file needed for TT_smooth
  #' @param data_path [str] Path to pre-smoothed data, and where post-smoothed data will be output to
  #' @param channel_name [str] Lowercase name of the channel to smooth
  #' @param total_colname [str] Name of the column containing the HFA sums
  #' @param year_colname [str] Name of the column containing the year designataions
  #' @param num_yrs_forecast [str] Number of years to forecast data (1 or 2)
  #' @param is_test [int] Numeric boolean for whether this is a test (default should be 0)
  #' @param hfa_list [vector] Vector of column names observed in the pre-smoothed dataset which should be smoothed
  
  config <- data.table(data_path = data_path, 
                       totvar = total_colname, 
                       timevar = year_colname, 
                       frcstvar = num_yrs_forecast, 
                       tstvar = is_test,
                       code_repo = code_repo
  )
  
  # Add a column for each HFA/country and give it a random value
  for (col in hfa_list) {
    config[, eval(col) := 1]
  }
  
  # Check to make sure the file doesn't already exist
  if (exists(paste0(get_path('tt_smooth_configs', 'all'), tolower(channel_name), '_config.dta'))) {
    unlink(paste0(get_path('tt_smooth_configs', 'all'), tolower(channel_name), '_config.dta'))
  }
  
  # Save the config
  save_dataset(config, paste0(tolower(channel_name), '_config'), 'tt_smooth_configs', 'all', write_dta = T)
}


## Launch TT_smooth job
launch_TT_smooth <- function(channel_name, job_mem=5, job_threads=1, runtime="00:15:00", queue='all.q', errors_path=dah.roots$h, output_path=dah.roots$h) {
  #' Function to launch TT_smooth
  #' @param channel_name [str] Lowercase name of the channel to run
  #' @param job_mem [int] Optional number of gigabytes needed for the job
  #' @param job_threads [int] Optional number of threads needed for the job
  #' @param runtime [str] Optional time in HH:MM:SS needed for the job
  #' @param queue [str] Optional queue to run job in
  #' @param errors_path [str] Optional filepath to directory for errors file
  #' @param output_path [str] Optional filepath to directory for output file
  
  system(paste0("qsub -e ", errors_path, "errors.txt -o ", output_path, "output.txt -N tt_sm_", tolower(channel_name), " -l archive=TRUE -q ", queue, " -P proj_fgh -l fthread=", 
                job_threads, " -l m_mem_free=", job_mem, "G -l h_rt=", runtime, " ", code_repo, "shellstata15.sh ", code_repo, "FUNCTIONS/launch_TT_smooth.ado ", tolower(channel_name)))
}


## Create Health_ADO config file
create_Health_config <- function(data_path, channel_name, varlist, language, function_to_run) {
  #' Function to create the config needed for Health_ADO
  #' @param data_path [str] Path to pre-smoothed data, and where post-smoothed data will be output to
  #' @param channel_name [str] Lowercase name of the channel to process
  #' @param varlist [vector] List of column names to search over
  #' @param language [str] Name of the language you'd like to search in
  #' @param function_to_run [int] Numeric representation of the Health_ADO_master function you'd like to run. 
  #'                              1=HFA_ado_master, 2=create_upper_vars, 3=run_keyword_search, 4=post_kws_fixes, 5=create_keyword_props
  
  config <- data.table(data_path = data_path,
                       varlist = paste(varlist, collapse=" "),
                       language = language,
                       function_to_run = function_to_run,
                       code_repo = code_repo
  )
  
  # Check to make sure the file doesn't already exist
  if (exists(paste0(get_path('health_ado_configs', 'all'), tolower(channel_name), '_config.dta'))) {
    unlink(paste0(get_path('health_ado_configs', 'all'), tolower(channel_name), '_config.dta'))
  }
  
  # Save the config
  save_dataset(config, paste0(tolower(channel_name), '_config'), 'health_ado_configs', 'all', write_dta = T)
}


## Launch Health_ADO job
launch_Health_ADO <- function(channel_name, job_mem=5, job_threads=1, runtime="00:15:00", queue='all.q', errors_path=dah.roots$h, output_path=dah.roots$h) {
  #' Function to launch Health_ADO_master
  #' @param channel_name [str] Lowercase name of the channel to run
  #' @param job_mem [int] Optional number of gigabytes needed for the job
  #' @param job_threads [int] Optional number of threads needed for the job
  #' @param runtime [str] Optional time in HH:MM:SS needed for the job
  #' @param queue [str] Optional queue to run job in
  #' @param errors_path [str] Optional filepath to directory for errors file
  #' @param output_path [str] Optional filepath to directory for output file
  
  system(paste0("qsub -e ", errors_path, "errors.txt -o ", output_path, "output.txt -N hlth_ado_", tolower(channel_name), " -l archive=TRUE -q ", queue, " -P proj_fgh -l fthread=", 
                job_threads, " -l m_mem_free=", job_mem, "G -l h_rt=", runtime, " ", code_repo, "shellstata15.sh ", code_repo, "FUNCTIONS/launch_Health_ADO.ado ", tolower(channel_name)))
}
#--------------------------------------------------------# ####

#----# Data Validation Functions #----# ####
## Ensure health focus area and program area hierarchy is valid
check_hfa_sum <- function(dataset, health_focus_area, program_areas, return_violations = F) {
  #' Function to ensure that specified program areas sum to the specified health focus area.
  #' @param dataset [data.frame/data.table] Dataset with columns for PAs/HFAs to be checked.
  #' @param health_focus_area [str] Parent health focus area to check against.
  #' @param program_areas [list] Children program areas to sum & check against parent.
  #' @param return_violations [bool] Specify this as True if you want to return any rows where the PA sum != HFA total
  
  # Ensure input dataset is a data.table
  dataset <- setDT(copy(dataset))
  
  # Print warning if children don't have save prefix as parent
  for (program_area in program_areas) {
    if (chr(str_split(program_area, "_")[[1]][1]) != chr(str_split(health_focus_area, "_")[[1]][1])) {
      cat("    WARNING: `program_areas` prefix doesn't match `health_focus_area` prefix.  You may have mistyped a program area.\n")
    }
  }
  
  # Calculate row-specific sums
  dataset[, ch_rsum := rowSums(dataset[, program_areas, with = F], na.rm=T)]
  # Flag if sum of PAs != HFA total 
  dataset[ch_rsum != get(health_focus_area), sum_check := 1]
  
  # If there are any rows where PA sum != HFA total, print error (and return violations if specified)
  # Else print success (and return nothing if specified)
  if (1 %in% unique(dataset$sum_check)) {
    cat(red(paste0('    ERROR: PAs do not sum to HFA in ', nrow(dataset[dataset$sum_check == 1,]), ' rows!!\n')))
    if (return_violations) {
      dataset <- dataset[dataset$sum_check == 1,]
      dataset[, program_area_sum := ch_rsum]
      dataset[, `:=`(ch_rsum = NULL, sum_check = NULL)]
      return(dataset)
    }
  } else {
    cat(green(paste0('    SUCCESS: PAs sum to HFA!!\n')))
    if (return_violations) {
      return(data.frame())
    }
  }
  
}


## Check for negative disbursements
check_neg_disb <- function(dataset, col_names, return_violations = F) {
  #' Function to check over specified columns and check for negative numbers
  #' @param dataset [data.frame/data.table] Dataset with columns in `col_names` to be checked.
  #' @param col_names [list] A list of column names to be checked for negative numbers
  
  # Ensure input dataset is a data.table
  dataset <- setDT(copy(dataset))
  
  # Check to make sure col_names are valid columns in dataset
  if (any(col_names %ni% names(dataset))) {
    cat(red(paste0('    ERROR: Invalid col_names specified.\n')))
    return(dataset)
  }
  
  # Foreach column in col_names, generate flag if negative number detected
  violations <- data.frame()
  for (col in col_names) {
    dataset[get(col) < 0, eval(paste0(col, "_check")) := 1]
    if (1 %in% unique(dataset[, get(paste0(col, "_check"))])) {
      violations <- rbind(violations, dataset[get(paste0(col, "_check")) == 1,], fill = T)
    }
  }
  
  # If we found negative numbers, print error (and return violations if specified)
  # Else print success (and return empty if specified)
  if (nrow(violations) != 0) {
    cat(red(paste0('    ERROR: ', nrow(violations), ' Negative vaules encountered!!\n')))
    if (return_violations) {
      violations[, eval(paste0(col_names, "_check")) := NULL]
      return(violations)
    }
  } else {
    cat(green(paste0('    SUCCESS: No negative values encountered!!\n')))
    if (return_violations) {
      return(violations)
    }
  }
}


## Ensure all required columns are present
check_col_reqs <- function(dataset, stage, create_missing = F) {
  #' Function to ensure that all required columns are present in the data.
  #' @param dataset [data.frame/data.table] Dataset to be checked for valid columns.
  #' @param create_missing [bool] Specify this as true if you expect your data to be missing columns and you would like them created & filled with NA
  
  # Ensure input dataset is a data.table
  dataset <- setDT(copy(dataset))
  
  # Pull required columns
  req_cols <- unname(unlist(yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_parameters.yml"))$required_cols[stage]))
  # Clean filepaths from any required columns
  req_cols <- unlist(lapply(req_cols, clean_filepath))
  
  # Check for required columns
  present_cols <- names(dataset)[names(dataset) %in% req_cols]
  missing_cols <- req_cols[req_cols %ni% present_cols]
  
  # If any required columns are missing
  if (length(missing_cols) >= 1) {
    # If requested to fill missing, create& fill
    # Else print a warning & break
    if (create_missing) {
      cat(yellow(paste0('    WARNING: The following required columns are being created & filled with NA values: '))); cat(paste(missing_cols, collapse=", ")); cat('\n')
      dataset[, eval(missing_cols) := NA]
      return(dataset)
    } else {
      cat(red('    ERROR: Missing some required columns!! Ensure your data contains the following columns: ')); cat(yellow(paste(missing_cols, collapse=", "))); cat("\n")
      return(dataset)
    }
  }
}


## Ensure valid observations 
check_col_obs <- function(dataset, std_colname, dataset_colname) {
  #' Function to check that only valid observations of a given column are present
  #' @param dataset [data.frame/data.table] Dataset to be checked for valid observations
  #' @std_colname [str] The standard name of the column to be checked. E.X. ISO_CODE, INCOME_SECTOR, RECIPIENT_AGENCY_SECTOR, etc..
  #' @dataset_colname [str] The name of the column to be checked observed in [@dataset].
  
  # Ensure provided dataset_colname is observed in provided dataset
  if (dataset_colname %ni% names(dataset)) {
    cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [dataset_colname]!! ENSURE [dataset_colname] IS ONE OF THE FOLLOWING:'), paste(names(dataset), collapse=', '), '\n')
    return(dataset)
  }
  
  # Ensure provided std_colname is valid
  yaml <- yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_parameters.yml"))$valid_observations
  if (toupper(std_colname) %ni% names(yaml)) {
    cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [std_colname]!! ENSURE [std_colname] IS ONE OF THE FOLLOWING:'), paste(names(yaml), collapse=', '), '\n')
    return(dataset)
  }
  
  # Collect valid observations & ensure input dataset is a data.table
  valid_obs <- unname(unlist(yaml[toupper(std_colname)]))
  dataset <- setDT(copy(dataset))
  rm(yaml)
  
  # Clean if std_colname == 'YEAR'
  if (toupper(std_colname) == 'YEAR') {
    valid_obs <- c(1990:dah.roots$report_year)
  }
  
  # Check for invalid observations of dataset_colname in dataset
  if (any(unique(dataset[, get(dataset_colname)]) %ni% valid_obs)) {
    violations <- unique(dataset[, get(dataset_colname)]) %ni% valid_obs
    cat(paste0(red('    ERROR: INVALID OBSERVATIONS OF ['), white(dataset_colname), red(']!! PLEASE RECODE THESE OBSERVATIONS ['), yellow(violations), red('] AS ONE OF THE FOLLOWING: ['), green(valid_obs), red(']\n')))
    return(dataset)
  } else {
    cat(green(paste0('    SUCCESS: ALL OBSERVATIONS OF [', dataset_colname, '] ARE VALID!!')))
  }
}
#-------------------------------------# ####

#----# COVID Functions #----# ####
## Remove 2020 COVID data function
covid_intercept <- function(dataset, year_colname, keyword_search_colnames) {
  #' Function to intercept 2020 COVID project-level data from channels which report report-year data
  #' @param dataset [data.frame/data.table] Dataset intended to be corrected
  #' @param year_colname [str] The name of the column containing year data
  #' @param keyword_search_colnames [vector] A list of column names to check for COVID keywords (These must be cleaned of special characters and uppercase)
  
  # Convert to data.table
  dataset <- setDT(copy(dataset))
  
  # Handle user errors
  if (year_colname %ni% names(dataset)) {
    cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [year_colname]!! ENSURE [year_colname] IS ONE OF THE FOLLOWING:'), paste(names(dataset), collapse=', '), '\n')
    return(dataset)
  }
  if (any(keyword_search_colnames %ni% names(dataset))) {
    cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [keyword_search_colnames]!! ENSURE [keyword_search_colnames] INCLUDE ONE OF THE FOLLOWING:'), paste(names(dataset), collapse=', '), '\n')
    return(dataset)
  }
  for (col in keyword_search_colnames) {
    dataset[, upper := toupper(get(col))]
    dataset[upper != get(col), check := 1]
    if (1 %in% unique(dataset$check)) {
      dataset[, `:=`(upper = NULL, check = NULL)]
      cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [keyword_search_colnames]!! ENSURE [keyword_search_colnames] CONTAIN CLEANED STRINGS!!\n'))
      return(dataset)
    }
    dataset[, `:=`(upper = NULL, check = NULL)]
  }
  
  # Load COVID keywords
  keywords <- yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_parameters.yml"))$covid_intercept_keywords
  
  # Search the keyword_search_columns for COVID keywords
  for (col in keyword_search_colnames) {
    dataset[, eval(paste0(col, '_srch')) := 0]
    for (keyword in keywords) {
      # If the year equals 2020, tag keywords
      dataset[get(year_colname) == 2020, 
              eval(paste0(col, '_srch')) := get(paste0(col, '_srch')) + 
                str_detect(string = get(col), pattern = keyword)]
    }
  }
  
  # Drop projects where a COVID keyword was tagged - not going to do in the function, leaving this up to each channel
  # for (col in keyword_search_colnames) {
  #   dataset <- dataset[get(paste0(col, '_srch')) == 0, ]
  #   dataset[, eval(paste0(col, '_srch')) := NULL]
  # }
  
  # Return the subset data
  return(dataset)
}


# COVID-specific keyword search function
covid_kws <- function(dataset, keyword_search_colnames, keep_clean = T, keep_counts = T,
                      languages = c('english', 'spanish', 'portuguese', 'french', 'italian', 
                                    'dutch', 'german', 'norwegian', 'swedish')) {
  #' Function to run COVID-specific keyword search
  #' @param dataset [data.frame/data.table] The dataset to tag COVID keywords in
  #' @param keyword_search_colnames [vector] A list of column names to search for COVID keywords in
  #' @param keep_clean [bool] True/False parameter to keep (True) or drop (False) the cleaned strings
  #' @param keep_counts [bool] True/False parameter to keep (True) or drop (False) the raw keyword counts
  #' @param languages [vector] A list of the languages present in the dataset you're keyword searching
  
  # Cast dataset at data.table
  dataset <- setDT(copy(dataset))
  
  # Load COVID keywords
  keywords <- yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_parameters.yml"))$covid_keywords
  
  # Handle user errors
  if (any(keyword_search_colnames %ni% names(dataset))) {
    cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [keyword_search_colnames]!! ENSURE [keyword_search_colnames] INCLUDE ONE OF THE FOLLOWING:'), paste(names(dataset), collapse=', '), '\n')
    return(dataset)
  }
  if (length(languages) != 9) {
    if (any(tolower(languages) %ni% names(keywords))) {
      cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [languages]!! ENSURE [languages] INCLUDES AT LEAST ONE OF THE FOLLOWING:'), paste(names(keywords), collapse=', '), '\n')
      return(dataset)
    }
  }
  
  cat('  Cleaning special characters from keyword_search_colnames\n')
  # Clean special characters from kws_colnames
  for (col in keyword_search_colnames) {
    dataset <- string_clean(dataset, col)
  }
  
  # Generate empty columns for keywords to tag
  dataset[, eval(names(keywords[[1]])) := 0]
  dataset[, other := 0] # Adding "other" PA to capture anything untagged
  dataset[, eval(paste0(names(keywords[[1]]), '_prop')) := 0.0]
  dataset[, other_prop := 0] # Adding "other" PA to capture anything untagged
  
  cat('  Performing keyword search\n')
  # Search the keyword_search_colnames for COVID keywords
  for (col in keyword_search_colnames) {
    # Foreach language
    for (lang in tolower(languages)) {
      # Pull keywords by program area for this language
      lst <- keywords[[lang]]
      # Foreach program area
      for (pa in names(lst)) {
        words <- lst[[pa]]
        # Foreach keyword in this program area
        for (keyword in words) {
          # Count the number of keyword hits + add to running number
          dataset[, eval(pa) := get(pa) + str_count(string = get(paste0('upper_', col)), pattern = keyword)]
          rm(keyword)
        }
        rm(pa, words)
      }
      rm(lst, lang)
    }
    rm(col)
  }
  
  cat('  Creating keyword proportions\n')
  # Rowtotal PAs to generate HFA
  dataset <- rowtotal(dataset, 'COVID_total', c(names(keywords[[1]]), 'other'))
  dataset[COVID_total == 0, other := 1] # Add any untagged projects to "other"
  dataset <- rowtotal(dataset, 'COVID_total', c(names(keywords[[1]]), 'other'))
  
  # Create HFA-PA proportions based on # of keywords tagged
  for (pa in c(names(keywords[[1]]), 'other')) {
    dataset[COVID_total != 0, eval(paste0(pa, '_prop')) := get(pa) / COVID_total]
    rm(pa)
  }
  
  # Rowtotal PA proportions to generate HFA total proportion
  dataset <- rowtotal(dataset, 'COVID_total_prop', paste0(c(names(keywords[[1]]), 'other'), '_prop'))
  
  # Drop cleaned strings if desired
  if (!keep_clean) {
    cat('  Dropping cleaned strings\n')
    for (col in keyword_search_colnames) {
      dataset[, eval(paste0('upper_', col)) := NULL]
    }
  }
  
  # Drop keyword counts if desired
  if (!keep_counts) {
    cat('  Dropping program area counts\n')
    dataset[, eval(c(names(keywords[[1]]), 'other')) := NULL]
  }
  
  # Return dataset
  return(dataset)
}


## COVID function to report summary stats and produce plots
covid_stats_report <- function(dataset, amount_colname = NULL, recipient_iso_colname = NULL,
                               save_plot = F, output_path = NULL) {
  #' Function to produce summary statistics and produce plots
  #' @param dataset [data.frame/data.table] The dataset containing tagged COVID PAs to analyze
  #' @param amount_colname [str] Optional name of the column in [dataset] which contains commitment/disbursement/pledge/prepayment amount. Must be present if you want to specify [recipient_iso_colname]
  #' @param recipient_iso_colname [str] Optional name of the column in [dataset] which contains the ISO code for each recipient country
  #' @param save_plot [boolean] True/False value whether you want to save the generated plot
  #' @param output_path [str] Only applicable when [save_plot] is True. The path where the generated plot should be saved
  
  # Cast dataset at data.table
  dataset <- setDT(copy(dataset))
  
  # Handle user errors
  if (save_plot) {
    if (is.null(output_path)) {
      cat(paste0(red('  ERROR: PARAMETER [save_plot] WAS SPECIFIED AS [TRUE], BUT NO [output_path] WAS PROVIDED!!\n')))
      return(dataset)
    }
  }
  if (!is.null(amount_colname)) {
    if (amount_colname %ni% names(dataset)) {
      cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [amount_colname]!! ENSURE [amount_colname] IS ONE OF THE FOLLOWING:'), paste(names(dataset), collapse=', '), '\n')
      return(dataset)
    }
    if (!is.null(recipient_iso_colname)) {
      if (recipient_iso_colname %ni% names(dataset)) {
        cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [recipient_iso_colname]!! ENSURE [recipient_iso_colname] IS ONE OF THE FOLLOWING:'), paste(names(dataset), collapse=', '), '\n')
        return(dataset)
      }
    }
  }
  if (is.null(amount_colname) & !is.null(recipient_iso_colname)) {
    cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [amount_colname]!! BECAUSE YOU SPECIFIED A [recipient_iso_colname], YOU MUST ALSO PROVIDE AN [amount_colname]\n'))
    return(dataset)
  }
  
  cat('  SUMMARY STATISTICS:\n  ')
  
  # Generate summary stats
  PAs <- toupper(names(yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_parameters.yml"))$covid_keywords$english))
  stats <- list()
  for (area in c(PAs, 'OTHER')) {
    stat <- nrow(dataset[get(paste0(tolower(area), '_prop')) != 0,])
    stats[[area]] <- stat
    cat(paste0('  ', yellow(area), green(' was tagged in '), stat, green(' projects;')))
    rm(area, stat)
  }
  cat('\n')
  # Place summary stats in data.table
  pa_map <- data.table('PA' = c(PAs, 'OTHER'), 'pa_name' = c('Country-level coordination, \nplanning, monitoring, \nand evaluation',
                                                             'Risk communication and \ncommunity engagement', 
                                                             'Surveillance, rapid-response \nteams, and case \ninvestigation',
                                                             'National labs and \ntesting', 'Infection prevention and \ncontrol',
                                                             'Case management and \ntreatment', 'Supply chain and \nlogistics',
                                                             'Maintaining essential \nhealth services and \nsystems', 'R&D', 
                                                             'Points of entry, \ninternational travel, \nand transport', 'Other'))
  pa_dt <- setDT(stats)
  pa_dt <- melt(pa_dt, measure.vars = names(pa_dt))
  pa_dt[, `:=`(dummy = 0, prop = value/sum(unlist(stats)))]
  setnames(pa_dt, c('variable', 'value'), c('PA', 'count'))
  pa_dt$PA <- as.character(pa_dt$PA)
  pa_dt <- merge(pa_dt, pa_map, by='PA')
  pa_dt <- pa_dt[order(PA),]
  rm(stats)
  
  # Calculate PA amounts (if provided)
  if (!is.null(amount_colname)) {
    amt_dt <- copy(dataset)
    for (area in c(PAs, 'OTHER')) {
      amt_dt[, eval(paste0(tolower(area), '__amt')) := get(amount_colname) * get(paste0(tolower(area), '_prop'))]
    }
    # Collapse sum and reshape
    amt_dt <- collapse(amt_dt, 'sum', '', names(amt_dt)[names(amt_dt) %like% '__amt'])
    amt_dt <- melt(amt_dt, measure.vars = names(amt_dt))
    setnames(amt_dt, c('variable', 'value'), c('PA', 'amount'))
    amt_dt$PA <- toupper(gsub('__amt', '', amt_dt$PA))
    amt_dt <- merge(amt_dt, pa_map, by='PA')
  }
  
  if (!is.null(recipient_iso_colname)) {
    iso_dt <- copy(dataset)
    iso_dt <- collapse(iso_dt, 'sum', recipient_iso_colname, amount_colname)
    setnames(iso_dt, c(recipient_iso_colname, amount_colname), c('loc', 'amt'))
  }
  
  cat('  Generating plot...\n')
  
  # Generate plot of program areas
  plot <- ggplot(data = pa_dt) +
    geom_bar(mapping=aes(x=dummy, y=prop, fill=pa_name), position='stack', stat='identity', color='white') +
    theme_bw() +
    scale_color_brewer(palette = 'Spectral', direction = -1) +
    labs(x='Total', y='Program Area Proportion', fill='Program Area',
         title='Distribution of COVID Program Areas Tagged') +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  
  # If an amount_colname was specified, make another plot
  if (!is.null(amount_colname)) {
    cat('    Adding extra bar chart\n')
    p1 <- ggplot(data = amt_dt) +
      geom_col(mapping=aes(x=pa_name, y=amount, fill=pa_name), color='white') +
      theme_bw() +
      scale_color_brewer(palette = 'Spectral', direction = -1) +
      scale_y_continuous(labels = comma) +
      labs(x='Program Area', y='Amount', fill='Program Area',
           title='Funding Allocation by COVID Program Area')  +
      theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1), legend.position = 'none')
    
    if (!is.null(recipient_iso_colname)) {
      p2 <- ggplot(data = iso_dt) +
        geom_col(mapping=aes(x=loc, y=amt, fill=loc), color='white') +
        theme_bw() +
        scale_color_brewer(palette = 'Spectral', direction = -1) +
        scale_y_continuous(labels = comma) +
        labs(x='Recipient ISO Code', y='Amount', fill='Recipient ISO Code',
             title='Funding Allocation by Recipient ISO Code')  +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1), legend.position = 'none')
      p1 <- grid.arrange(p1, p2, ncol=1)
    }
    # Arrange this plot next to existing plot
    plot <- grid.arrange(plot, p1, nrow=1)
  }
  
  plot(plot)
  
  # Save the plot if desired
  if (save_plot) {
    cat(paste0('  Saving plot to: ', output_path))
    ggsave(plot = plot, filename = paste0(output_path, 'COVID_kws_report.png'), device = 'png', width = 15, height = 10)
  }
}
#---------------------------# ####









## database functions







# Return dah.roots
if (!exists("dah.roots"))  {
  dah.roots <- set_roots()
}
