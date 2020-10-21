#----# INFO #----# ####
# Script: DAH R Utils
# Description: Loads R utilities for DAH channel work
# Contributors: Kyle Simpson
#----------------# ####

#----# Load Required Libraries #----# ####
pacman::p_load(yaml, readxl, foreign, crayon, reticulate, stringi, plotly, haven, scales, chron, tidyverse, data.table, rjson, feather, bit64, readstata13)

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
  dataset <- setDT(dataset)
  
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
  dataset <- setDT(dataset)
  
  # rowSums the specified columns
  dataset[, eval(new_colname) := rowSums(dataset[, c(rowtotal_cols), with=F], na.rm=na.rm)]
  
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
  dataset <- setDT(dataset)
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
  dataset <- setDT(dataset)
  
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
  dataset <- setDT(dataset)
  
  # Check to make sure col_names are valid columns in dataset
  if (any(col_names %ni% names(dataset))) {
    cat(red(paste0('    ERROR: Invalid col_names specified.\n')))
    break
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
  dataset <- setDT(dataset)
  
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
      break
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
    break
  }
  
  # Ensure provided std_colname is valid
  yaml <- yaml.load_file(paste0(code_repo, "FUNCTIONS/dah_parameters.yml"))$valid_observations
  if (toupper(std_colname) %ni% names(yaml)) {
    cat(red('    ERROR: SUPPLIED INVALID PARAMETER: [std_colname]!! ENSURE [std_colname] IS ONE OF THE FOLLOWING:'), paste(names(yaml), collapse=', '), '\n')
    break
  }
  
  # Collect valid observations & ensure input dataset is a data.table
  valid_obs <- unname(unlist(yaml[toupper(std_colname)]))
  dataset <- setDT(dataset)
  rm(yaml)
  
  # Clean if std_colname == 'YEAR'
  if (toupper(std_colname) == 'YEAR') {
    valid_obs <- c(1990:dah.roots$report_year)
  }
  
  # Check for invalid observations of dataset_colname in dataset
  if (any(unique(dataset[, get(dataset_colname)]) %ni% valid_obs)) {
    violations <- unique(dataset[, get(dataset_colname)]) %ni% valid_obs
    cat(paste0(red('    ERROR: INVALID OBSERVATIONS OF ['), white(dataset_colname), red(']!! PLEASE RECODE THESE OBSERVATIONS ['), yellow(violations), red('] AS ONE OF THE FOLLOWING: ['), green(valid_obs), red(']\n')))
    break
  } else {
    cat(green(paste0('    SUCCESS: ALL OBSERVATIONS OF [', dataset_colname, '] ARE VALID!!')))
  }
}
#-------------------------------------# ####











## database functions







# Return dah.roots
if (!exists("dah.roots"))  {
  dah.roots <- set_roots()
}
