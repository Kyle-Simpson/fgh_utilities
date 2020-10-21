#----# Docstring #----# ####
# Project:  FGH 2019
# Purpose:  Creating graphs to compare old and new FGH compiled channel data
# Date:     10/17/2019
# Author:   Kyle Simpson

#' Takes a formatted dataset of FGH channel-level data and produces one stacked bar graph, two scatter plots, and two csv files comparing desired aspect of dataset.
#' Data must be collapsed & long by \param{toCompare} and year. Expects at least four columns.  One column must be "dah_old" containing the DAH estimates from the previous FGH cycle.  
#' One column must be named "dah_new" containing the DAH estimates from the current FGH cycle.  One column must be "year" containing the specific year for which the row's estimates represent.
#' 
#' @param inputData [data.frame/data.table] A data frame or data table with at least four columns, three of which must be "dah_old", "dah_new", and "year". Data must be collapsed and long by \param{toCompare} and year.
#' @param toCompare [str] The name of the column you want to compare. Case Sensitive.
#' @param save_graphs [logical] Default is TRUE.  If TRUE, the graphs and csv files will be output.
#' @param savepath [str] Default is the current working directory. Specify exact path to desired directory.
#' @param outlier_label [logical] Default is TRUE.  If TRUE, the scatterplots will label outlier points in red.
#' @param outlier_pc [int] Default set to 100%.  The percent at which an outlier will be flagged.
#' 
#---------------------# ####


#----# Environment Prep #----# ####
if (!exists("code_repo"))  {
  code_repo <- unname(ifelse(Sys.info()['sysname'] == "Windows", "H:/repos/fgh/", paste0("/ihme/homes/", Sys.info()['user'][1], "/repos/fgh/")))
}
source(paste0(code_repo, 'FUNCTIONS/utils.R'))
#----------------------------# ####




#-------------------------------#
#----# Function definition #----#
#-------------------------------#

fgh_graphs <- function(inputData, toCompare, save_graphs = TRUE, savepath = getwd(), outlier_label = TRUE, outlier_pc = 100) {
  
  #------------# Exception Handling #------------# ####
  # Check if savepath is valid directory
  ensure_dir(filepath = savepath)
  
  # Check if outlier percentage outside bounds of 0:100
  if (outlier_pc > 100 | outlier_pc < 0) {
    stop(paste0("Provided 'outlier_pc' is outside valid range of percentages."))
  }
  
  # Check if inputData contains valid columns
  if ("year" %ni% colnames(inputData) |
      "dah_old" %ni% colnames(inputData) |
      "dah_new" %ni% colnames(inputData) |
      toCompare %ni% colnames(inputData)) {
    stop(paste0("Provided 'inputData' is missing one of the following columns: 'year' 'dah_old' 'dah_new' '", toCompare, "'"))
  }
  
  # Check if data is collapsed by year & toCompare
  # if (length(unique(inputData[[toCompare]])) != nrow(inputData[inputData$year == max(inputData$year), ])) {
  #   stop(paste0("Provided 'inputData' is not collapsed by `year` and `", toCompare, "`"))
  # }
  #----------------------------------------------# ####
  
  
  
  #------------# Data Prep #------------# ####
  # Ensure data is a data.table 
  if(!is.data.table(inputData)) {
    inputData <- setDT(inputData)
  }
  
  # Subset internal data to only what will be used
  toKeep <- c("year", "dah_old", "dah_new", toCompare)
  data <- copy(inputData[, toKeep, with=FALSE])
  #-------------------------------------# ####
  
  
  
  #------------# Stacked Bar Graph #------------# ####
  cat("#-----------------------------------------#\n    Generating stacked bar graph. \n")
  
  stacked_data <- copy(data)
  stacked_data <- melt(data, measure=c("dah_old", "dah_new"))
  
  # Fill NA values with 0s
  stacked_data$value <- ifelse(is.na(stacked_data$value), 0, stacked_data$value)
  
  # Generate fake year to plot stacked dah_old and dah_new side-by-side
  stacked_data$fake_year <- ifelse(stacked_data$variable == "dah_new", 
                                   stacked_data$year + 0.25, stacked_data$year - 0.25)
  stacked_data$fake_year <- as.factor(stacked_data$fake_year)
  
  # Generate axis labels
  years <- min(stacked_data$year):max(stacked_data$year)
  years <- rep(years, each=2)
  
  # Make axis label color
  axis_color <- rep(c('black', 'black', 'dark grey', 'dark grey'), length(years)/4)
  
  # Stacked bar graph
  options(warn=-1) # mute axis.text.x color warning
  stacked_bar <- ggplot(data = stacked_data) +
    geom_col(mapping=aes(x=fake_year, y=value, fill=get(toCompare)), position="stack") +
    scale_x_discrete(labels = years) +
    scale_y_continuous(expand = c(0.01, 0), labels = comma) +
    labs(fill=toCompare, x="Year", y="",
         title=paste0("DAH by ", toCompare),
         caption = "Left stack is dah_old") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1, color = axis_color))
  options(warn=0)
  #---------------------------------------------# ####
  
  
  
  #------------# Level Space Scatterplot #------------# ####
  cat("    Generating level scatter plot. \n")
  
  level_scatter_data <- copy(data)
  
  # Generate outlier flag
  level_scatter_data[is.na(level_scatter_data)] <- 0
  level_scatter_data[, pc_diff := (100 * (dah_new - dah_old) / dah_old), ]
  
  # Replace percent_difference rows where NA
  level_scatter_data$pc_diff <- ifelse(is.na(level_scatter_data$pc_diff), 0, level_scatter_data$pc_diff)
  
  # Scatter plot
  level_scatter <- ggplot(data = level_scatter_data) +
    geom_point(mapping=aes(x=dah_old, y=dah_new),
               color = ifelse(level_scatter_data[, year] == 2019, "green",
                              ifelse(abs(level_scatter_data[, pc_diff]) >= outlier_pc, "red", "blue"))) +
    geom_text(hjust=-0.1, mapping = aes(x=dah_old, y=dah_new, label=ifelse(abs(level_scatter_data[, pc_diff]) >= outlier_pc,
                                                                           paste0(level_scatter_data[, get(toCompare)], ", ", level_scatter_data[, year]),
                                                                           "")),
              color=ifelse(level_scatter_data[, year] == 2019, "green", "red")) +
    geom_abline(slope = 1, intercept = 0, color="red", linetype="dashed") +
    labs(title=paste0(toupper(toCompare), " New vs Old: Level Space"))
  #---------------------------------------------------# ####
  
  
  
  #------------# Log Space Scatterplot #------------# ####
  cat("    Generating log scatter plot. \n")
  
  log_scatter_data <- copy(data)
  
  # Fill NA values with 0
  log_scatter_data$dah_old <- ifelse(is.na(log_scatter_data$dah_old), 0, log_scatter_data$dah_old)
  log_scatter_data$dah_new <- ifelse(is.na(log_scatter_data$dah_new), 0, log_scatter_data$dah_new)
  
  # Drop dah_old and dah_new values that = 0
  log_scatter_data <- log_scatter_data[(dah_old > 0) & (dah_new > 0), ]
  
  # Generate log() versions of dah_old and dah_new
  log_scatter_data[, log_dah_old := log(dah_old),]
  log_scatter_data[, log_dah_new := log(dah_new),]
  
  # Generate outlier flag
  log_scatter_data[, log_pc_diff := (100 * (log(log_dah_new) - log(log_dah_old))),]
  
  # Scatterplot
  log_scatter <- ggplot(data = log_scatter_data) +
    geom_point(mapping=aes(x=dah_old, y=dah_new),
               color = ifelse(abs(log_scatter_data[, log_pc_diff]) >= log(outlier_pc), "red", "blue")) +
    geom_text(hjust=-0.1, mapping = aes(x=dah_old, y=dah_new, label=ifelse(abs(log_scatter_data[, log_pc_diff]) >= log(outlier_pc),
                                                                           paste0(log_scatter_data[, get(toCompare)], ", ", log_scatter_data[, year]),
                                                                           "")), color="red") +
    geom_abline(slope = 1, intercept = 0, color="red", linetype="dashed") +
    scale_x_log10(labels = comma) +
    scale_y_log10(labels = comma) +    labs(title=paste0(toupper(toCompare), " New vs Old: Log Space")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Remove unneeded cols
  log_scatter_data[, c("log_dah_old", "log_dah_new") := NULL]
  #-------------------------------------------------# ####
  
  
  
  #------------# Save out graphs and csv's #------------# ####
  # If user wants to save graphs
  if(save_graphs) {
    
    # If the user didn't specify an output directory
    if(savepath == getwd()) {
      date <- format(Sys.Date(), "%b_%d_%Y")
      
      # Check if directory exists
      if(!dir.exists(paste0(getwd(), "/fgh_graphs_output_", date, "/"))) {
        
        dir.create(paste0(getwd(), "/fgh_graphs_output_", date, "/"))
        
        cat(paste0("Auto-generated directory created by FGH_GRAPHS.R on ", date, " at ", 
                   times(strftime(Sys.time(),"%H:%M:%S")), " by ", Sys.info()[7], "@uw.edu."),
            file = paste0(getwd(), "/fgh_graphs_output_", date, "/README.txt"))
      }
      
      # Set savepath
      savepath <- paste0(getwd(), "/fgh_graphs_output_", date, "/")
    }
    
    cat(paste0("    Saving graphs to: ", savepath, "\n"))
    
    # Stacked bar
    ggsave(filename = paste0(savepath, "stacked_bar_by_", toCompare, ".png"), stacked_bar, width=10, height=10)
    
    # Level scatter and csv
    ggsave(filename = paste0(savepath, "level_scatter_of_", toCompare, ".png"), level_scatter, width=10, height=10)
    fwrite(level_scatter_data, file= paste0(savepath, "percentage_diffs_", toCompare, ".csv"))
    
    # Log scatter and csv
    ggsave(filename = paste0(savepath, "log_scatter_of_", toCompare, ".png"), log_scatter, width=10, height=10)
    fwrite(log_scatter_data, file= paste0(savepath, "absolute_diffs_", toCompare, ".csv"))
  }
  #-----------------------------------------------------# ####
  
  cat("    Function complete. \n#-----------------------------------------#\n")
}




