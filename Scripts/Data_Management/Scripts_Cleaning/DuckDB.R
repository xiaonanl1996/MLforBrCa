


#' Reads a database of UKB data and returns a dataset containing the requested columns
#'
#' The database contains the data with field codes instead of variable names. It has been pre-processed by the UKB-generated R file to apply categorical variable levels and labels. This loads selecteed variables from the database, applies the chosing naming convention and derives requested variables.
#'
#' @param col_list The objects defining derived variables you want in your dataset.
#' @param db The path to the database you want to extract the variables from.
#' @param name_map The path to the .csv mapping file containing human-readable names for the raw UKB data fields. 
#'
#' @return Returns a data.frame containing the variables requested.
#'
#' @import DBI
#' @import duckdb
#' @export
#' @examples
#' \dontrun{
#' # Extract variables for HTN project from V2 data
#'
#' DB_extract(HTNcols, db="ukb_v2.db")
#' }
#'
DB_extract <- function(extract_cols, db = config$data$database, 
                       name_map = config$cleaning$renaming,
                       withdrawals = config$cleaning$withdrawals){
  
  mapping <- read.csv(name_map, stringsAsFactors = FALSE)
  withdrawals <- read.csv(withdrawals, header=FALSE)
  withdrawn_ids <- withdrawals$V1
  
  # Connect to the database
  con <- dbConnect(duckdb::duckdb(), db, read_only=TRUE)
  on.exit(dbDisconnect(con, shutdown=TRUE))
  
  # List all tables available in the database
  # Each should correspond to one data download
  tables <- dbListTables(con)
  
  # Convert column names to raw form
  cols_fdot <- name_to_fdot(extract_cols, mapping)
  # Join all download tables to get all data and extract requested columns
  view <- lapply(tables, 
                 function(x) tbl(con, from=x) %>% select(any_of(cols_fdot))
                 ) %>% 
    reduce(inner_join, by = "f.eid", suffix = c("", ".delete")) %>%
    select(-ends_with(".delete")) %>% # Remove any duplicate columns
    filter(!(f.eid %in% withdrawn_ids)) %>% # Exclude participants who have withdrawn
    filter(f.eid != 6025392) %>% # NOTE: HACKY FIX TO DEAL WITH BROKEN PARTICIPANT
    collect %>%
    rename_with(fdot_to_name, mapping=mapping)
  
  return(view)
}



# Maps UKB variable names to human readable names according to the given mapping
#
# UKB variable names of the form f.XXXXX.0.0 are converted to TLA_VarName.0.0
#
# @param ukb_col A vector of UKB variable names
# @param mapping A dataframe with the mapping between UKB field IDs and human readable variable names
#
fdot_to_name <- function(ukb_col, mapping) {
  ukb_col <- strsplit(ukb_col, split = ".", fixed = TRUE)
  ukb_col <- sapply(ukb_col, function(x) {
    if (as.character(x[2]) %in% mapping$Field_ID) {
      # Swap the field ID for a human-readable variable name
      x[2] <- mapping$NewVarName[mapping$Field_ID == as.character(x[2])]
    } else {
      print(x[2])
    }
    # Remove the 'f'
    x <- x[-1]
    # Stick it back together with the instances and measurements
    x <- paste(x, collapse = ".")
    return(x)
  })
  return(ukb_col)
}


# Maps human readable names to UKB variable names according to the given mapping
#
# Human readable names of the form TLA_VarName.0.0 are converted to UKB variable names f.XXXXX.0.0
#
# @param col_list A vector of human-readable names
# @param mapping A dataframe with the mapping between UKB field IDs and human readable variable names
#
name_to_fdot <- function(col_names, 
                         mapping = read.csv(config$cleaning$renaming, stringsAsFactors = FALSE),
                         link = FALSE) {
  col_names <- strsplit(col_names, split = ".", fixed = TRUE)
  col_names <- sapply(col_names, function(x){
    if(x[1] %in% mapping$NewVarName) {
      code <- mapping$Field_ID[mapping$NewVarName == x[1]]
      x[1] <- code
      x <- c("f", x)
      x <- paste(x, collapse=".")
      
      if(link==TRUE) {
        x <- text_spec(x, link = paste0("http://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=", code))
      }
    } else {
      x <- paste(x, collapse=".")
    }
    return(x)
  })
  return(col_names)
}
