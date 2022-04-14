# Jennifer Collister
# 22/09/20

library(DBI)
library(duckdb)
library(tidyverse)
library(knitr)
library(kableExtra)
library(R.cache)
setCacheRootPath(path="Data/Cache")

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

# Create an environment for the dependency functions
DBfunc <- new.env()

# Source the variable maps into the TEUmaps environment
source(file.path(config$scripts$cleaning, "TEU_specifications.R"))
source(file.path(config$scripts$cleaning, "DuckDB.R"), local=DBfunc)

# Note - may in future want to use https://cran.r-project.org/web/packages/modules/
# This means stuff in the modules *can't* see and interact with stuff in global env
# Better practice


derive_variables <- function(database=config$data$database, field_definitions, 
                             exclusions=function(x){x}, dictionary=NULL,
                             name_map = config$cleaning$renaming,
                             withdrawals = config$cleaning$withdrawals){
  
  # Extract the lists from the list of functions
  objects <- Map(function(p) {if(is.function(p)) {p()} else {p}}, field_definitions)
  
  # Find the names of the requested columns
  outcols <- sapply(objects, function(x) x$name)
  
  # Remove duplicated variables that have been entered more than once in spec
  objects <- objects[!duplicated(outcols)]
  outcols <- outcols[!duplicated(outcols)]
  
  # And the names of all the source columns required
  source_cols <- unique(unlist(sapply(objects, function(x) x$source)))
  
  # Extract data fields from database
  data <- DBfunc$DB_extract(source_cols, db = database, name_map = name_map, withdrawals = withdrawals)

  # Separate into derivations to be calculated before and after exclusion criteria
  before <- objects[sapply(objects, function(x) x$post_exclusion==FALSE)]
  after <- objects[sapply(objects, function(x) x$post_exclusion==TRUE)]
  
  data <- DBfunc$source_rotation(data, field_definitions = before)
  
  data <- exclusions(data)
  # Note - future update
  # Make exclusions a list of functions, each function is a separate exclusion criterion
  # Then the derive_variables() function can handle the row counts
  # And can write individual documentation for each exclusion criterion to be output nicely
  
  data <- DBfunc$source_rotation(data, field_definitions = after)
  
  # Return only requested columns
  data <- data[,outcols[outcols %in% colnames(data)]]
  
  if(!is.null(dictionary)) {
    kable(DBfunc$make_dict(data, objects), "html", escape = FALSE) %>%
      kable_styling(bootstrap_options = c("hover", "condensed"), fixed_thead = T,
                    html_font = "arial", font_size = 12) %>%
      column_spec(3, width="20em") %>%
      column_spec(5, width="50em") %>%
      collapse_rows(columns = c(1, 2, 5, 6), target = 1, valign = "top") %>%
      cat("<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css\">", 
          paste0("<head><title>", str_remove(dictionary, ".html"), "</title></head>"),
          paste0("<h1>", str_remove(dictionary, ".html"), "</h1>"),
          paste0("<h2>", format(Sys.time(), '%d %B %Y'), "</h2>"),
          ., 
          file = dictionary)
  }
  
  return(data)
}


# Derive variables whose sources are available in the data first
DBfunc$source_rotation <- function(data, field_definitions) {
  
  data_cols <- colnames(data)
  
  while (length(field_definitions) > 0) {
    remove <- vector()
    for (d in seq(1, length(field_definitions), by=1)) {
      
      # Iterate over the derivation objects
      defn <- field_definitions[[d]]
      
      if (all(defn$source %in% data_cols)) {
        
        # If all required source columns are available, derive them
        tryCatch(
          data <- DBfunc$derive_fn(data, field_definition = defn),
          error=function(cond) {
            message(paste(defn$name, "threw the following error: "))
            message(cond)
            return(data)
          },
          warning=function(cond) {
            message(paste(defn$name, "threw the following warning: "))
            message(cond)
            return(data)
          }
        )
        
        
        # Add the variable to the list of available data columns
        data_cols <- append(data_cols, defn$name)
        
        # Keep a list of the indices of the derivations we've completed
        remove <- append(remove, d)
      }
    }
    if(length(remove)>0) {
      # Once variables have been derived, remove them from the list
      field_definitions <- field_definitions[-remove]
    } else {
      # If there are still derivation objects waiting but no new variables have been derived
      # Then we know that these objects can't be derived on the next loop either
      # We print out the objects so the user can see which they are
      warning(paste0("Source columns not available to derive variables: ", 
                     paste(sapply(field_definitions, function(x) x$name), collapse=", ")))
      # And exit the while loop
      break
    }
  }
  return(data)
}



# Actually do the deriving
DBfunc$derive_fn <- function(data, field_definition) {
  colfunc <- field_definition$mapper
  if(length(field_definition$source)>1){
    data[[field_definition$name]] <- colfunc(data[field_definition$source])
  } else {
    data[[field_definition$name]] <- colfunc(data[[field_definition$source]])
  }
  print(paste0("Derived ", field_definition$name))

  return(data)
}

# Generate the data dictionary
DBfunc$make_dict <- function(data, objects, na.rm=FALSE) {
  # Implementation ideas from https://github.com/dmrodz/dataMeta
  
  # Extract descriptive properties of the data, one variable at a time
  data_list = list()
  for(i in seq(1, length(names(data)))) {
    vartype <- class(data[[i]])
    
    if(vartype %in% c("factor", "logical", "character")) {
      tab <- data.frame(table(data[i], useNA = ifelse(na.rm==TRUE, "no", "ifany")))
      var_opt <- c("", as.character(tab[,1]))
      n <- c(length(data[i][!is.na(data[i])]), tab[,2])
    } else if (vartype %in% c("integer", "numeric")) {
      var_opt <- paste(range(data[i], na.rm = TRUE), sep = "", collapse = " to ")
      n <- length(data[i][!is.na(data[i])])
    } else if (vartype == "Date") {
      var_opt <- paste(min(data[[i]], na.rm = na.rm), max(data[[i]], na.rm = na.rm), sep = " to ")
      n <- length(data[i][!is.na(data[i])])
    } else {
      warning(paste0("Unrecognised variable type: ", vartype))
    }
    
    d <- data.frame(variable_name = names(data[i]),
                    variable_type = vartype,
                    variable_options = var_opt, 
                    n = n, 
                    stringsAsFactors = FALSE)
    d$i <- i
    data_list[[i]] <- d
  }
  # Join all the variables together into one dataframe
  dict = do.call(rbind, data_list) %>%
    as.data.frame %>% 
    select(-i)
  
  # Extract variable descriptions from the derivation objects
  linker <- data.frame(variable_name = sapply(objects, function(x) x$name),
                       variable_description = sapply(objects, function(x) x$description),
                       source_vars = sapply(objects, function(x) paste(DBfunc$name_to_fdot(x$source, link=TRUE), collapse=", ")))
  
  dict_df <- inner_join(dict, linker, by="variable_name")
  
  dictdf <- dict_df 
  # Before using kableExtra collapse_rows, had to remove duplicated var names and descriptions
  # Note this didn't work for var types - removed too many!
  #%>%
    # mutate(
    #   variable_name = ifelse(duplicated(variable_name), " ", as.character(variable_name)),
    #   variable_description = ifelse(duplicated(variable_description), " ", as.character(variable_description)),
    #   variable_type = ifelse(duplicated(variable_type), " ", as.character(variable_type))
    #   )
  
  dictdf <- as.data.frame(dictdf)
  
  return(dictdf)
}

# Create a switch function from variable names to display names
pretty_switch <- function(field_definitions, return_type="function"){
  
  objects <- Map(function(p) {if(is.function(p)) {p()} else {p}}, field_definitions)
  colnames <- lapply(objects, function(x) x$name)
  prettynames <- lapply(objects, function(x) x$display_name)
  names(prettynames) <- colnames
  
  if(return_type=="function") {
    TEUswitch <- function(x) {
      do.call("switch", c(x, c(prettynames,x)))
    }
    return(TEUswitch)
  } else if(return_type=="list"){
    return(prettynames)
  }
}

