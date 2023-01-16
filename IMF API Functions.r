# IMF API FUNCTIONS ############################################################
# ! MUST RUN/SAVE THESE BEFORE PULLING IN IMF DATA
#******************************************************************************#
#*
#*
#******************************************************************************#
# Function: Get Database Name and Description ----------------------------------
##+ Saves on Databases you are observing
#******************************************************************************#

getIMF_DbInfo <- function(DB_table) {
  #Note: DB_Table must be a table with the Database ID/Codes in a column "database_id"
  
  ## Pull List of Database IDs and Names from IMF API
  url_imf_datainfo <- 'http://dataservices.imf.org/REST/SDMX_JSON.svc/Dataflow'
  datainfo_imp <- fromJSON(url_imf_datainfo)
  
  datainfo_id <- datainfo_imp$Structure$Dataflows$Dataflow$KeyFamilyRef$KeyFamilyID
  datainfo_name <- datainfo_imp$Structure$Dataflows$Dataflow$Name$`#text`
  datainfo_final <- data.frame(database_id = datainfo_id, db_name = datainfo_name) %>%
    mutate(database_name = gsub(r"{\s*\([^\)]+\)}","",as.character(db_name))) %>%
    select(-db_name)
  
  ## Filter to only relevant Databases
  table <- datainfo_final %>%
    subset(database_id %in% DB_table$database_id) %>%
    
    return(table)
}


#******************************************************************************#
# METADATA: Get Metadata for selected Databases --------------------------------
##+ 
#******************************************************************************#

getIMF_MetaData <- function(db_NamesID) {
  
  #IMF Metadata URL
  url_imf_metadata <- 'http://dataservices.imf.org/REST/SDMX_JSON.svc/GenericMetadata'
  
  #Prepare Blank Data Frame
  meta_data_df <- data.frame()
  
  ## First Loop: Pull Metadata for Each Database ----
  for (i in rownames(db_NamesID)) {
    
    ### Set DB and corresponding URL
    database_id <- as.character(db_NamesID[i,"database_id"])
    url_imf_metadata_db <- sprintf('%s/%s/', url_imf_metadata, database_id)
    
    ### Pull full data from JSON
    metadata_imp <- fromJSON(url_imf_metadata_db)
    # assign(paste("metadata_imp", database_id, sep="_"), metadata_imp) #Keeps original import for reference
    
    ### Only Keep section that we need
   metadata01 <- metadata_imp[["GenericMetadata"]][["MetadataSet"]][["AttributeValueSet"]][["ReportedAttribute"]]
    length_metadata <- length(lapply(metadata01,length))
    
    ## Second Loop: Look at each part of list and make table of Desired Metadata Information ----
    for (j in 1:length_metadata) {
      
      metadata_type <- metadata01[[j]][["ReportedAttribute"]][[1]][["@conceptID"]]
      
      if (metadata_type == 'FREQ' | metadata_type == 'REF_AREA') {next} # We do not care about this metadata
      
      attr_code <- metadata01[[j]]$ReportedAttribute[[1]]$Value$`#text`
      if (attr_code == "All_Indicators") {next}
      
      metadata_final <- metadata01[[j]]$ReportedAttribute[[2]] %>%
        mutate(db_code = database_id) %>%
        mutate(metadata_type = metadata_type) %>%
        mutate(attr_code = attr_code) %>%
        mutate(metadata_attribute = `@conceptID`) %>%
        mutate(metadata_value = Value$`#text`) %>%
        subset(select = c("db_code","metadata_type","attr_code","metadata_attribute","metadata_value"))
      
      
      ### Put in longer table with other indicators ------------------------------------
      
      meta_data_df <- rbind(meta_data_df,metadata_final)
      
      ### Return Table
      
      
    }
  }
  return(meta_data_df)
}

#******************************************************************************#
# DATA: Get Data for selected DB/Indicators and Countries ----------------------
##+ 
#******************************************************************************#

getIMF_Data <- function(dbIndicators_table) {
  
  # Compact Data URL --
  url_imf_data <- 'http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData'
  
  #Prepare Blank Data Frame
  data_df <- data.frame()
  
  ## First Loop: Pull Data for Each Database ----
  for (i in rownames(dbIndicators_table)) {
    
    ### Set DB and corresponding URL ----
    db_code <- as.character(dbIndicators_table[i,"database_id"])
    indic_code <- as.character(dbIndicators_table[i,"indicator_code"])
    #print(db_code)
    #print(indic_code)
    
    if (db_code =="PCPS") {countries = "W00"
    } else {countries = country_split}
    
    for (u in 1:length(countries)) {
      if (db_code =="PCPS") { country_sub = countries
      } else {country_sub_list <- country_split[u] %>% unlist
      country_sub <- paste(country_sub_list, sep = '', collapse = '+')
      }
      #print(country_sub)
      url_imf_data_db <- sprintf('%s/%s/.%s.%s?startPeriod=%s&endPeriod=%s',
                                 url_imf_data,db_code,country_sub,indic_code,date_start,date_end)
      
      
      ### Pull full data from JSON
      data_imp <- fromJSON(url_imf_data_db)
      # assign(paste("data_imp", db_code, u, sep="_"), data_imp) #Keeps original import for reference
      
      
      ### Get necessary sections 
      overview     <- data_imp$CompactData$DataSet$Series
      observations <- overview$Obs
      length_data <- length(lapply(observations,length))
      
      
      ## Second Loop: Look at each part of list and make table of Desired Metadata Information ----
      for (j in 1:length_data) {
        #Test if "Obs" column is null (i.e. missing data for country/indicator)
        isNullOb <- all(sapply(observations[j], is.null))
        if (isNullOb == TRUE) {next} #Skip if true
       
        ### Get data from this specific row ----
        sub_data <- observations[j] #This is the actual data (i.e year + amount)
        info_row <- overview[j,] %>% #This is the surrounding info about the data (i.e. indicator + country)
          mutate(rownum = j) %>%
          mutate(database_id = db_code) %>%
          subset(select = c(-Obs))
        
        data_final <- sub_data %>%
          lapply(as.data.frame, stringsAsFactors = FALSE,simplify = FALSE) %>%
          Map(cbind, ., rownum = j) %>%
          do.call(bind_rows, .) %>%
          left_join(info_row, by= "rownum")
        
        ### Get rid of "X." and "@" in the column names ----
        names(data_final) <- gsub(x = names(data_final), pattern = "X.", replacement = "")
        names(data_final) <- gsub(x = names(data_final), pattern = "\\@", replacement = "") 
        
        # assign(paste("data_", database_id, u, sep="_"), data_imp) #Keeps original table for reference
        
        ### Put in longer table with other indicators ------------------------------
        
        data_df <- bind_rows(data_df,data_final)
        

        
      }
      Sys.sleep(2)
      #Note: this was added after receiving the following error
      #under the theory that the error was caused by hitting a query threshold limit:
      #
      # Error in parse_con(txt, bigint_as_char) : 
      #   lexical error: invalid char in json text.
      #                                  <!DOCTYPE html PUBLIC "-//W3C//
      #                (right here) ------^
      

    }
    
  }
  
  return(data_df)
  
}
