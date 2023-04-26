# IMF API FUNCTIONS ############################################################
# ! MUST RUN/SAVE THESE BEFORE PULLING IN IMF DATA
#******************************************************************************#

#******************************************************************************#
# Function: Get Database Name and Description ----------------------------------
##+ Saves on Databases you are observing
#******************************************************************************#

getIMF_dsInfo <- function() {
  #Note: DB_Table must be a table with the Database ID/Codes in a column "database_id"
  
  ## Pull full Dataset information from IMF API
  url_imf_dsInfo <- 'http://dataservices.imf.org/REST/SDMX_JSON.svc/Dataflow'
  ds_imp <- fromJSON(url_imf_dsInfo)
  
  # Pull out the ID and Name
  ds_id <- ds_imp$Structure$Dataflows$Dataflow$KeyFamilyRef$KeyFamilyID
  ds_name <- ds_imp$Structure$Dataflows$Dataflow$Name$`#text`
  ds_final <- data.frame(dataset_id = ds_id, ds_name) %>%
    mutate(dataset_name = gsub(r"{\s*\([^\)]+\)}","",as.character(ds_name))) %>%
    select(-ds_name)
  
  return(ds_final)
  
}


#******************************************************************************#
# METADATA: Get Metadata for selected Databases --------------------------------
##+ 
#******************************************************************************#

getIMF_MetaData <- function(dsID) {
  
  #IMF Metadata URL
  url_imf_metadata <- 'http://dataservices.imf.org/REST/SDMX_JSON.svc/GenericMetadata'

    ### Set URL with respective dataset ID
  url_imf_metadata_ds <- sprintf('%s/%s/', url_imf_metadata, dsID)
    
    ### Pull full data from JSON
    metadata_imp <- fromJSON(url_imf_metadata_ds)
    # assign(paste("metadata_imp", database_id, sep="_"), metadata_imp) #Keeps original import for reference
    
    ### Only Keep section that we need
   metadata01 <- metadata_imp[["GenericMetadata"]][["MetadataSet"]][["AttributeValueSet"]][["ReportedAttribute"]]
    length_metadata <- length(lapply(metadata01,length))
    
  #Prepare Blank Data Frame
    metadata_ds <- data.frame()
    
    ##  Loop: Look at each part of list and make table of Desired Metadata Information ----
    for (j in 1:length_metadata) {
      
      metadata_type <- metadata01[[j]][["ReportedAttribute"]][[1]][["@conceptID"]]
      
      #if (metadata_type == 'FREQ' | metadata_type == 'REF_AREA') {next} # Sometimes we do not care about this metadata
      
      attr_code <- metadata01[[j]]$ReportedAttribute[[1]]$Value$`#text`
      if (attr_code == "All_Indicators") {next}
      
      metadata02 <- metadata01[[j]]$ReportedAttribute[[2]] %>%
        mutate(ds_code = dsID) %>%
        mutate(metadata_type = metadata_type) %>%
        mutate(attr_code = attr_code) %>%
        mutate(metadata_attribute = `@conceptID`) %>%
        mutate(metadata_value = Value$`#text`) %>%
        subset(select = c("ds_code","metadata_type","attr_code","metadata_attribute","metadata_value"))
      
      
      ### Put in longer table with other indicators ------------------------------------
      
      metadata_ds <- rbind(metadata_ds,metadata02)
      
      ### Return Table
  }
  return(metadata_ds)
}

#******************************************************************************#
# DATA: Get Data for selected DB/Indicators and Countries ----------------------
##+ 
#******************************************************************************#

getIMF_Data <- function(dsID,indicatorID,iso,date_start,date_end) {
  
  # Compact Data URL --
  url_imf_data <- 'http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData'
  
  #Prepare Blank Data Frame
  data_df <- data.frame()
  
  # Get countries to pull. Will split if there is more than 10
    if (dsID =="PCPS") {
      countries = "W00"
    } else {countries = iso}
    
  country_split <- split(countries, ceiling(seq_along(countries)/10))
  
  #Create blank data frame
  countrystack_df <- data.frame()
    
    for (u in 1:length(country_split)) {
      country_sub_list <- country_split[u] %>% unlist
      country_sub <- paste(country_sub_list, sep = '', collapse = '+')


  #Create final URL with all relevant information (dataset, indicator, country, etc.)
      url_imf_data_db <- sprintf('%s/%s/.%s.%s?startPeriod=%s&endPeriod=%s',
                                 url_imf_data,dsID,country_sub,indicatorID,date_start,date_end)
      
      
      ### Pull full data from JSON
      data_imp <- fromJSON(url_imf_data_db)
      # assign(paste("data_imp", db_code, u, sep="_"), data_imp) #Keeps original import for reference
      
      
      ### Get necessary sections 
      overview     <- data_imp$CompactData$DataSet$Series
      observations <- overview$Obs
      length_data <- length(lapply(observations,length))
      
      ## Prepare blank dataframe
      data_df <- data.frame()
      
      ## Second Loop: Look at each part of list and make table of Desired Metadata Information ----
      for (j in 1:length_data) {
        #Test if "Obs" column is null (i.e. missing data for country/indicator)
        isNullOb <- all(sapply(observations[j], is.null))
        if (isNullOb == TRUE) {next} #Skip if true
       
        ### Get data from this specific row ----
        sub_data <- observations[j] #This is the actual data (i.e year + amount)
        info_row <- overview[j,] %>% #This is the surrounding info about the data (i.e. indicator + country)
          mutate(rownum = j) %>%
          mutate(dataset_id = ds_code) %>%
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
      countrystack_df <- bind_rows(countrystack_df,data_df)

      Sys.sleep(2)
      #Note: this was added after receiving the following error
      #under the theory that the error was caused by hitting a query threshold limit:
      #
      # Error in parse_con(txt, bigint_as_char) : 
      #   lexical error: invalid char in json text.
      #                                  <!DOCTYPE html PUBLIC "-//W3C//
      #                (right here) ------^
      

    }
  
  return(countrystack_df)
  
}
