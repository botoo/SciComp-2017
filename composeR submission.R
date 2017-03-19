composeR <- function(nexus){
  require(readr)
  require(stringr)
  
  file <- read_file(nexus) #reads in the file as a single string and stores it
  
  vector_version <- as.vector(file) #creates a vector-coerced version of the file for future use
  
  
  ###component functions:
  
  ####
  
  read.ntax <- function(vector_version){	#makes a function to read the taxon block
    ntax_match <- str_extract_all(vector_version, "NTAX=\\d+")	#find the 'NTAX=...' bit
    ntax_match <- str_extract_all(ntax_match, "\\d+")	#extracts the digits only- aka the actual number of taxa- from the match
    ntax_match <- unlist(ntax_match)	#flattens the one-item list recovered above
    ntax <- as.numeric(ntax_match)	#coerces to numeric
    return(ntax)
  }
  
  
  ###
  
  read.nchar <- function(vector_version){
    nchar_match <- str_extract_all(vector_version, "NCHAR=\\d+")	#find the 'NCHAR=...' bit
    nchar_match <- str_extract_all(nchar_match, "\\d+")	
    nchar_match <- unlist(nchar_match)	#flattens the one-item list recovered above
    nchar <- as.numeric(nchar_match)	#coerces to numeric
    return(nchar)	#spits out ntax for future use
  }
  
  
  ###
  read.matrix.codes <- function(vector_version){
    
    vector_versionUP <- toupper(vector_version)
    matrix_match <- str_extract_all(vector_versionUP, regex("MATRIX\\n\\t(.*?);", dotall=TRUE, multiline=TRUE)) 
    matrix_match[[1]] <- gsub("MATRIX", "", matrix_match[[1]])
    matrix_match_split <- str_match_all(matrix_match, "(\\w+?)\\s+([\\w\\?\\{\\s\\}\\-]+?)[\\n;]")
    
    matrix_codes_match <- matrix_match_split[[1]][,3]
    
    matrix_codes <- str_extract_all(matrix_codes_match, "\\{\\w\\s\\w\\}|.")
    
    return(matrix_codes)
  }
  
  ###
  
  read.matrix.names <- function(vector_version){
    vector_versionUP <- toupper(vector_version)
    matrix_match <- str_extract_all(vector_versionUP, regex("MATRIX\\n\\t(.*?);", dotall=TRUE, multiline=TRUE)) 
    matrix_match[[1]] <- gsub("MATRIX", "", matrix_match[[1]])
    matrix_match_split <- str_match_all(matrix_match, "(\\w+?)\\s+([\\w\\?\\{\\s\\}\\-]+?)[\\n;]")
    
    matrix_names <- matrix_match_split[[1]][,2]
    
    return(matrix_names)
    
  }
  ####
  
  create.composed.matrix <- function(matrix_codes, matrix_names){
    
    composed_matrix <- do.call(rbind, matrix_codes)
    
    rownames(composed_matrix) <- matrix_names
    
    return(composed_matrix)
  }
  
  
  ####
  
  create.csv.from.nexus <- function(composed_matrix){
    data_frame_version <- as.data.frame(as.matrix(composed_matrix), stringsAsFactors=FALSE)
    csv_version <- write.csv(data_frame_version, file="csv_version.csv")
    return(csv_version)
  }
  
  nchar <- read.nchar(vector_version)
  
  ntax <- read.ntax(vector_version)
  
  matrix_codes <- read.matrix.codes(vector_version)
  
  matrix_names <- read.matrix.names(vector_version)
  
  column_numbers <- 1:nchar
  
  composed_matrix <- create.composed.matrix(matrix_codes, matrix_names)
  
  csv_version <- create.csv.from.nexus(composed_matrix)
  
  cat("The number of taxa/OTUs in this dataset is ", ntax)
  
  cat("The number of characters in this dataset is", nchar)
  
}
