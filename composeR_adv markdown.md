---
title: "composeR_adv markdown"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This is a Markdown document meant to give some explanation for the composeR_adv function.  Compare the counterpart document for composeR.  Note: this is a marked-up, broken-down look at the function.  The file won't actually execute.

composeR_adv is a (presently nonfunctional as of March 19 2017) version of composeR, designed to be more ambitious (indeed, composeR_adv can be considered an attempt to more fully realize the original project goals.)  The main differences are that this version is meant to, if they are present in the file, capture the taxon labels and character state labels and use them as the row and column names when creating the csv file.  As not every Nexus file has taxon labels or character state labels they were judged non-essential to the basic initial functioning of composeR but important enough to merit further work dealing with them.

There are several conditionals (using regex) that check to see if the taxa block and character state block are present.  Correspondingly, there are new and altered versions of several component functions, grouped in the code as "component functions- secondary".  They are:

-read.taxa.labels
-read.char.labels
-create.composed.matrix.adv
-create.csv.from.nexus.adv

The modifications are largely if statements enabling use of the taxa labels/character labels if they're recovered properly.

KNOWN ISSUES:
-The regex meant to read the character labels (in read.char.labels) splits over numbers, so if a character description contains a number it will be split into 2 (or more) entries.  

=======
composeR_adv
=======

```{r}
composeR_adv <- function(nexus){
  require(readr)
  require(stringr)
  
  file <- read_file(nexus) #reads in the file as a single string and stores it
  
  vector_version <- as.vector(file) #creates a vector-coerced version of the file for future use
  
  
  ###component functions- primary###
  
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
    matrix_match_split <- str_match_all(matrix_match, "(\\w+?)\\s+([\\w\\?\\{\\s\\}\\-]+?)[\\n;]") #brackets with //n and ; are for closer, [] are 'or'
    
    matrix_codes_match <- matrix_match_split[[1]][,3]
    
    matrix_codes <- str_extract_all(matrix_codes_match, "\\{\\w\\s\\w\\}|.")
    
    return(matrix_codes)
  }
  
  ###
  
  read.matrix.names <- function(vector_version){
    vector_versionUP <- toupper(vector_version)
    matrix_match <- str_extract_all(vector_versionUP, regex("MATRIX\\n\\t(.*?);", dotall=TRUE, multiline=TRUE)) 
    matrix_match[[1]] <- gsub("MATRIX", "", matrix_match[[1]])
    matrix_match_split <- str_match_all(matrix_match, "(\\w+?)\\s+([\\w\\?\\{\\s\\}\\-]+?)[\\n;]") #brackets with //n and ; are for closer, [] are 'or'
    
    matrix_names <- matrix_match_split[[1]][,2]
    
    return(matrix_names)
    
  }
  ####
  
  create.composed.matrix <- function(matrix_codes, matrix_names){
    
    composed_matrix <- do.call(rbind, matrix_codes)
    
    rownames(composed_matrix) <- matrix_names
    
    return(composed_matrix)
  }
  
  create.csv.from.nexus <- function(composed_matrix){
    data_frame_version <- as.data.frame(as.matrix(composed_matrix), stringsAsFactors=FALSE)
    csv_version <- write.csv(data_frame_version, file="csv_version.csv")
    return(csv_version)
  }
```  

These are the composeR_adv-specific functions:
```{r}
  ###component functions- secondary###
  
  read.taxa.labels <- function(vector_version, tail) {
    vector_versionUP <- toupper(vector_version)
    taxa_labels_match <- str_extract_all(vector_versionUP, regex("TAXLABELS(.*?);", dotall=TRUE, multiline = TRUE))
    split <- " "
    taxa_labels_match <- as.character(taxa_labels_match)
    taxa_labels_match <- strsplit(taxa_labels_match, split)
    taxa_labels_match <- taxa_labels_match[[1]]
    taxa_labels_match <- taxa_labels_match[-tail]
    clean_leader <- str_match_all(taxa_labels_match[1], "([A-Z])\\w+")
    taxa_labels_match[1] <- clean_leader[[1]][2,1]
    taxa_labels <- taxa_labels_match
    return(taxa_labels)
  }
  
  read.char.labels <- function(vector_version, nchar){
    vector_versionUP <- toupper(vector_version)
    character_labels_match = str_extract_all(vector_version, regex("CHARSTATELABELS(.*?);", dotall=TRUE, multiline=TRUE)) #seems to be matching most, if not all, of the file somehow
    character_labels_match <- str_extract_all(character_labels_match, regex("\\d+(.*?)(,|;)", dotall=TRUE, multiline=TRUE))
    character_labels_match <- character_labels_match[[1]]
    character_labels <- character_labels_match
    
    
    return(character_labels)	#spits out character labels for future use
  }

  create.composed.matrix.adv <- function(matrix_codes, matrix_names, taxa_labels, character_labels, column_numbers, taxlabels_green, charstatelabels_green){
    
    composed_matrix <- do.call(rbind, matrix_codes)
    
    if(charstatelabels_green == TRUE){
    colnames(composed_matrix) <- character_labels  
    } else{
    colnames(composed_matrix) <- column_numbers  
    }
    
    if(taxlabels_green==TRUE){
    rownames(composed_matrix) <- taxa_labels
    }else{
     rownames(composed_matrix) <- matrix_names 
    }
    return(composed_matrix)
  }  
  
  create.csv.from.nexus.adv <- function(composed_matrix, taxa_labels, character_labels, taxlabels_green, charstatelabels_green){
    data_frame_version <- as.data.frame(as.matrix(composed_matrix), stringsAsFactors=FALSE)
    csv_version <- write.csv(data_frame_version, file="csv_version.csv")
    if(taxlabels_green==TRUE){
      cat("The taxon labels for this dataset are as follows ", taxa_labels)
    }
    if(charstatelabels_green==TRUE){
      cat("The character labels for this dataset are as follows ", character_labels)
    } else{
      print("The number of characters recovered by the program did not match the number of characters stated to be in the dataset.")
    }
    return(csv_version)
  }   
  

```

And here is the main body of the function:
```{r}
  ###main function###
  nchar <- read.nchar(vector_version)
  
  ntax <- read.ntax(vector_version)
  
  tail <- ntax + 1
  
  column_numbers <- 1:nchar
  
  taxlabels_green = FALSE

  charstatelabels_green = FALSE
  
  if (grep("CHARSTATELABELS", vector_version, ignore.case=TRUE) !=0){
    charstatelabels_green = TRUE
    character_labels <- read.char.labels(vector_version, nchar)
    if (length(character_labels) != nchar){
      charstatelabels_green = FALSE
      print("The number of characters recovered did not match the number of characters stated in the file")
    }
  }
  
  if (grep("TAXLABELS", vector_version, ignore.case=TRUE) != 0){
    taxlabels_green = TRUE
    taxa_labels <- read.taxa.labels(vector_version, tail)
    # return(taxlabels_green)
    }

  # taxa_label_length <- length(taxa_labels)
  # 
  # if (taxa_label_length != ntax){
  #   taxlabels_green = FALSE
  # }	
  
  # taxa_labels <- read.taxa.labels(vector_version, tail)
  # 
  # character_labels <- read.char.labels(vector_version, charstatelabels)
  
  matrix_codes <- read.matrix.codes(vector_version)
  
  matrix_names <- read.matrix.names(vector_version)
  
  column_numbers <- 1:nchar
  
  
  if(taxlabels_green == TRUE | charstatelabels_green == TRUE){
    composed_matrix <- create.composed.matrix.adv(matrix_codes, matrix_names, taxa_labels, character_labels, column_numbers, taxlabels_green, charstatelabels_green)
    csv_version <- create.composed.matrix.adv(composed_matrix, taxa_labels, character_labels, taxlabels_green, charstatelabels_green)
  } else{
    csv_version <- create.composed.matrix(matrix_codes, matrix_names)
  }
  csv_version <- create.csv.from.nexus(composed_matrix)
  
  cat("The number of taxa/OTUs in this dataset is ", ntax) 
  
  cat("The number of characters in this dataset is", nchar)
  
}
```

  
