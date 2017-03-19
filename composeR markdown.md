---
title: "composeR markdown version"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
This is a Markdown document meant to give an explanation of the composeR function.
Note: this is a marked-up, broken-down look at the function.  The file won't actually execute because it's spread between chunks.

composeR is a function designed to read Nexus data files and produce a csv file.  Nexus files are widely used in phylogenetics for both morphological and sequence data; papers generally include them in the supplementary information, either printed or as an attached file.  However, most of a Nexus file is text telling the reading program how to read the file, and thus not very useful in terms of exploring or manipulating datasets, especially as supertree analyses become more common.

The ape package already has a function for reading Nexus data files into R, read.nexus.data.  However, the function is unable to accomodate polymorphic characters or spaces within taxon names.  This limits its utility considerably.  Moreover, not much phylogenetic inference is done within R itself, so there isn't much existing structure (packages/functions) for reading in (Nexus) datasets otherwise.  

Thus composeR is designed to read a Nexus file and create a csv version for easier use.  The file requires no internal modifications, and only needs to be in .txt format (changing the file ending nex -> .txt will suffice, no other editing required.)

=========

composeR

==========

This is the start of the function.  The readr and stringr packages provide very useful functions for handling regular expressions, which the function uses heavily.  The file is first read is as a single enormous string using read_file, and then converted to a vector.  The vector version will be the only one used therafter, but using read_file is the only way to import the file conents, as .nex is not recognized (not being a widely-used filetype outside of phylogenetics.)
```{r}
composeR <- function(nexus){
  require(readr)
  require(stringr)
  
  file <- read_file(nexus) #file must be .txt
  
  vector_version <- as.vector(file)
```


COMPONENT FUNCTIONS
What follows are the functions that make up composeR.  They are:

-read.ntax
-read.nchar
-read.matrix.codes
-read.matrix.names
-create.composed.matrix
-create.csv.from.nexus

Put simply, read.ntax figures out the number of taxa and nchar figures out the number of characters; read.matrix.codes and read.matrix.names get the character state codings and taxon names, respectively, from the matrix; create.composed.matrix makes a matrix from the names and character state codings; create.csv.from.nexus takes the composed matrix and writes it to a csv file.

```{r}
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
  
```

read.matrix.codes and read.matrix.names are the core of the function; while many Nexus files have taxon and/or character labels, not all do, but every file will at the very least have a matrix.  One goal for composeR was making it able to handle polymorphic characters, which are coded with multiple symbols within curly brackets (eg. {0 1}).  "\\{\\w\\s\\w\\}|." is the regex eventually devised as the solution- it prioritizes matching the polymorphic character pattern as a unit, then matching single symbols (Many thanks to M Michalska-Smith for cracking this one).  

```{r}
 read.matrix.codes <- function(vector_version){
    
    vector_versionUP <- toupper(vector_version)
    matrix_match <- str_extract_all(vector_versionUP, regex("MATRIX\\n\\t(.*?);", dotall=TRUE, multiline=TRUE)) 
    matrix_match[[1]] <- gsub("MATRIX", "", matrix_match[[1]])
    matrix_match_split <- str_match_all(matrix_match, "(\\w+?)\\s+([\\w\\?\\{\\s\\}\\-]+?)[\\n;]") #\\w used to facilitate reading of both numbers (used with morphological data) and letters (used with sequence data)
    
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
```

And at the end the matrix is composed and then written to the csv file (which will be put in your working directory.)
```{r}
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
```

MAIN FUNCTION

This is the main body of the function, which essentially consists of function calls.  At the end, in addition to having written the csv version of the original nexus file, composeR will tell you how many taxa (ntax) and characters (nchar) are in the dataset.
```{r}
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
```
