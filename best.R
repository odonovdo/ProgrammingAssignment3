setwd("C:/Users/odonovad/datasciencecoursera/ProgrammingAssignment3")

best <- function(state, outcome) {
      File.in <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      if(!state%in%unique(File.in$State)){
            stop("invalid state")
      }else{
            colnames(File.in) <- casefold(gsub("[.]"," ",colnames(File.in)))
            pattern <- paste0("^[hH]ospital","(.*)","mortality","(.*)",outcome,"$")
            
            if(sum(grepl(pattern,colnames(File.in)))==0){
                  stop("invalid outcome")  
            }else{          
                  Column.ID <- which(grepl(pattern,colnames(File.in),ignore.case = TRUE))
                  File.in <- File.in [ File.in$state==state,]
                  filter <- suppressWarnings(!is.na(as.numeric(File.in[,Column.ID])))
                  File.values <- File.in[filter,]
                  File.values[,Column.ID] <- as.numeric(File.values[,Column.ID] )
      
      # return requirements
      as.character(head(File.values[order(File.values[,Column.ID]),"hospital name"],1))
                  
            }}}


