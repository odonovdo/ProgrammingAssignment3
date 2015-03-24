setwd("C:/Users/odonovad/datasciencecoursera/ProgrammingAssignment3")

rankhospital <- function(state, outcome,num= "best") {
      File.in <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      if(!state%in%unique(File.in$State)){
            stop("invalid state")
      }else{
            colnames(File.in) <- casefold(gsub("[.]"," ",colnames(File.in)))
            pattern <- paste0("^[hH]ospital","(.*)","mortality","(.*)",outcome,"$")
            if(sum(grepl(pattern,colnames(File.in)))==0){
                  stop("invalid outcome")  
            }else{
                  if(!(grepl("[Bb]est",num)|grepl("[Ww]orst",num)|is.numeric(num))){
                        stop("invalid Choice")  
                  }else{
                        Column.ID <- which(grepl(pattern,colnames(File.in),ignore.case = TRUE))
                        #subset on selected state
                        File.in <- File.in [ File.in$state==state,]
                        #identfy valid values
                        filter <- suppressWarnings(!is.na(as.numeric(File.in[,Column.ID])))
                        File.values <- File.in[filter,]
                        File.values[,Column.ID] <- as.numeric(File.values[,Column.ID]) 
                        if(grepl("[Bb]est",num))num <-1     
                        if(grepl("[Ww]orst",num))num <- nrow(File.values)
                        Hos.col <- which("hospital name"==colnames(File.values))
      # return values that meet requirements   
      File.values[order(File.values[,Column.ID],File.values[,Hos.col]),Hos.col][num]
                  }}}}

