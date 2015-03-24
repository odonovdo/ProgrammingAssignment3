setwd("C:/Users/odonovad/datasciencecoursera/ProgrammingAssignment3")

rankall <- function(outcome,num= "best") {
      File.in <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      # remove dots from colunms     
      colnames(File.in) <- casefold(gsub("[.]"," ",colnames(File.in)))
      # create a expression to use for finding the outcome     
      pattern <- paste0("^[hH]ospital","(.*)","mortality","(.*)",outcome,"$")
      if(sum(grepl(pattern,colnames(File.in)))==0){
            stop("invalid outcome")  
      }else{
            if(!(grepl("[Bb]est",num)|grepl("[Ww]orst",num)|is.numeric(num))){
                  stop("invalid Choice")  
            }else{
                  Column.ID <- which(grepl(pattern,colnames(File.in),ignore.case = TRUE))
                  filter <- suppressWarnings(!is.na(as.numeric(File.in[,Column.ID])))
                  # remove non numeric values
                  File.values <- File.in[filter,]
                  File.values[,Column.ID] <- as.numeric(File.values[,Column.ID]) 
                  # assisgn "best" a numerical value of 1
                  if(grepl("[Bb]est",num))num <-1     
                  #identfy hosipital column
                  Hos.col <- which("hospital name"==colnames(File.values))
                  # loop over a list of hospitals by state and return rank
                  out.put <- lapply(split(File.values,File.values$state),function(x){
                        Hospitals <- as.data.frame(x)
                        if(grepl("[Ww]orst",num))num <- nrow(Hospitals)
                        x[order(x[,Column.ID],x[,Hos.col]),c(Hos.col,7)] [num,]
                  })
                  Names.states <- as.data.frame(do.call(rbind,out.put) )
                  colnames(Names.states) <-c("hospital","state")
                  Names.states
            }}}
