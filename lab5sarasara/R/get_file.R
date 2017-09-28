#' @title Obtain election files
#' @description A function to obtain election files from the Swedish election 2014
#' @param x Number specifying which file to get where x is between 1 and 18 and of length one
#' @return A list containing the file name and a data frame of the file

get_file<- function(x){
  if(!(x %in% 1:18 && length(x)==1)) stop("argument x is invalid")
  
  requireNamespace(stringr)
  requireNamespace(XML)
  
  links<-getHTMLLinks("http://www.val.se/val/val2014/statistik/index.html")
  files<-links[str_detect(links,".skv")]
  
  
  
  if(x %in% 14:18){
    base<-"http://www.val.se"
  } else if(x %in% 11:13){
    base <- ""
  } else{
    base<-"http://www.val.se/val/val2014/statistik/"
  }
  path<-files[x]
  file<-paste(base, path, sep="")
  
  result<-list(file=file, table=read.csv2(file, stringsAsFactors=FALSE))
  return(result)
}
