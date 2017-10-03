#' @title Obtain election files
#' @description A function to obtain election files from the Swedish election 2014
#' @param x Number specifying which file to get where x is between 1 and 18 and of length one
#' @return A list containing the file name and a data frame of the file


get_file<- function(x){
  if(!(x %in% 1:18 && length(x)==1)) stop("argument x is invalid")

  links<-XML::getHTMLLinks("http://www.val.se/val/val2014/statistik/index.html")
  files<-links[stringr::str_detect(links,".skv")]
  
  if(x %in% 14:18){
    base<-"http://www.val.se"
  } else if(x %in% 11:13){
    base <- ""
  } else{
    base<-"http://www.val.se/val/val2014/statistik/"
  }
  path<-files[x]
  file<-paste(base, path, sep="")
  
  result<-list(file=file, table=utils::read.csv2(file, stringsAsFactors=FALSE, fileEncoding = "UTF-8"))
  
  char_col <- result[[2]][, sapply(result[[2]], class) == 'character']
  char_col <- apply(X = char_col, MARGIN = 2, FUN = iconv, from = "latin1", to = "ASCII//TRANSLIT")
  char_col <- data.frame(apply(X = char_col, MARGIN = 2, FUN = iconv, from = "ASCII//TRANSLIT", to = "UTF-8"), stringsAsFactors = FALSE)
  result[[2]][, sapply(result[[2]], class) == 'character'] <- char_col
  colnames(result[[2]]) <- iconv(colnames(result[[2]]), "latin1", "ASCII//TRANSLIT")
  colnames(result[[2]]) <- iconv(colnames(result[[2]]), "ASCII//TRANSLIT", "UTF-8")
  
  #colnames(result[[2]])<-stringr::str_replace_all(colnames(result[[2]]), pattern=".f6.", replacement="o")
  #colnames(result[[2]])<-stringr::str_replace_all(colnames(result[[2]]), pattern=".e5.", replacement="a")
  #colnames(result[[2]])<-stringr::str_replace_all(colnames(result[[2]]), pattern=".e4.", replacement="a")
  
  return(result)
}

