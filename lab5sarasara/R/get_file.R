#' @title Obtain election files
#' @description A function to obtain election files from the Swedish parliamentary election 2014
#' @return A data frame with the election result on municipality level


get_file<- function(){

  links<-XML::getHTMLLinks("http://www.val.se/val/val2014/statistik/index.html")
  file<-links[stringr::str_detect(links,".xls")]
  file<- paste("http://www.val.se/val/val2014/statistik/", file[1], sep="")
  
  httr::GET(file, httr::write_disk(tf <- tempfile(fileext = ".xls")))
  df <- readxl::read_excel(tf)
  result<-df[3:nrow(df),]
  colnames(result)<-df[2,]
  
  temp<-apply(result[,c(-3,-4)], MARGIN=2, as.numeric)

  result[,c(-3,-4)]<-temp
  
  result<-data.frame(result,stringsAsFactors = FALSE)
  
  return(result)
}

