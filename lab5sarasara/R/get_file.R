#' @title Obtain election files
#' @description A function to obtain election files from the Swedish election 2014
#' @param x Specifying what election type to obtain with three allowed arguments ("Riksdagsval", "Landstingsval" or "Kommunval")
#' @return A list containing the election type and the election result on municipality level
#' @export get_file


get_file<- function(x){
  if(length(x)!=1) stop("can only choose one election at a time")
  if(!(x %in% c("Riksdagsval", "Landstingsval", "Kommunval"))) stop("x argument is invalid")
  
  y <- structure(c(1,2,3), names=c("Riksdagsval", "Landstingsval", "Kommunval"))
  z <- which(names(y)==x)
  
  links<-XML::getHTMLLinks("http://www.val.se/val/val2014/statistik/index.html")
  file<-links[stringr::str_detect(links,".xls")]
  file<- paste("http://www.val.se/val/val2014/statistik/", file[c(1,3,5)], sep="")
  
  if(z == 3){
    fileext <- ".xlsx"
  } else{
    fileext <- ".xls"
  }
  
  httr::GET(file[z], httr::write_disk(tf <- tempfile(fileext = fileext)))
  df <- readxl::read_excel(tf)
  result<-df[3:nrow(df),]
  colnames(result)<-df[2,]
  
  temp<-apply(result[,c(-3,-4)], MARGIN=2, as.numeric)

  result[,c(-3,-4)]<-temp
  
  result<-data.frame(result,stringsAsFactors = FALSE)

  result <- list(Election=x, table=result)
  
  return(result)
}

