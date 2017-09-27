

get_file<- function(x){
  require(stringr)
  require(XML)
  
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
  
  result<-list(file=path, table=read.csv2(file, stringsAsFactors=FALSE))
  return(result)
}

