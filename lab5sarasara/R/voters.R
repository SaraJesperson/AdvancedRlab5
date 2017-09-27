

voters<-function(election){
  if(!(election %in% c("riksdagsval", "landstingsval", "kommunval"))) stop("election argument is invalid")
  if(election=="riksdagsval"){
    x<-16
  } else if (type=="landstingsval"){
    x<-17
  } else {
    x<-18
  }
  get_file(x)
}

