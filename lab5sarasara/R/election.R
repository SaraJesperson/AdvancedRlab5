

#election riksdagsval landstingsval kommunval

#type kommun valdistrikt

election <- function(election, type){
  if(!(election %in% c("riksdagsval", "landstingsval", "kommunval"))) stop("election argument is invalid")
  if(!(type %in% c("kommun", "valdistrikt"))) stop("type argument is invalid")
  if(type=="kommun"){
    if(election=="riksdagsval"){
      x<-1
    } else if (type=="landstingsval"){
      x<-3
    } else {
      x<-5
    }
  } else {
    if(election=="riksdagsval"){
      x<-2
    } else if (type=="landstingsval"){
      x<-4
    } else {
      x<-6
    }
  }
  get_file(x)
}

