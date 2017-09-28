#' @title Voters 2014
#' @description A function to obtain information about the voters from the Swedish election 2014
#' @param election Type of election. Must be one of the following: "Riksdagsval", "Landstingsval" or "Kommunval"
#' @return A list containing the name of the file and a data frame containing information about the voters from the election
#' @export voters

voters<-function(election){
  if(length(election)!=1) stop("election argument must be of length 1")
  if(!(election %in% c("Riksdagsval", "Landstingsval", "Kommunval"))) stop("election argument is invalid")
  
  if(election=="Riksdagsval"){
    x<-16
  } else if (type=="Landstingsval"){
    x<-17
  } else {
    x<-18
  }
  get_file(x)
}

