#' @title Election results 2014
#' @description A function to obtain election results from the Swedish election 2014
#' @param election Type of election. Must be one of the following: "Riksdagsval", "Landstingsval" or "Kommunval"
#' @param type Specifies which level the results should be presented in. Must be either "Kommun" or "Valdistrikt"
#' @return A list containing the name of the file and a data frame containing the election results depending 
#' on the arguments election and type
#' @export election

election <- function(election, type){
  if(!(election %in% c("Riksdagsval", "Landstingsval", "Kommunval"))) stop("election argument is invalid")
  if(!(type %in% c("Kommun", "Valdistrikt"))) stop("type argument is invalid")
  if(type=="Kommun"){
    if(election=="Riksdagsval"){
      x<-1
    } else if (type=="Landstingsval"){
      x<-3
    } else {
      x<-5
    }
  } else {
    if(election=="Riksdagsval"){
      x<-2
    } else if (type=="Landstingsval"){
      x<-4
    } else {
      x<-6
    }
  }
  get_file(x)
}
