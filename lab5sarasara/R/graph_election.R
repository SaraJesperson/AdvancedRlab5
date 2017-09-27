#' @title Plot the election results
#' @description A function to plot the results from the Swedish election 2014 for a specific municipality in Sweden. 
#' Only the nine biggest parties in Sweden are plotted
#' @param municipality The name of the municipality to plot
#' @param election_type Type of election. Must be one of the following: "Riksdagsval", "Landstingsval" or "Kommunval"
#' @return A bar chart over the results in chosen municipality and the chosen election type
#' @export graph_election

graph_election <- function(municipality, election_type){
  if(!(election_type %in% c("Riksdagsval", "Landstingsval", "Kommunval"))) stop("election_type argument is invalid")
  
  require(ggplot2)

  y <- election(election=election_type, type="Kommun")$table

  if(!municipality %in% y$KOMMUN) stop("municipality argument is invalid")  
  
  y <- t(y[y$KOMMUN == municipality,
           colnames(y) %in% c("M.proc","C.proc","FP.proc","KD.proc","S.proc","V.proc","MP.proc","SD.proc","FI.proc")])
  
  y <- data.frame(Party=c("Moderaterna",
                          "Centerpartiet",
                          "Folkpartiet",
                          "Kristedemokraterna",
                          "Socialdemokraterna",
                          "Vansterpartiet",
                          "Miljopartiet",
                          "Sverigedemokraterna",
                          "Feministiskt Initiativ"),
                  Percent=y[,1])
  
  ggplot(data=y, aes(x=reorder(Party, Percent), y=Percent)) + 
    geom_bar(stat="identity", fill="darkslategray4") +
    coord_flip() +
    theme_bw() +
    labs(title=paste0("Election results 2014 in municipality ", municipality),
         subtitle=election_type,
         x="") +
    theme(plot.title = element_text(hjust=0.5, size=16),
          plot.subtitle = element_text(hjust = 0.5, size=14, face="italic"),
          axis.text = element_text(size=12)
    )
}
