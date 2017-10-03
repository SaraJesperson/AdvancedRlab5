#' @title Plot the election results
#' @description A function to plot the results from the Swedish election 2014 for a specific municipality in Sweden. 
#' Only the nine biggest parties in Sweden are plotted
#' @param municipality The name of the municipality to plot
#' @return A bar chart over the results in chosen municipality
#' @export graph_election

graph_election <- function(municipality){
  if(length(municipality)!=1) stop("argument must be of length 1")

  y <- get_file()

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
  
  ggplot2::ggplot(data=y, ggplot2::aes(x=stats::reorder(y$Party, y$Percent), y=y$Percent)) + 
    ggplot2::geom_bar(stat="identity", fill="darkslategray4") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::labs(title=paste0("Election results 2014 in municipality ", municipality),
         subtitle="Riksdagsval",
         x="") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5, size=16),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size=14, face="italic"),
          axis.text = ggplot2::element_text(size=12)
    )
}
