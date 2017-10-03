## ----echo=FALSE----------------------------------------------------------
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

## ---- echo=FALSE---------------------------------------------------------
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
  
  ggplot2::ggplot(data=y, ggplot2::aes(x=reorder(Party, Percent), y=Percent)) + 
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

## ----message=FALSE, warning=FALSE, fig.align='center', fig.height=4, fig.width=7----
 graph_election(municipality="Motala")

