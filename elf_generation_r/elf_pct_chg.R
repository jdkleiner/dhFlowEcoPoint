elf_pct_chg <- function(pct_inputs = list()){
  ruslope <- pct_inputs$ruslope
  ruint <- pct_inputs$ruint
  biometric_title <- pct_inputs$biometric_title 
  flow_title <- pct_inputs$flow_title
  Feature.Name <- pct_inputs$Feature.Name
  pct_chg <- pct_inputs$pct_chg
  sampres <- pct_inputs$sampres
  startdate <- pct_inputs$startdate
  enddate <- pct_inputs$enddate
  analysis_point <- pct_inputs$analysis_point
  save_directory <- pct_inputs$save_directory
  
  analysis_point_x <- analysis_point$analysis_point_x
  analysis_point_y <- analysis_point$analysis_point_y
  
  apt_pct <- (analysis_point_x -((pct_chg/100)*analysis_point_x))
  apt_sa <- (ruslope*log(analysis_point_x))+ruint
  apt_sb <- (ruslope*log(apt_pct))+ruint
  apt_chg <- (((apt_sa-apt_sb)/apt_sa)*100)
  apt <- data.frame(analysis_point_x,apt_chg)
  
  
  #pct_chg <- 10
  #ruslope <- 1.26
  #ruint <- 17.3
  
  #its <- (1:10)
  its <- seq(1, 500, 1)
  
  
  pct <- (its -((pct_chg/100)*its))
  #pct_5 <- (5 -((pct_chg/100)*5))
  #pct_10 <-(10 -((pct_chg/100)*10))
  #pct_25 <-(25 -((pct_chg/100)*25))
  #pct_50 <-(50 -((pct_chg/100)*50))
  #pct_100 <-(100 -((pct_chg/100)*100))
  #pct_200 <-(200 -((pct_chg/100)*200))
  #pct_500 <-(500 -((pct_chg/100)*500))
  
  sa <- (ruslope*log(its))+ruint
  #sa_5 <- (ruslope*log(5))+ruint
  #sa_10 <- (ruslope*log(10))+ruint
  #sa_25 <- (ruslope*log(25))+ruint
  #sa_50 <- (ruslope*log(50))+ruint
  #sa_100 <- (ruslope*log(100))+ruint
  #sa_200 <- (ruslope*log(200))+ruint
  #sa_500 <- (ruslope*log(500))+ruint
  
  sb <- (ruslope*log(pct))+ruint
  #sb_5 <- (ruslope*log(pct_5))+ruint
  #sb_10 <- (ruslope*log(pct_10))+ruint
  #sb_25 <- (ruslope*log(pct_25))+ruint
  #sb_50 <- (ruslope*log(pct_50))+ruint
  #sb_100 <- (ruslope*log(pct_100))+ruint
  #sb_200 <- (ruslope*log(pct_200))+ruint
  #sb_500 <- (ruslope*log(pct_500))+ruint
  
  pct_chgb <- (((sa-sb)/sa)*100)
  #pct_chg_5 <- (((sa_5-sb_5)/sa_5)*100)
  #pct_chg_10 <- (((sa_10-sb_10)/sa_10)*100)
  #pct_chg_25 <- (((sa_25-sb_25)/sa_25)*100)
  #pct_chg_50 <- (((sa_50-sb_50)/sa_50)*100)
  #pct_chg_100 <- (((sa_100-sb_100)/sa_100)*100)
  #pct_chg_200 <- (((sa_200-sb_200)/sa_200)*100)
  #pct_chg_500 <- (((sa_500-sb_500)/sa_500)*100)
  
  pct_chgs = c(pct_chgb)#_1,pct_chg_5,pct_chg_10,pct_chg_25,pct_chg_50,pct_chg_100,pct_chg_200,pct_chg_500)
  xvalues = c(its)
  #xvalues = c("1","5","10","25","50","100","200","500")
  #value = c("1","2","3","4","5","6","7","8")
  
  slope_table = data.frame(xvalues,pct_chgs)
  print(head(slope_table))
  
  title_projname <- sampres
  
  #Plot titles
  #ptitle <- paste("Change in ",biometric_title," at ",pct_chg,"% Flow Reduction","\n", Feature.Name," (",startdate," to ",enddate,")\n",title_projname," grouping",sep="")
  ptitle <- paste("Change in ",biometric_title," at ",pct_chg,"% Flow Reduction","\n", Feature.Name," ",title_projname," grouping",sep="")
  
  xaxis_title <- paste("\n",flow_title,sep="");
  yaxis_title <- paste("% Decrease in ", biometric_title,"\n", sep="");
  
  if(flow_title == "Drainage Area (km^2)"){
    yup_lim <- 6
  } else {
    yup_lim <- 4
  }
  
  #yup_lim <- 2
  
  loss <- (signif(analysis_point_y, digits = 3))-((signif(analysis_point_y, digits = 3))-((signif(analysis_point_y, digits = 3))*signif((apt_chg/100), digits = 3)))
  loss <- signif(loss, digits = 3)
  
  #plot_annotation2 <- paste("Analysis Point\n Taxa Reduction: ", signif(apt_chg, digits = 3),"%\nTaxa Loss: ",loss)
  plot_annotation2 <- paste("Taxa Reduction: ", signif(apt_chg, digits = 3),"%\nTaxa Lost: ",loss)
  
  
  
  #-OUTPUT------------------------------------------------------------------------------------------            
  output_bar_x <- c(0,1)
  output_bar_y <- c(0,1)
  output_frame_bar <- data.frame(output_bar_x,output_bar_y)
  
  output_bar <- ggplot(output_frame_bar , aes(x=output_bar_x,y=output_bar_y)) + 
    annotate("text", x=0.5, y=0.5, label= plot_annotation2,colour = "red",size =10)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  
  # END plotting function
  filename <- paste("279034-analysis-point-barplot-output.png", sep="")
  ggsave(file=filename, path = save_directory, width=8, height=5)
  #-------------------------------------------------------------------------------------------
  
  
  
  
  
  
  #plt2 <- ggplot(slope_table, aes(x=reorder(xvalues, -pct_chgs),y=pct_chgs)) + geom_bar(stat = "identity")+
  plt2 <- ggplot(slope_table, aes(x=xvalues, y=pct_chgs)) + 
    geom_line()+
    geom_point(data = apt, aes(x=analysis_point_x,apt_chg,color = "red"),size=5,shape=17) + 
    
    scale_color_manual(
      "Legend",
      values=c("red"),
      labels=c("Analysis Point")
    ) + 
    
    #annotate("text", x=150, y=1.5, label= plot_annotation2,colour = "red",size =8)+
    
    
    #ylim(0,yup_lim)+
    ggtitle(ptitle)+
    labs(x=xaxis_title,y=yaxis_title)+
    theme(axis.text.x = element_text(colour="grey20",size=20,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=15,hjust=.5,vjust=.5,face="plain"))
}
