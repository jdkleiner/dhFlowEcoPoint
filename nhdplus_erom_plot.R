library(ggplot2);

nhdplus_erom_plot <- function(site,save_directory,nhdplus_hydroid){
#nhdplus_hydroid <- 340325
#print(nhdplus_hydroid)
  
#attempt to plot bars of erom data
#RETRIEVE ALL NHDPLUS EROM DATA
nhdplus_erom <- paste(site,"point-analysis-flowmetric-value",nhdplus_hydroid,sep="/")

nhdplus_erom <- read.csv(nhdplus_erom, header = TRUE, sep = ",")
nhdplus_erom <- nhdplus_erom[order(nhdplus_erom$id),] #this reordering relies on the assumption that the id always increases with monthly flowmetric 
#nhdplus_flowmetric_value <- nhdplus_flowmetric_value$propvalue

months <- c("Mean","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
mo_count <- c(0,1,2,3,4,5,6,7,8,9,10,11,12)
nhdplus_erom <- cbind(nhdplus_erom,months,mo_count)
#nhdplus_erom$propvalue <- round(nhdplus_erom$propvalue, digits = 1) #round flow values 

annual_mean <- nhdplus_erom[which(nhdplus_erom$months == "Mean"),]
annual_mean <- annual_mean$propvalue

nhdplus_erom <- nhdplus_erom[-which(nhdplus_erom$months == "Mean"),]

#add annual mean value as line
cutoff <- data.frame( x = c(-Inf, Inf), y = annual_mean, cutoff = factor(annual_mean))


plot_title <- paste("EROM Monthly Mean Flow Data",sep="")
plt_erom <- ggplot(nhdplus_erom, aes(x = reorder(months, mo_count), y = propvalue,  width=0.5)) +
  geom_bar(stat = "identity",fill = "royalblue3") +
  geom_line(aes( x, y, linetype = cutoff), cutoff, color="firebrick1")+
  labs(linetype="Annual Mean\nFlow (cfs)")+
  geom_text(aes(label=comma(round(propvalue,digits=1))), vjust=-0.25) +      #adds values above bars
  ggtitle(plot_title)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("\nMonth\n") +
  ylab("Mean Flow (cfs)\n") +
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"))
# END plotting function

filename <- paste("279034-analysis-point-containing-nhdplus-erom.png", sep="")
ggsave(file=filename, path = save_directory, width=8, height=5)

}