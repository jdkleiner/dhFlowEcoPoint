library(ggplot2);

elf_skip <- function(save_directory,skipper){

  x <- c(0,1)
  y <- c(0,1)
  skipdata <- data.frame(x,y)

            plt <- ggplot(skipdata , aes(x=x,y=y)) + 
              #geom_point(data = skipdata,aes(colour="blue")) + 
            
  
             annotate("text", x=0.5, y=0.5, label= skipper,colour = "red",size =8)+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
          
            
            # END plotting function
            filename <- paste("279034-analysis-point-elf.png", sep="")
            ggsave(file=filename, path = save_directory, width=8, height=5)

            filename <- paste("279034-analysis-point-barplot.png", sep="")
            ggsave(file=filename, path = save_directory, width=8, height=5)

 	    filename <- paste("279034-analysis-point-elf-output.png", sep="")
            ggsave(file=filename, path = save_directory, width=8, height=5)

 	    filename <- paste("279034-analysis-point-barplot-output.png", sep="")
            ggsave(file=filename, path = save_directory, width=8, height=5)



} #close function
