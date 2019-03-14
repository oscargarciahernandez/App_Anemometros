library(ggplot2)
library(RColorBrewer)

plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 0.5,
                          dirres = 22.5,
                          spdmin = 0,
                          spdmax = 15,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          opacity=0.6,
                          border_color="white"){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  }
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  data<-data[,c("Date","lon","lat",spd,dir)]
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } 
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,n.colors.in.range),
                                                min(9,n.colors.in.range)),
                                            palette ))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  

  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    cat("ggplot2 version > V2.2")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar(width = 1,color=border_color, size=0.001, alpha=opacity) + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) + 
    theme(legend.position = "none",
          plot.background = element_rect(fill= "transparent", colour= NA),
          panel.background = element_rect(fill= "transparent", colour = NA),
          panel.grid.major = element_line(colour = "NA"), 
          axis.line = element_line(colour = NA),
          axis.text.y=element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.text.x = element_blank()) +
    xlab("")+ ylab("") +
    coord_polar(start = -((dirres/2)/360) * 2*pi) 
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}
