source('common.R')

perfload <- function (filename='') {
  #Omit not counted values
  d <- read.csv(filename,header=T,
                na.strings = c("#NC","#NC#NC","#NC#NC#NC","#NC#NC#NC#NC",
                               "#NC#NC#NC#NC#NC","#NC#NC#NC#NC#NC#NC",
                               "#NC#NC#NC#NC#NC#NC#NC","#NC#NC#NC#NC#NC#NC#NC#NC"))
  # Last row may not be complete, Do a bunch of other filtering to remove bogus values
  d=d[-nrow(d),]
  d[is.na(d)] <- 0
  d <- d[complete.cases(d),]
  if (colnames(d)[2]=="iMC0.MEM_BW_TOTAL"){
    d2 <- d[!is.na(as.numeric(as.character(d$iMC0.MEM_BW_TOTAL))),]
    d2 <- d[!is.na(as.numeric(as.character(d$iMC1.MEM_BW_TOTAL))),]
    d2 <- d[!is.na(as.numeric(as.character(d$iMC2.MEM_BW_TOTAL))),]
  }
  if (colnames(d)[2]=="CBO.LLC_DDIO_MEM_TOTAL_BYTES"){
    d2 <- d[!is.na(as.numeric(as.character(d$CBO.LLC_DDIO_MEM_TOTAL_BYTES))),]
  }
  if (colnames(d)[2]=="CBO.LLC_PCIE_MEM_TOTAL_BYTES"){
    d2 <- d[!is.na(as.numeric(as.character(d$CBO.LLC_PCIE_MEM_TOTAL_BYTES))),]
  }
  
  d2
}
plotMemBW <- function(filename='',extratitle=''){
  d<-perfload(filename=filename)
  samplefreq <- 1/(d[2,1] - d[1,1])
  d["membw"]<-(d$iMC0.MEM_BW_TOTAL+d$iMC1.MEM_BW_TOTAL+d$iMC2.MEM_BW_TOTAL+d$iMC3.MEM_BW_TOTAL)*samplefreq
  p<- ggplot(d,aes(x=timestamp,y=membw,color="membw"))+
    geom_point()+
    geom_line()+
    coord_cartesian(ylim=c(0, 10000),xlim=c(0,120))+
    ggtitle(extratitle)
    myTheme
  p
    
}
plotDDIOBW <- function(filename=''){
  d<-perfload(filename=filename)
  samplefreq <- 1/(d[2,1] - d[1,1])
  d["membw"]<-d$CBO.LLC_DDIO_MEM_TOTAL_BYTES*samplefreq
  p<- ggplot(d,aes(x=timestamp,y=membw,color="membw"))+
    geom_point()+
    geom_line()+
    coord_cartesian(ylim=c(0, 10000))+
    myTheme
  p
  
}

plotPCIEBW <- function(filename=''){
  d<-perfload(filename=filename)
  samplefreq <- 1/(d[2,1] - d[1,1])
  d["membw"]<-d$CBO.LLC_PCIE_MEM_TOTAL_BYTES*samplefreq
  p<- ggplot(d,aes(x=timestamp,y=membw,color="membw"))+
    geom_point()+
    geom_line()+
    coord_cartesian(ylim=c(0, 10000))+
    myTheme
  p
  
}

plotAllMemBWFigures <- function(dir='',title=''){
  filenames = Sys.glob(paste(dir,'/*-membw.csv',sep=""))
  print(filenames)
  plotlist = list()
  for (file in filenames){
    server<-(substr(basename(file),1,7))
    plotlist[[length(plotlist)+1]] <- plotMemBW(file,extratitle=paste(title, server))
  }
  pdf(paste('membw-',title,'.pdf',sep=''),onefile = T)
  for (i in seq(length(plotlist))){
    do.call('grid.arrange',plotlist[i])
  }
  dev.off()
  }
