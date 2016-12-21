source('common.R')

membwfilenames <- Sys.glob("/Users/aniraj/development/thesis/working-copy/data/singledatapoints/cx3/pcieddioimc/*.csv")
tputfilenames <- Sys.glob("/Users/aniraj/development/thesis/working-copy/data/singledatapoints/cx3/pcieddioimc/*out.log")

mergealltputfiles <- function(){
  fileNumbers <- seq(tputfilenames)
  mergedfilename <- paste(file_path_sans_ext(tputfilenames[1]),"-merged.out",sep = "")
  print(mergedfilename)
  for (filenumber in fileNumbers){
    if (filenumber == 1){
    d <- read.table(tputfilenames[filenumber],header=T)
    print(head(d,1))
    }
    else{
      d <- read.table(tputfilenames[filenumber],header=F)
      print(head(d,1))
    }
    write.table(d,mergedfilename,append=T,row.names = FALSE,
                col.names = FALSE,quote=FALSE)
  }
}

makeZeroCopyTputFigure <- function (filename='') {
  d <- tputload(filename)
  d <- d[d$chunkSize %in% c(128,1024),]
  p <- myplot(d) +
    coord_cartesian(ylim=c(0, 8000), xlim=c(128, 16 * 1024))+
  ggtitle(substr(file_path_sans_ext(basename(filename)),14,45))
  p
  #ggsave(plot=p, filename='~/development/thesis/working-copy/figures/cx3_noperf/20161109180229-15clients/fig-zero-copy-tput.pdf',
  #       width=5, height=2, units='in')
}

myplot <- function (d, xlim=c(2 * 1024, 64 * 1024)) {
  d <- aggregateClients(d)
  d$chunkSize <- factor(d$chunkSize)
  p <- ggplot(d, aes(x=bytesPerMessage, y=aggMBs,
                     linetype=chunkSize, color=chunkSize,
                     shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='Transfer Rate (MB/s)') +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 8000))
  p
}


aggregateClients <- function (d) {
  ddply(d, .(copied, chunksPerMessage, bytesPerMessage,
             chunkSize, deltasPerMessage, deltaSize), summarise,
        #aggMBs=sum(transmissions *
        #(deltasPerMessage * deltaSize + chunksPerMessage * chunkSize))
        #/ 2^20 / max(seconds),
        #aggMBs=sum(transmissions * (as.double(deltasPerMessage * deltaSize) +
        #                            as.double(chunksPerMessage * chunkSize)))
        #           / 2^20 / max(seconds),
        aggMBs=sum(as.numeric(transmittedBytes)) / 2^20 / max(seconds),
        clients=length(server))
}


tputload <- function (filename='/Users/aniraj/development/thesis/working-copy/data/singledatapoints/cx3/pcieddioimc/201612172047-15-clients-128B-32chunks-r320-out.log') {
  d <- read.table(filename
                  , header=T)
  d <- d[complete.cases(d),]
  d$bytesPerMessage <- d$chunkSize * d$chunksPerMessage
  d$copied <- factor(d$copied)
  d
}

membwload <- function (filename='/Users/aniraj/development/thesis/working-copy/data/singledatapoints/cx3/pcieddioimc/201612172047-15-clients-128B-32chunks-r320-membw.csv') {
  d <- read.csv(filename,header=T)
  d <- d[complete.cases(d),]
}
makeMemoryBWFigure <- function(filename='',extratitle=''){
if(is.null(filename)){
  return()
}
print(filename)
a <- membwload(filename)
print(head(a,1))
a["membw"]<-(a$iMC0.MEM_BW_TOTAL+a$iMC1.MEM_BW_TOTAL+a$iMC2.MEM_BW_TOTAL)*10
a["pciebw"]<-a$CBO.LLC_PCIE_MEM_TOTAL_BYTES*10
a["ddiobw"]<-a$CBO.LLC_DDIO_MEM_TOTAL_BYTES*10
titlestring<-paste(substr(file_path_sans_ext(basename(filename)),14,44),extratitle, sep="\n")

p<- ggplot(a,aes(x=timestamp,y=membw,color="membw"))+
     geom_point()+
     geom_line()+
     geom_line(aes(y=pciebw,color="pciebw"))+
     geom_line(aes(y=ddiobw,color="ddiobw"))+
     geom_line(aes(y=ddiobw,color="ddiobw"))+
     geom_line(aes(y=ddiobw,color="ddiobw"))+
     geom_line(aes(y=ddiobw,color="ddiobw"))+
     coord_cartesian(ylim=c(0, 8000))+
     #ggtitle("0 copy - no load - copyout\niMC.MEM_BW_TOTAL=membw\nCBO.LLC_DDIO_MEM_TOTAL_BYTES=ddiobw\nCBO.LLC_PCIE_MEM_TOTAL_BYTES=pciebw")+
     ggtitle(titlestring)
p
}
makeallmembwfigures<-function(){
for (membwfile in membwfilenames){
  tryCatch({
  print(paste("doing - ", substr(file_path_sans_ext(basename(membwfile)),14,45)))
  p<-makeMemoryBWFigure(membwfile)
  print(paste("done - ", substr(file_path_sans_ext(basename(membwfile)),14,45)))
  outputfilename<-paste("/Users/aniraj/development/thesis/working-copy/figures/singledatapoints/membwandtput/",basename(file_path_sans_ext(membwfile)),".pdf",sep="")
  ggsave(outputfilename,width=4,height=4,units='in')
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
}
makealltputfigures<-function(){
  for (tputfile in tputfilenames){
    p<-makeZeroCopyTputFigure(tputfile)
    outputfilename<-paste("/Users/aniraj/development/thesis/working-copy/figures/singledatapoints/membwandtput/",basename(file_path_sans_ext(tputfile)),"-tput.pdf",sep="")
    ggsave(outputfilename,width=4,height=4,units='in')
  }
}
