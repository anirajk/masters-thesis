source('common.R')

makeZeroCopyTputFigure <- function () {
  d <- tputload()
  head(d)
  d <- d[d$chunkSize %in% c(128,1024),]
  p <- myplot(d) +
    coord_cartesian(ylim=c(0, 8000), xlim=c(128, 16 * 1024))
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
        aggMBs=sum(transmittedBytes) / 2^20 / max(seconds),
        clients=length(server))
}


tputload <- function (filename='/Users/aniraj/development/thesis/working-copy/data/singledatapoints/cx3/pcieddioimc/201612172047-15-clients-128B-32chunks-r320-out.log') {
  d <- read.table(filename
                  , header=T)
  d$bytesPerMessage <- d$chunkSize * d$chunksPerMessage
  #d$chunkSize <- factor(d$chunkSize)
  d$copied <- factor(d$copied)
  d
}

membwload <- function (filename='/Users/aniraj/development/thesis/working-copy/data/singledatapoints/cx3/pcieddioimc/201612172047-15-clients-128B-32chunks-r320-membw.csv') {
  d <- read.csv(filename,header=T)
  d
}
makeMemoryBWFigure <- function(){
a <- membwload()
a["membw"]<-(a$iMC0.MEM_BW_TOTAL+a$iMC1.MEM_BW_TOTAL+a$iMC2.MEM_BW_TOTAL)*10
a["pciebw"]<-a$CBO.LLC_PCIE_MEM_TOTAL_BYTES*10
a["ddiobw"]<-a$CBO.LLC_DDIO_MEM_TOTAL_BYTES*10

p<- ggplot(a,aes(x=timestamp,y=membw,color="membw"))+
     geom_point()+
     geom_line()+
     geom_line(aes(y=pciebw,color="pciebw"))+
     geom_line(aes(y=ddiobw,color="ddiobw"))+
     geom_vline(xintercept = 47,colour="black", linetype = "longdash", show.legend = T)+
     geom_vline(xintercept = 87,colour="grey", linetype = "longdash", show.legend = T)+
     coord_cartesian(ylim=c(0, 8000))+
     ggtitle("Various metrics at Zero Copy,Unloaded and Copy Out\niMC.MEM_BW_TOTAL=membw\nCBO.LLC_DDIO_MEM_TOTAL_BYTES=ddiobw\nCBO.LLC_PCIE_MEM_TOTAL_BYTES=pciebw")
p
}