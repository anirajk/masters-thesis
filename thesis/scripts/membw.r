source('common.R')

tputfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_membw_profiles/*membw-out.log")
membwfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_membw_profiles/filtered-*membw.csv")
ddiobwfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_ddiobw_profiles/filtered-*ddiobw.csv")
pciebwfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_pciebw_profiles/filtered-*pciebw.csv")
deltatputfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_membw_profiles/deltas/*membw-out.log")
deltamembwfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_membw_profiles/deltas/filtered-*membw.csv")


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

makeZeroCopyTputFigure <- function (filename='',extratitle='') {
  d <- tputload(filename)
  d <- d[d$chunkSize %in% c(128,1024),]
  p <- myplot(d) +
    coord_cartesian(ylim=c(0, 8000), xlim=c(128, 16 * 1024))+
  ggtitle(paste(substr(file_path_sans_ext(basename(filename)),14,45),extratitle,sep="\n"))
  p
  #ggsave(plot=p, filename='~/development/thesis/thesis/figures/cx3_noperf/20161109180229-15clients/fig-zero-copy-tput.pdf',
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





aggregatedeltaClients <- function (d) {
  ddply(d, .(copied, chunksPerMessage, bytesPerMessage,
             chunkSize, deltasPerMessage, deltaSize, membw), summarise,
        #aggMBs=sum(transmissions *
        #(deltasPerMessage * deltaSize + chunksPerMessage * chunkSize))
        #/ 2^20 / max(seconds),
        #aggMBs=sum(transmissions * (as.double(deltasPerMessage * deltaSize) +
        #                            as.double(chunksPerMessage * chunkSize)))
        #           / 2^20 / max(seconds),
        aggMBs=sum(as.numeric(transmittedBytes)) / 2^20 / max(seconds),
        clients=length(server))
}


aggregateClients <- function (d) {
  ddply(d, .(copied, chunksPerMessage, bytesPerMessage,
             chunkSize, deltasPerMessage, deltaSize, membw,ddiobw,pciebw), summarise,
        #aggMBs=sum(transmissions *
        #(deltasPerMessage * deltaSize + chunksPerMessage * chunkSize))
        #/ 2^20 / max(seconds),
        #aggMBs=sum(transmissions * (as.double(deltasPerMessage * deltaSize) +
        #                            as.double(chunksPerMessage * chunkSize)))
        #           / 2^20 / max(seconds),
        aggMBs=sum(as.numeric(transmittedBytes)) / 2^20 / max(seconds),
        clients=length(server))
}


tputload <- function (filename='/Users/aniraj/development/thesis/thesis/data/singledatapoints/cx3/pcieddioimc/201612172047-15-clients-128B-32chunks-r320-out.log') {
  d <- read.table(filename
                  , header=T)
  d <- d[complete.cases(d),]
  d$bytesPerMessage <- d$chunkSize * d$chunksPerMessage
  d$copied <- factor(d$copied)
  d
}

perfload <- function (filename=pciebwfilenames[1]) {
  #Omit not counted values
  d <- read.csv(filename,header=T,
                 na.strings = c("#NC","#NC#NC","#NC#NC#NC","#NC#NC#NC#NC",
                                "#NC#NC#NC#NC#NC","#NC#NC#NC#NC#NC#NC",
                                "#NC#NC#NC#NC#NC#NC#NC","#NC#NC#NC#NC#NC#NC#NC#NC"))
  #print(paste("file:",filename))
  #print(paste("before removing last row:",nrow(d)))
  d=d[-nrow(d),]
  print(paste("before is na check:",nrow(d)))
  d[is.na(d)] <- 0
  print(paste("before complete cases:",nrow(d)))
  d <- d[complete.cases(d),]
  print(paste("before regex:",nrow(d)))
  if (colnames(d)[2]=="iMC0.MEM_BW_TOTAL"){
    d2 <- d[!is.na(as.numeric(as.character(d$iMC0.MEM_BW_TOTAL))),]
    d2 <- d[!is.na(as.numeric(as.character(d$iMC1.MEM_BW_TOTAL))),]
    d2 <- d[!is.na(as.numeric(as.character(d$iMC2.MEM_BW_TOTAL))),]
  }
  if (colnames(d)[2]=="CBO.LLC_DDIO_MEM_TOTAL_BYTES"){
    d2 <- d[!is.na(as.numeric(as.character(d$CBO.LLC_DDIO_MEM_TOTAL_BYTES))),]
  }
  if (colnames(d)[2]=="CBO.LLC_PCIE_MEM_TOTAL_BYTES"){
      d3 <- d[ grepl("(#NC)+",d$CBO.LLC_PCIE_MEM_TOTAL_BYTES,perl=T),]
      d2 <- d[ !grepl("(#NC)+",d$CBO.LLC_PCIE_MEM_TOTAL_BYTES,perl=T),]
      print(str(d3))
  }
  
  print(paste("after regex:",nrow(d2)))
  
  #print(paste(substr(file_path_sans_ext(basename(filename)),14,46)))
  d2
}
makeMemoryBWFigure <- function(filename='',extratitle=''){
if(is.null(filename)){
  return()
}
a <- perfload(filename)
a["membw"]<-(a$iMC0.MEM_BW_TOTAL+a$iMC1.MEM_BW_TOTAL+a$iMC2.MEM_BW_TOTAL)
#a["pciebw"]<-a$CBO.LLC_PCIE_MEM_TOTAL_BYTES*10
#a["ddiobw"]<-a$CBO.LLC_DDIO_MEM_TOTAL_BYTES*10
print(median(a$membw))
a<-data.frame(data.matrix(a))
titlestring<-paste(substr(file_path_sans_ext(basename(filename)),14,45),extratitle,paste("median",median(a$membw)), sep="\n")

p<- ggplot(a,aes(x=timestamp,y=membw,color="membw"))+
     geom_point()+
     geom_line()+
     #geom_line(aes(y=pciebw,color="pciebw"))+
     #geom_line(aes(y=ddiobw,color="ddiobw"))+
     #geom_line(aes(y=ddiobw,color="ddiobw"))+
     coord_cartesian(ylim=c(0, 30000))+
     #ggtitle("0 copy - no load - copyout\niMC.MEM_BW_TOTAL=membw\nCBO.LLC_DDIO_MEM_TOTAL_BYTES=ddiobw\nCBO.LLC_PCIE_MEM_TOTAL_BYTES=pciebw")+
     ggtitle(titlestring)
p
}
makeallmembwfigures<-function(){
for (membwfile in membwfilenames){
  tryCatch({
  print(paste("doing - ", substr(file_path_sans_ext(basename(membwfile)),14,45)))
  p<-makeMemoryBWFigure(membwfile,"warmup5s-run30s")
  p
  print(paste("done - ", substr(file_path_sans_ext(basename(membwfile)),14,45)))
  outputfilename<-paste("/Users/aniraj/development/thesis/thesis/figures/singledatapoints/membwandtput/randomised_run_60s/",basename(file_path_sans_ext(membwfile)),".pdf",sep="")
  #ggsave(outputfilename,width=12,height=8,units='in')
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
}

makealltputfigures<-function(){
  for (tputfile in tputfilenames){
    p<-makeZeroCopyTputFigure(tputfile)
    outputfilename<-paste("/Users/aniraj/development/thesis/thesis/figures/singledatapoints/membwandtput/randomised_30s_warmup5s/",basename(file_path_sans_ext(tputfile)),"-tput.pdf",sep="")
    ggsave(outputfilename,width=4,height=4,units='in')
  }
}

makeMergedFigure <- function (i=1,extratitle='') {
  for (i in seq(tputfilenames)){
  t <- tputload(filename=tputfilenames[i])
  t <- t[t$chunkSize %in% c(128,1024),]
  m <- perfload(filename=membwfilenames[i])
  d <- perfload(filename=ddiobwfilenames[i])
  p <- perfload(filename=pciebwfilenames[i])
  m["membw"]<-(m$iMC0.MEM_BW_TOTAL+m$iMC1.MEM_BW_TOTAL+m$iMC2.MEM_BW_TOTAL)*10
  p["pciebw"]<-p$CBO.LLC_PCIE_MEM_TOTAL_BYTES*10
  d["ddiobw"]<-d$CBO.LLC_DDIO_MEM_TOTAL_BYTES*10
  print('convert because of factor error')
  d$ddiobw <- as.numeric(d$ddiobw)
  p$pciebw <- as.numeric(p$pciebw)
  m$membw <- as.numeric(m$membw)
  membw<-rep(unname(quantile(m$membw,0.5,na.rm=TRUE)),nrow(t))
  pciebw<-rep(unname(quantile(p$pciebw,0.5,na.rm=TRUE)),nrow(t))
  ddiobw<-rep(unname(quantile(d$ddiobw,0.5,na.rm=TRUE)),nrow(t))
  print('profiles')
  print(paste(membw[1],pciebw[1],ddiobw[1]))
  curr<-data.frame(membw=membw,pciebw=pciebw,ddiobw=ddiobw,t)
  if(i==1){
    merged<-curr
  }else{
    merged<-rbind(merged,curr)
  }
  }
  print("merged")
  print(head(merged,2))
  tp <- mergeplot(merged)
  tp
  #ggsave(plot=p, filename='~/development/thesis/thesis/figures/cx3_noperf/20161109180229-15clients/fig-zero-copy-tput.pdf',
  #       width=5, height=2, units='in')
}

makeMergedDeltaFigure <- function (i=1,extratitle='') {
  for (i in seq(deltatputfilenames)){
    t <- tputload(filename=deltatputfilenames[i])
    t <- t[t$chunkSize %in% c(128,1024),]
    m <- perfload(filename=deltamembwfilenames[i])
    m["membw"]<-(m$iMC0.MEM_BW_TOTAL+m$iMC1.MEM_BW_TOTAL+m$iMC2.MEM_BW_TOTAL)
    membw<-rep(unname(quantile(m$membw,0.5,na.rm=TRUE)),nrow(t))
    curr<-data.frame(membw=membw,t)
    if(i==1){
      merged<-curr
    }else{
      merged<-rbind(merged,curr)
    }
  }
  print("merged")
  print(head(merged,2))
  tp <- mergedeltaplots(merged)
  tp
 
}


mergedeltaplots <- function (d, xlim=c(128, 16 * 1024)) {
  print("here")
  d <- aggregatedeltaClients(d)
  print("after aggclients")
  d$chunkSize <- factor(d$chunkSize)
  print(head(d,2))
  # NIC Tx tput plot
  p1 <- ggplot(d, aes(x=bytesPerMessage, y=aggMBs,
                      linetype=chunkSize, color=chunkSize,
                      shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='NIC Transfer Rate (MB/s)'
                       #,trans=log2_trans()
                       #,breaks=c(2000,3000,4000,8000,16000,32000)
    ) +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    myTheme+
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 12000))
  
  #Plot Memory B/W
  p2 <- ggplot(d, aes(x=bytesPerMessage, y=membw,
                      linetype=chunkSize, color=chunkSize,
                      shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='Memory B/w (MB/s)',
                       #trans=log2_trans(),
                       #breaks=c(2000,4000,6000,8000,10000,12000,14000,16000,18000,20000,22000,24000)
                       breaks=c(4000,8000,12000,16000,20000,24000)
    ) +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    #    geom_line(aes(x=bytesPerMessage,y=membw,linetype=chunkSize,color=membw,shape=copied))+
    myTheme+
    
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 24000))+
    geom_hline(yintercept=c(23950, 39321),color="black",linetype="longdash")+
    annotate("text", x=2000, y=39321, label="38.4 GB/s (Intel ARK spec Xeon E5-2450) ", size=2.5, color = "black",vjust=-1)+
    annotate("text", x=2000, y=23950, label="23.3 GB/s (Measured Peak Memory B/W) ", size=2.5, color = "black",vjust=-1)
  
  
    
    
    
    #d<-d[d$chunkSize==1024,]
    #d<-d[d$copied==1,]
    #print(summary(d))
    
    #ggsave("tput.pdf",p1,width=5,height=2,units='in')
    #ggsave("membw.pdf",p2,width=5,height=2,units='in')  
    #ggsave("ddiobw.pdf",p3,width=5,height=2,units='in')  
    #ggsave("pciebw.pdf",p4,width=5,height=2,units='in')  
  #ggsave("membw-ratio.pdf",p5,width=5,height=2,units='in')  
  #ggsave("~/development/thesis/thesis/figures/fig-tput.pdf",p1,width=5,height=2,units='in')
  #ggsave("~/development/thesis/thesis/figures/fig-membw.pdf",p2,width=5,height=6,units='in')  
  #ggsave("~/development/thesis/thesis/figures/fig-ddiobw.pdf",p3,width=5,height=2,units='in')  
  #ggsave("~/development/thesis/thesis/figures/fig-pciebw.pdf",p4,width=5,height=2,units='in')  
  #ggsave("~/development/thesis/thesis/figures/fig-membw-ratio.pdf",p5,width=5,height=2,units='in')  
  ggsave("~/development/thesis/thesis/figures/fig-ddiobw-percent.pdf",p6,width=5,height=2,units='in')  
  ggsave("~/development/thesis/thesis/figures/fig-pciebw-ratio.pdf",p7,width=5,height=2,units='in')  
  
  p7
  
}






ploty<-function(d,y){
  p <- ggplot(d, aes(x=bytesPerMessage, y=eval(parse(text=paste(y,sep=""))),
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
    #    geom_line(aes(x=bytesPerMessage,y=membw,linetype=chunkSize,color=membw,shape=copied))+
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 8000))
    p
  
}

mergeplot <- function (d, xlim=c(128, 16 * 1024)) {
  print("here")
  d <- aggregateClients(d)
  print("after aggclients")
  d$chunkSize <- factor(d$chunkSize)
  print(head(d,2))
  # NIC Tx tput plot
    p1 <- ggplot(d, aes(x=bytesPerMessage, y=aggMBs,
                     linetype=chunkSize, color=chunkSize,
                     shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='NIC Transfer Rate (MB/s)'
                       #,trans=log2_trans()
                       #,breaks=c(2000,3000,4000,8000,16000,32000)
                       ) +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    myTheme+
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 12000))
  #Plot Memory B/W
  p2 <- ggplot(d, aes(x=bytesPerMessage, y=membw,
                      linetype=chunkSize, color=chunkSize,
                      shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='Memory B/w (MB/s)',
                       #trans=log2_trans(),
                       #breaks=c(2000,4000,6000,8000,10000,12000,14000,16000,18000,20000,22000,24000)
                       breaks=c(4000,8000,12000,16000,20000,24000)
                       ) +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    #    geom_line(aes(x=bytesPerMessage,y=membw,linetype=chunkSize,color=membw,shape=copied))+
    myTheme+
    
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 24000))+
    geom_hline(yintercept=c(23950, 39321),color="black",linetype="longdash")+
    annotate("text", x=2000, y=39321, label="38.4 GB/s (Intel ARK spec Xeon E5-2450) ", size=2.5, color = "black",vjust=-1)+
    annotate("text", x=2000, y=23950, label="23.3 GB/s (Measured Peak Memory B/W) ", size=2.5, color = "black",vjust=-1)
  
  
  #Plot DDIO B/W
  p3 <- ggplot(d, aes(x=bytesPerMessage, y=ddiobw,
                      linetype=chunkSize, color=chunkSize,
                      shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='DDIO B/w (MB/s)'
                       #,trans=log2_trans(),
                       #breaks=c(1000,2000,4000,8000,16000,32000)
                       ) +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    #    geom_line(aes(x=bytesPerMessage,y=membw,linetype=chunkSize,color=membw,shape=copied))+
    myTheme+
    
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 12000))
  
  #Plot PCIe B/W
  p4 <- ggplot(d, aes(x=bytesPerMessage, y=pciebw,
                      linetype=chunkSize, color=chunkSize,
                      shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='PCIe BW (MB/s)'
                       #,trans=log2_trans(),
                       #breaks=c(51000,2000,4000,8000,16000,32000)
                       ) +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    #    geom_line(aes(x=bytesPerMessage,y=membw,linetype=chunkSize,color=membw,shape=copied))+
    myTheme+
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 12000))

  
  #Plot Memory B/W to Tx tput ration
  p5 <- ggplot(d, aes(x=bytesPerMessage, y=(membw/aggMBs),
                      linetype=chunkSize, color=chunkSize,
                      shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='Mem B/W to Transmit B/W ratio'
                       #,trans=log2_trans(),
                       #breaks=c(51000,2000,4000,8000,16000,32000)
    ) +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    #    geom_line(aes(x=bytesPerMessage,y=membw,linetype=chunkSize,color=membw,shape=copied))+
    myTheme+
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 3))
  
  
  #Plot DDIO misses as % of Memory consumed
  p6 <- ggplot(d, aes(x=bytesPerMessage, y=(ddiobw*100/membw),
                      linetype=chunkSize, color=chunkSize,
                      shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='DDIO contribution to Mem B/W (%)  ',
                       #,trans=log2_trans(),
                       breaks=c(20,40,60,80,100)
    ) +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    #    geom_line(aes(x=bytesPerMessage,y=membw,linetype=chunkSize,color=membw,shape=copied))+
    myTheme+
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 100))
  
  
  #Plot PCIeB/W to Agg MB/s
  p7 <- ggplot(d, aes(x=bytesPerMessage, y=(pciebw/aggMBs),
                      linetype=chunkSize, color=chunkSize,
                      shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
    scale_y_continuous(name='PCIe misses to Transmit Throughput' ,
                       #,trans=log2_trans(),
                       breaks=c(0,1,2,3)
    ) +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                       labels=c('Zero-Copy', 'Copy-Out'),
                       values=c(19,3)) +
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    #    geom_line(aes(x=bytesPerMessage,y=membw,linetype=chunkSize,color=membw,shape=copied))+
    myTheme+
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 3))+
    geom_vline(xintercept=2048,color="black",linetype="longdash")+
    annotate("text", x=2048, y=2.5, label="Throughput tipping point", size=2.5, color = "black",vjust=-1)
    
  
  
  #d<-d[d$chunkSize==1024,]
  #d<-d[d$copied==1,]
  #print(summary(d))
   
  #ggsave("tput.pdf",p1,width=5,height=2,units='in')
  #ggsave("membw.pdf",p2,width=5,height=2,units='in')  
  #ggsave("ddiobw.pdf",p3,width=5,height=2,units='in')  
  #ggsave("pciebw.pdf",p4,width=5,height=2,units='in')  
  #ggsave("membw-ratio.pdf",p5,width=5,height=2,units='in')  
  #ggsave("~/development/thesis/thesis/figures/tlocalseededrandom/tlocalseededrandom-fig-tput.pdf",p1,width=5,height=2,units='in')
  ggsave("/Users/aniraj/development/thesis/thesis/figures/fig-membw.pdf",p2,width=5,height=6,units='in')  
  ggsave("/Users/aniraj/development/thesis/thesis/figures/fig-ddiobw.pdf",p3,width=5,height=2,units='in')  
  ggsave("/Users/aniraj/development/thesis/thesis/figures/fig-pciebw.pdf",p4,width=5,height=2,units='in')  
  ggsave("/Users/aniraj/development/thesis/thesis/figures/fig-membw-ratio.pdf",p5,width=5,height=2,units='in')  
  ggsave("/Users/aniraj/development/thesis/thesis/figures/seededrandom_filtered_100ms/fig-ddiobw-percent.pdf",p6,width=5,height=2,units='in')  
  ggsave("/Users/aniraj/development/thesis/thesis/figures/fig-pciebw-ratio.pdf",p7,width=5,height=2,units='in')  
  
  p1

  }


