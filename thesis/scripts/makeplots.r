# R to plot all figures for the paper.
# Sourcing this file and running makeAllFigures() will reproduce the
# plots from the paper from the same data used in the paper.
# By default, it collects data from logs/latest. The paper
# repo contains a directory at that path that holds the data
# from ibv-bench for the paper figs.
#
# ibv-bench generates timestamped log directories in a 'logs' directory,
# and it links the latest log directory to 'latest'. This script is
# designed to match that. Doing a
# $ rsync -ave ssh experiment-host:~/ibv-bench/logs/ logs/
# pulls all of the logs over in a format such that this script should
# automatically pick up the last run.

source('common.R')

tputfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_membw_profiles/*membw-out.log")
membwfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_membw_profiles/filtered-*membw.csv")
ddiobwfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_ddiobw_profiles/filtered-*ddiobw.csv")
pciebwfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_pciebw_profiles/filtered-*pciebw.csv")
deltatputfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_deltas_membw_profiles/*membw-out.log")
deltamembwfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_deltas_membw_profiles/filtered-*membw.csv")
extendedtputfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_extended_copyout_points/*membw-out.log")
extendedmembwfilenames <- Sys.glob("/Users/aniraj/development/thesis/thesis/data/seededrandom_filtered_100ms_extended_copyout_points/filtered-*membw.csv")



loadRDMAModes <- function(file="/Users/aniraj/development/thesis/thesis/data/rdmammodes/1000B_all_modes.csv"){
  d <- read.csv(file, header=T)
  d
}

plotRDMAModes <- function(size=""){
  outputfilename = paste("~/development/thesis/thesis/figures/fig-",size,"-RDMAverbs.pdf",sep="")
  inputfilename = paste("/Users/aniraj/development/thesis/thesis/data/rdmammodes/",size,"_all_modes.csv",sep="")
  d <- loadRDMAModes(inputfilename)
  d$mode <- factor(d$mode)
  d["bytesperop"]<- d$sglength * 200
  p <- ggplot(d, aes(x=bytesperop, y=txrate,
                     color=mode)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Op',
                       trans=log2_trans(),
                       #breaks=c(1000, 2000, 4000, 8000, 16000, 32000))+
                       breaks=c(200, 400, 800, 1600, 3200, 6400))+
    scale_y_continuous(name='Transfer Rate (MB/s)',
                       breaks=c(0,2000,4000,6000,8000)) +
    
    #scale_shape_manual(name='RDMA Verb',
    #                   labels=c('SEND', 'WRITE', 'READ'),
    #                   values=c(19,3,4)) +
    scale_color_manual(name='RDMA Verbs',
                       labels=c('SEND','WRITE','READ'),
                       values=brewer.pal(6, 'Set1')) +
    
    myTheme+
    coord_cartesian(xlim=c(200,6400),ylim=c(0,8000))+
    geom_hline(yintercept=c(6051, 7*1024),color="black",linetype="longdash")+
    annotate("text", x=4000, y=7168, label="7168 MB/s (Theoretical line rate) ", size=2.5, color = "black",vjust=-1)+
    annotate("text", x=4000, y=6051, label="6051 MB/s (Measured peak B/W) ", size=2.5, color = "black",vjust=-1)
  
  p
   ggsave(plot=p, filename=outputfilename,
         width=5, height=2, units='in')
   p
  
}

loadMerged <- function (i=1,extratitle='') {
  for (i in seq(tputfilenames)){
    t <- tputload(filename=tputfilenames[i])
    t <- t[t$chunkSize %in% c(128,1024),]
    m <- perfload(filename=membwfilenames[i])
    d <- perfload(filename=ddiobwfilenames[i])
    p <- perfload(filename=pciebwfilenames[i])
    m["membw"]<-(m$iMC0.MEM_BW_TOTAL+m$iMC1.MEM_BW_TOTAL+m$iMC2.MEM_BW_TOTAL)
    p["pciebw"]<-p$CBO.LLC_PCIE_MEM_TOTAL_BYTES
    d["ddiobw"]<-d$CBO.LLC_DDIO_MEM_TOTAL_BYTES
    print(str(p))
    print(summary(p))
    print(ddiobwfilenames[i])
    print(summary(d))
    print('convert')
    d$ddiobw <- as.numeric(d$ddiobw)
    p$pciebw <- as.numeric(p$pciebw)
    m$membw <- as.numeric(m$membw)
    print(summary(d))
    membw<-rep(unname(quantile(m$membw,0.5,na.rm=TRUE)),nrow(t))
    pciebw<-rep(unname(quantile(p$pciebw,0.5,na.rm=TRUE)),nrow(t))
    ddiobw<-rep(unname(quantile(d$ddiobw,0.5,na.rm=TRUE)),nrow(t))
    curr<-data.frame(membw=membw,pciebw=pciebw,ddiobw=ddiobw,t)
    if(i==1){
      merged<-curr
    }else{
      merged<-rbind(merged,curr)
    }
  }
  merged
}

loadMergedDeltas <- function (i=1,extratitle='') {
  for (i in seq(deltatputfilenames)){
    t <- tputload(filename=deltatputfilenames[i])
    #t <- t[t$chunkSize %in% c(128,1024),]
    m <- perfload(filename=deltamembwfilenames[i])
    m["membw"]<-(m$iMC0.MEM_BW_TOTAL+m$iMC1.MEM_BW_TOTAL+m$iMC2.MEM_BW_TOTAL)*10
    membw<-rep(unname(quantile(m$membw,0.5,na.rm=TRUE)),nrow(t))
    curr<-data.frame(membw=membw,t)
    if(i==1){
      merged<-curr
    }else{
      merged<-rbind(merged,curr)
    }
  }
  merged
}



load <- function (filename='/Users/aniraj/development/thesis/thesis/data/cx3_noperf/cx3_after_15clients_tlocal_extended_data_20161118.log') {
  d <- read.table(filename
                  , header=T)
  d$bytesPerMessage <- d$chunkSize * d$chunksPerMessage
  #d$chunkSize <- factor(d$chunkSize)
  d$copied <- factor(d$copied)
  d
}

plot <- function (d, xlim=c(2 * 1024, 64 * 1024)) {
  d <- aggregateClients(d)
  #d <- d[d$copied==0,]
  #d <- d[d$chunkSize==1024,]
  d$chunkSize <- factor(d$chunkSize)
  print(d)
  
  p <- ggplot(d, aes(x=bytesPerMessage, y=aggMBs,
                     linetype=chunkSize, color=chunkSize,
                     shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384)) +
#    scale_x_continuous(name='Bytes per Send',
#                       breaks=seq(0, 1024 * 1024, by=2 * 1024)) +
    scale_y_continuous(name='Transfer Rate (MB/s)') +
    scale_linetype_discrete(name='Record Size (B)') +
    scale_shape_manual(name='Transmit Mode',
                         labels=c('Zero-Copy', 'Copy-Out'),
                         values=c(19,3)) +
    #scale_color_manual(name='Record Size (B)',
    #                   values=c("steelblue","red")) +
    
    scale_color_manual(name='Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    coord_cartesian(xlim=xlim,
                    ylim=c(0, 6000)) +
    
    myTheme
    #bigFonts
  p
}

plotBreakdown <- function (d) {
  
  d$chunkSize <- factor(d$chunkSize)
  d$sendSecs <- d$sendNSecs / 1e9
  d$memcpySecs <- d$memcpyNSecs / 1e9
  d$addingGESecs <- d$addingGENSecs / 1e9
  d$setupWRSecs <- d$setupWRNSecs / 1e9
  d$miscSecs <- d$miscCycles / 1e9
  d$getTxSecs <- d$getTxNSecs / 1e9
  
  d <- d[,c('copied',
            'chunkSize',
            'chunksPerMessage',
            'bytesPerMessage',
            'sendSecs',
            'setupWRSecs',
            'memcpySecs',
            'addingGESecs',
            'getTxSecs',
            'miscSecs',
            'seconds')]
  
  d <- ddply(d, .(copied, chunkSize, chunksPerMessage, bytesPerMessage), summarise,
             postSend=sum(sendSecs) / sum(seconds),
             #setupWR=sum(setupWRSecs) / sum(seconds),
             memcpy=sum(memcpySecs) / sum(seconds),
             appendGE=sum(addingGESecs) / sum(seconds)) #
             #getTxBuffer=sum(getTxSecs) / sum(seconds)) #,
             #misc=sum(miscSecs) / sum(seconds))
  print(summary(d))
  
  m <- melt(d, .(copied, chunkSize, chunksPerMessage, bytesPerMessage))
  copied_labels <- c('0'='Zero-Copy',
                     '1'='Copy-Out')
  chunkSize_labels <- c('1'='1 B Records',
                        '32'='32 B Records',
                        '64'='64 B Records',
                        '128'='128 B Records',
                        '256'='256 B Records',
                        '512'='512 B Records',
                        '1024'='1024 B Records')
  variable_labels <- c(
    'appendGE'='Create Gather Entries',
    'memcpy'='Copy into Tx Buffer (memcpy)',
    'postSend'='Post Tx to NIC'
                       #'setupWR'='Create Basic Descriptor',
    )
                       #'getTxBuffer'='Waiting for NIC Buffers')
  p <- ggplot(m, aes(x=bytesPerMessage, y=value,
                     color=variable, fill=variable)) +
    facet_wrap(chunkSize~copied,
               #copied~chunkSize,
               scales='free_x',
               labeller=labeller(
                 copied=copied_labels,
                 chunkSize=chunkSize_labels
               ),
               nrow=2) +
    geom_area(position='stack') +
    scale_x_continuous(name='Bytes per Send') +
    scale_y_continuous(name='Fraction of Total CPU Time') +
    scale_color_manual(values=brewer.pal(6, 'Set1'),
                       labels=variable_labels,
                       name='CPU Time') +
    scale_fill_manual(values=brewer.pal(6, 'Pastel1'),
                      labels=variable_labels,
                      name='CPU Time') +
    coord_cartesian(ylim=c(0, 1.00), expand=F) +
    myTheme +
    guides(fill=guide_legend(reverse=T),
           color=guide_legend(reverse=T))
  p
}

plotCyclesPerRecord <- function(d) {
  d$chunkSize <- factor(d$chunkSize)
  d$sendSecs <- d$sendNSecs / 1e9
  d$memcpySecs <- d$memcpyNSecs / 1e9
  d$addingGESecs <- d$addingGENSecs / 1e9
  d$setupWRSecs <- d$setupWRNSecs / 1e9
  d$miscSecs <- d$miscCycles / 1e9
  d$getTxSecs <- d$getTxNSecs / 1e9
  
  #print(head(d[d$chunksPerMessage == 32,]))
  d <- d[,c('copied',
            'chunkSize',
            'chunksPerMessage',
            'bytesPerMessage',
            'chunksTx',
            'sendSecs',
            'setupWRSecs',
            'memcpySecs',
            'addingGESecs',
            'getTxSecs',
            'miscSecs',
            'seconds')]
  
  d <- ddply(d, .(copied, chunkSize, chunksPerMessage, bytesPerMessage), summarise,
             postSend=sum(sendSecs) / sum(seconds),
             setupWR=sum(setupWRSecs) / sum(seconds),
             memcpy=sum(memcpySecs) / sum(seconds),
             appendGE=sum(addingGESecs) / sum(seconds),
             getTxBuffer=sum(getTxSecs) / sum(seconds),
             chunksPerSecond=sum(as.numeric(chunksTx)) / mean(seconds),
             recordsPerSecond=sum(as.numeric(chunksTx)) / mean(seconds))
#misc=sum(miscSecs) / sum(seconds))
  
  d$bytesPerSecond <- as.double(as.character(d$chunkSize)) * d$chunksPerSecond
  # 0 to 1 sum indicates ratio of 16 cores used, so multiply
  # by cycles/per * cores to get cycles.
  d$busyCyclesPerSecond <- (d$postSend + d$setupWR + d$memcpy + d$appendGE) * (2.1e9 * 16)
  #print(head(d$chunksPerSecond))
  #print(head(d$bytesPerSecond))
  #print(head(d$busyCyclesPerSecond) / (2.1e9 * 16))
  
  copied_labels <- c('0'='Zero-Copy',
                     '1'='Copy-Out')
  chunkSize_labels <- c('1'='1 B Records',
                        '32'='32 B Records',
                        '64'='64 B Records',
                        '128'='128 B Records',
                        '256'='256 B Records',
                        '512'='512 B Records',
                        '1024'='1024 B Records')
  variable_labels <- c('postSend'='Post Tx to NIC',
                       'setupWR'='Create Basic Descriptor',
                       'memcpy'='Copy into Tx Buffer',
                       'appendGE'='Create Gather Entries',
                       'getTxBuffer'='Waiting for NIC Buffers')
  
  p <- ggplot(d, aes(x=bytesPerMessage,
                     y=busyCyclesPerSecond/bytesPerSecond,
                     color=chunkSize,
                     linetype=chunkSize,
                     shape=copied)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384, 32768)) +
    scale_y_log10(name='CPU Cycles Per Transmitted Byte',
                  breaks=c(0.01, 0.1, 1, 10)) +
    scale_color_manual(values=brewer.pal(6, 'Set1'),
                       labels=chunkSize_labels,
                       name='Record Size') +
    scale_linetype_discrete(labels=chunkSize_labels,
                       name='Record Size') +
    scale_shape_manual(name='Transmit Mode',
                         labels=copied_labels,
                         values=c(3, 4)) +
    coord_cartesian(ylim=c(.01, 20), expand=T) +
    annotation_logticks(sides='l') +
    myTheme
  p
}

plotDeltas <- function (d) {
  d <- aggregateClients(d)
  d$chunkSize <- factor(d$chunkSize)
  d$deltaSize <- factor(d$deltaSize)
  print(head(d))
  p <- ggplot(d, aes(x=deltasPerMessage,
                     y=aggMBs,
                     linetype=deltaSize,
                     color=deltaSize)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Delta Records per 16 KB Base Page') +
    scale_y_continuous(name='Transfer Rate (MB/s)') +
    scale_linetype_discrete(name='Delta Record Size (B)') +
    scale_color_manual(name='Delta Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    coord_cartesian(xlim=c(0, 32),
                    ylim=c(0, 8000)) +
    myTheme+
    geom_hline(yintercept=c(6051, 7*1024),color="black",linetype="dotted")+
    annotate("text", x=10, y=7168, label="7168 MB/s (Theoretical line rate) ", size=2.5, color = "black",vjust=-1)+
    annotate("text", x=10, y=6051, label="6051 MB/s (Measured peak B/W) ", size=2.5, color = "black",vjust=-1)
  
  p
}

plotDeltasMemBW <- function (d) {
  d <- aggregateClientsmembw(d)
  print(summary(d))

  d$chunkSize <- factor(d$chunkSize)
  d$deltaSize <- factor(d$deltaSize)
  p <- ggplot(d, aes(x=deltasPerMessage,
                     y=membw,
                     linetype=deltaSize,
                     color=deltaSize)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Delta Records per 16 KB Base Page') +
    scale_y_continuous(name='Memory Bandwidth (MB/s)') +
    scale_linetype_discrete(name='Delta Record Size (B)') +
    scale_color_manual(name='Delta Record Size (B)',
                       values=brewer.pal(6, 'Set1')) +
    coord_cartesian(xlim=c(0,32),
                    ylim=c(0, 24000))+
    geom_hline(yintercept=c(23950, 39321),color="black",linetype="longdash")+
    annotate("text", x=10, y=20000, label="23.3 GB/s (Measured Peak Memory B/W) ", size=2.5, color = "black",vjust=-1)+
    myTheme
  
  p
}

save <- function (p) {
  #ggsave('plot.pdf', width=6, height=4, units='in')
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
aggregateClientsmembw <- function (d) {
  ddply(d, .(copied, chunksPerMessage, bytesPerMessage,
             chunkSize, deltasPerMessage, deltaSize,membw), summarise,
        #aggMBs=sum(transmissions *
        #(deltasPerMessage * deltaSize + chunksPerMessage * chunkSize))
        #/ 2^20 / max(seconds),
        #aggMBs=sum(transmissions * (as.double(deltasPerMessage * deltaSize) +
        #                            as.double(chunksPerMessage * chunkSize)))
        #           / 2^20 / max(seconds),
        aggMBs=sum(transmittedBytes) / 2^20 / max(seconds),
        clients=length(server))
}



makeZeroCopyTputFigure <- function () {
  d <- loadMerged()
  print(head(d))
  d <- d[d$chunkSize %in% c(128,1024),]
  p <- plot(d) +
    coord_cartesian(ylim=c(0, 8000), xlim=c(128, 16 * 1024))+
    geom_hline(yintercept=c(6051, 7*1024),color="black",linetype="longdash")+
    annotate("text", x=1024, y=7168, label="7168 MB/s (Theoretical line rate) ", size=2.5, color = "black",vjust=-1)+
    annotate("text", x=1024, y=6051, label="6051 MB/s (Measured peak B/W) ", size=2.5, color = "black",vjust=-1)
  
    p 
  ggsave(plot=p, filename='/Users/aniraj/development/thesis/thesis/figures/fig-zero-copy-tput.pdf',
          width=5, height=2, units='in')
  p
}

makeZeroCopyFiguresExtendeded <- function () {
  d <- loadExtended()
  print(head(d))
  #d <- d[d$chunkSize==128,]
  d <- d[d$chunkSize %in% c(128,1024),]
  p <- plot(d) +
    coord_cartesian(ylim=c(0, 8000), xlim=c(128, 32 * 1024))+
    geom_hline(yintercept=c(6051, 7*1024),color="black",linetype="longdash")+
    annotate("text", x=1024, y=7168, label="7168 MB/s (Theoretical line rate) ", size=2.5, color = "black",vjust=-1)+
    annotate("text", x=1024, y=6051, label="6051 MB/s (Measured peak B/W) ", size=2.5, color = "black",vjust=-1)
  
  p
  d <- aggregateClientsmembw(loadExtended())
  print("after aggclients")
  d$chunkSize <- factor(d$chunkSize)
  p2 <- ggplot(d, aes(x=bytesPerMessage, y=membw,
                      linetype=chunkSize, color=chunkSize,
                      shape=copied)) +
    geom_line() +
    geom_point(size=1) +
    scale_x_continuous(name='Bytes per Send',
                       trans=log2_trans(),
                       breaks=c(128, 1024, 2048, 4096, 8192, 16384,32768)) +
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
    coord_cartesian(ylim=c(0, 24000), xlim=c(128, 32 * 1024))+
    geom_hline(yintercept=c(23950, 39321),color="black",linetype="longdash")+
    annotate("text", x=2000, y=39321, label="38.4 GB/s (Intel ARK spec Xeon E5-2450) ", size=2.5, color = "black",vjust=-1)+
    annotate("text", x=2000, y=23950, label="23.3 GB/s (Measured Peak Memory B/W) ", size=2.5, color = "black",vjust=-1)
  
  p2
}


makeOverheadsFigure <- function () {
  d <- loadMerged()
  #d <- d[d$copied == 0,]
  #d <- d[d$chunkSize == 128,]
  
  d <- d[(d$chunkSize %in% c(128, 1024) & d$chunksPerMessage <= 64 ),]
  p <- plotBreakdown(d) +
    coord_cartesian(ylim=c(0, 0.3))
  p
    ggsave(plot=p, filename='~/development/thesis/thesis/figures/fig-overheads.pdf',
         width=5, height=5, units='in')
  p
}

makeCyclesFigure <- function () {
  d <- loadMerged()
  d <- d[d$chunkSize %in% c(128,1024),]
  p <- plotCyclesPerRecord(d)
  p
  ggsave(plot=p, filename='~/development/thesis/thesis/figures/fig-cycles.pdf',
         width=5, height=2, units='in')
  p
}

makeDeltasFigure <- function () {
  d <- loadMergedDeltas()
  d <- d[d$chunkSize == 16384,]
  p1 <- plotDeltas(d)
  p2 <- plotDeltasMemBW(d)
  ggsave(plot=p1, filename='~/development/thesis/thesis/figures/fig-deltas-tput.pdf',width=5, height=2, units='in')
  ggsave(plot=p2, filename='~/development/thesis/thesis/figures/fig-deltas-membw.pdf', width=5, height=2, units='in')
  
  p <- multiplot(p1,p2)
 p
   #ggsave(plot=p, filename='~/development/thesis/thesis/figures//cx3_noperf/fig-deltas.pdf',
  #       width=5, height=1.5, units='in')
}

makeAllFigures <- function () {
  makeZeroCopyTputFigure()
  makeOverheadsFigure()
  makeCyclesFigure()
  makeDeltasFigure()
}

d <- load()

bwPlot <- plot(d)
breakdownPlot <- plotBreakdown(d)

c <- aggregateClients(d)

s <- d
s$chunkSize <- factor(s$chunkSize)
s$sendSecs <- s$sendNSecs / 1e9
s$memcpySecs <- s$memcpyNSecs / 1e9
s$addingGESecs <- s$addingGENSecs / 1e9
s$setupWRSecs <- s$setupWRNSecs / 1e9
s$setupWRSecs <- s$setupWRNSecs / 1e9
s$miscSecs <- s$miscCycles / 1e9 # !!!
s$getTxSecs <- s$getTxNSecs / 1e9

s <- ddply(s, .(copied, chunkSize, chunksPerMessage, bytesPerMessage), summarise,
           postSend=sum(sendSecs) / sum(seconds),
           setupWR=sum(setupWRSecs) / sum(seconds),
           memcpy=sum(memcpySecs) / sum(seconds),
           appendGE=sum(addingGESecs) / sum(seconds),
           getTxBuffer=sum(getTxSecs) / sum(seconds))

# Little facts for the paper.

computePctTputImprovementForZeroCopy <- function (d) {
  d <- d[d$chunkSize %in% c(128, 1024),]
  d <- aggregateClients(d)
  nocpSmall <- d[d$copied == 0 & d$chunkSize == 128,]$aggMBs
  nocpBig <- d[d$copied == 0 & d$chunkSize == 1024,]$aggMBs
  nolen <- length(nocpSmall)
  cpSmall <- head(d[d$copied == 1 & d$chunkSize == 128,]$aggMBs, n=nolen)
  cpBig <- head(d[d$copied == 1 & d$chunkSize == 1024,]$aggMBs, n=nolen)
  print(max(cpSmall))
  print(max(nocpSmall))
  print(max(cpBig))
  print(max(nocpBig))
  
  print('128 B item pct improvement in tput for zero-copy')
  i <- (nocpSmall/cpSmall -1) * 100
  print(i)
  print(paste('Max', max(i)))
  
  print('1024 B item pct improvement in tput for zero-copy')
  i <- (nocpBig/cpBig - 1) * 100
  print(i)
  print(paste('Max', max(i)))
}

computeOverheads <- function (d) {
  d <- d[d$chunkSize %in% c(128, 1024),]
  #d <- aggregateClients(d)
  d$chunkSize <- factor(d$chunkSize)
  d$sendSecs <- d$sendNSecs / 1e9
  d$memcpySecs <- d$memcpyNSecs / 1e9
  d$addingGESecs <- d$addingGENSecs / 1e9
  d$setupWRSecs <- d$setupWRNSecs / 1e9
  d$miscSecs <- d$miscCycles / 1e9
  d$getTxSecs <- d$getTxNSecs / 1e9
  d <- ddply(d, .(copied, chunkSize, chunksPerMessage, bytesPerMessage), summarise,
             postSend=sum(sendSecs) / sum(seconds),
             setupWR=sum(setupWRSecs) / sum(seconds),
             memcpy=sum(memcpySecs) / sum(seconds),
             appendGE=sum(addingGESecs) / sum(seconds),
             getTxBuffer=sum(getTxSecs) / sum(seconds),
             bytesPerSecond=sum(as.double(chunkSize) * chunksTx) / sum(seconds),
             recordsPerSecond=sum(chunksTx) / sum(seconds))
  #misc=sum(miscSecs) / sum(seconds))
  
  # 0 to 1 sum indicates ratio of 16 cores used, so multiply
  # by cycles/per * cores to get cycles.
  d$busyFrac <- (d$postSend + d$memcpy + d$appendGE)
  d$totalcpu <- (d$busyFrac + d$setupWR + d$getTxBuffer)
  print(ddply(d, .(copied, chunkSize), summarise,
        minBusy=min(busyFrac),
        maxBusy=max(busyFrac)))
  print(ddply(d, .(copied, chunkSize), summarise,
              minMemcpy=min(memcpy),
              maxMemcpy=max(memcpy)))
  print(ddply(d, .(copied, chunkSize), summarise,
              minAppendGE=min(appendGE),
              maxAppendGE=max(appendGE)))
  print("Improvement over Copy Out(busy) (copyout/zerocopy)")
  nocopybusysmall <- d[d$chunkSize == 128 & d$copied == 0,]$busyFrac
  copybusysmall <- d[d$chunkSize == 128 & d$copied == 1,]$busyFrac
  print(paste("128B records", max(copybusysmall/nocopybusysmall)))
  nocopybusylarge <- d[d$chunkSize == 1024 & d$copied == 0,]$busyFrac
  copybusylarge <- d[d$chunkSize == 1024 & d$copied == 1,]$busyFrac
  print(paste("1024 B records", max(copybusylarge/nocopybusylarge)))
  print("Difference in absolute total (copyout-zerocopy)")
  
  #nocopytotalsmall <- d[d$chunkSize == 128 & d$copied == 0,]$totalcpu
  #copytotalsmall <- d[d$chunkSize == 128 & d$copied == 1,]$totalcpu
  #nocopytotallarge <- d[d$chunkSize == 1024 & d$copied == 0,]$totalcpu
  #copytotallarge <- d[d$chunkSize == 1024 & d$copied == 1,]$totalcpu
  print(paste("128B records", max(copybusysmall-nocopybusysmall)))
  print(paste("1024 B records", max(copybusylarge-nocopybusylarge)))
  
  }

computeSmallTputImprovementForCopyOut <- function (d) {
  d <- d[d$chunkSize %in% c(128, 1024),]
  d <- aggregateClients(d)
  d <- ddply(d, .(copied, chunkSize), summarise,
             maxMBs=max(aggMBs))
  print(d)
  
  smallCopied <- d[d$chunkSize == 128 & d$copied == 1,]$maxMBs
  smallNoCopied <- d[d$chunkSize == 128 & d$copied == 0,]$maxMBs

  print('Pct improvement in tput of best 128 B copy-out versus best zero-copy')
  print((smallCopied/smallNoCopied - 1) * 100)
}

computeBestCyclesPerRecordImprovement <- function (d) {
  #d$chunkSize <- factor(d$chunkSize)
  d$sendSecs <- d$sendNSecs / 1e9
  d$memcpySecs <- d$memcpyNSecs / 1e9
  d$addingGESecs <- d$addingGENSecs / 1e9
  d$setupWRSecs <- d$setupWRNSecs / 1e9
  d$miscSecs <- d$miscCycles / 1e9
  d$getTxSecs <- d$getTxNSecs / 1e9
  
  d <- ddply(d, .(copied, chunkSize, chunksPerMessage, bytesPerMessage), summarise,
             postSend=sum(sendSecs) / sum(seconds),
             setupWR=sum(setupWRSecs) / sum(seconds),
             memcpy=sum(memcpySecs) / sum(seconds),
             appendGE=sum(addingGESecs) / sum(seconds),
             getTxBuffer=sum(getTxSecs) / sum(seconds),
             chunksPerSecond=sum(as.double(chunksTx)) / mean(seconds),
             recordsPerSecond=sum(as.double(chunksTx)) / mean(seconds))
  #misc=sum(miscSecs) / sum(seconds))
  
  d$bytesPerSecond <- as.double(as.character(d$chunkSize)) * d$chunksPerSecond
  # 0 to 1 sum indicates ratio of 16 cores used, so multiply
  # by cycles/per * cores to get cycles.
  d$busyCyclesPerSecond <- (d$postSend + d$setupWR + d$memcpy + d$appendGE) * (2.1e9 * 16)
  
  bestSmallCp <- min(d[d$copied == 1 & d$chunkSize == 128,]$busyCyclesPerSecond)
  bestSmallNoCp <- min(d[d$copied == 0 & d$chunkSize == 128,]$busyCyclesPerSecond)
  bestBigCp <- min(d[d$copied == 1 & d$chunkSize == 1024,]$busyCyclesPerSecond)
  bestBigNoCp <- min(d[d$copied == 0 & d$chunkSize == 1024,]$busyCyclesPerSecond)
  
  print(paste(bestSmallCp, bestSmallNoCp, bestBigCp, bestBigNoCp))
  
  print('Pct overhead reduction of nocp versus cp for')
  print('128 B records')
  print((1 - bestSmallNoCp/bestSmallCp) * 100)
  
  print('1024 B records')
  print((1 - bestBigNoCp/bestBigCp) * 100)
}

loadExtended <- function (i=1,extratitle='') {
  for (i in seq(tputfilenames)){
    t <- tputload(filename=tputfilenames[i])
    t <- t[t$chunkSize %in% c(128,1024),]
    m <- perfload(filename=membwfilenames[i])
    m["membw"]<-(m$iMC0.MEM_BW_TOTAL+m$iMC1.MEM_BW_TOTAL+m$iMC2.MEM_BW_TOTAL)*10
    m$membw <- as.numeric(m$membw)
    print(summary(d))
    membw<-rep(unname(quantile(m$membw,0.5,na.rm=TRUE)),nrow(t))
    curr<-data.frame(membw=membw,t)
    if(i==1){
      merged<-curr
    }else{
      merged<-rbind(merged,curr)
    }
  }
  for (i in seq(extendedtputfilenames)){
    t <- tputload(filename=extendedtputfilenames[i])
    t <- t[t$chunkSize %in% c(128,1024),]
    m <- perfload(filename=extendedmembwfilenames[i])
    m["membw"]<-(m$iMC0.MEM_BW_TOTAL+m$iMC1.MEM_BW_TOTAL+m$iMC2.MEM_BW_TOTAL)*10
    m$membw <- as.numeric(m$membw)
    print(summary(d))
    membw<-rep(unname(quantile(m$membw,0.5,na.rm=TRUE)),nrow(t))
    curr<-data.frame(membw=membw,t)
    merged<-rbind(merged,curr)
  }
  
  merged
}

computeCPUOverheadOfDeltas <- function (d) {
  d <- d[d$chunkSize == 16384,]
  d$chunkSize <- factor(d$chunkSize)
  d$sendSecs <- d$sendNSecs / 1e9
  d$memcpySecs <- d$memcpyNSecs / 1e9
  d$addingGESecs <- d$addingGENSecs / 1e9
  d$setupWRSecs <- d$setupWRNSecs / 1e9
  d$miscSecs <- d$miscCycles / 1e9
  d$getTxSecs <- d$getTxNSecs / 1e9
  
  d <- ddply(d, .(copied, chunkSize, chunksPerMessage, deltaSize, deltasPerMessage), summarise,
             postSend=sum(sendSecs) / sum(seconds),
             setupWR=sum(setupWRSecs) / sum(seconds),
             memcpy=sum(memcpySecs) / sum(seconds),
             appendGE=sum(addingGESecs) / sum(seconds),
             getTxBuffer=sum(getTxSecs) / sum(seconds),
             bytesPerSecond=sum(as.double(chunkSize) * chunksTx) / mean(seconds),
             recordsPerSecond=sum(chunksTx) / mean(seconds))
  d$busyFrac = d$postSend + d$memcpy + d$appendGE
  d <- d[,c('copied', 'deltaSize', 'deltasPerMessage', 'busyFrac')]
  print(d)
  d
}