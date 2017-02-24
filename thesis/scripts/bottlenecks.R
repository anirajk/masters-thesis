source('common.R')

fetchBottlenecks <- function() {
  system('scp rcmaster:~/src/ramcloud/migration-bottlenecks.data .')
}

# NOTE: throws samples into the global scope.
readBottlenecks <- function(tag=NULL) {
  fileName <- '/Users/aniraj/development/thesis/working-copy/data/rcmigration/migration-bottlenecks/migration-bottlenecks.data'
  bottlenecks = read.table(fileName, header=TRUE)
  print(summary(bottlenecks))
  bottlenecks
  
}

plotBottlenecks <- function () {
  breaks <- c('full',
              'no-replication',
              'skip-replay',
              'skip-tx',
              'skip-append')
  labels <- c('Full',
              'Skip Re-replication',
              'Skip Replay on Target',
              'Skip Tx to Target',
              'Skip Copy for Tx')
  d <- readBottlenecks()
  #d[cumsum(c(d$mbs, 0)) - cumsum(c(0, d$mbs)) == 0,]$mbs <- NA
  p <- ggplot(d,
         aes(x=seconds-1,
             y=mbs,
             color=mode,
             linetype=mode)) +
    (theme_tufte(ticks=T, base_size = 10) + theme(legend.position = c(1, 1),
                     legend.justification = c(1, 1),
                     legend.margin = margin(t=10, r=10),
                     legend.background = element_rect(fill='white',
                                                      color='white'))) +
    geom_line(size=0.35) +
    scale_x_continuous(name='Time Since Start of Migration (s)',
                       limits=c(0, 60)) +
    scale_y_continuous(name='Migration Rate (MB/s)',
                       breaks=seq(0, 1400, by=200),
                       limits=c(0, 1200)) +
    scale_color_brewer(palette='Set1',
                       name='Part of Migration',
                       breaks=breaks, labels=labels,
                       guide=guide_legend(ncol=2, title.position = 'top')) +
    scale_linetype_discrete(name='Part of Migration',
                          breaks=breaks, labels=labels) +
                          #values=c(1, 4, 5, '101', 6)) +
    coord_cartesian(expand = F)
  #ggsave(plot=p,
  #       filename='bottlenecks.pdf',
  #       width=4, height=2, units='in')
  p
}