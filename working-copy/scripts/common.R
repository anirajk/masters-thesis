list.of.packages <- c('ggplot2',
                      'grid',
                      'gridExtra',
                      'quantreg',
                      'reshape2',
                      'scales',
                      'extrafont',
                      'ggthemes',
                      'plyr',
                      'RColorBrewer')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages,
                                           repos="http://cran.rstudio.com/")

library(ggplot2)
library(grid)
library(gridExtra)
library(quantreg)
library(reshape2)
library(scales)
library(extrafont)
library(ggthemes)
library(plyr)
library(RColorBrewer)


myTheme <- theme_bw() +
  theme(panel.grid.major = element_line(color = 'darkgrey',
                                        linetype = 'dashed'),
        panel.grid.minor = element_line(color = 'lightgrey',
                                        linetype ='dotted'),
        #panel.border = element_blank(),
        #axis.ticks.length = unit(-0.2, 'cm'),
        text = element_text(size = rel(3.1)),
        legend.text = element_text(size=rel(2.4)),
        legend.title = element_text(face='plain', size=rel(2.4)),
        axis.text = element_text(size=rel(0.9)),
        axis.line = element_line(color = 'black'),
        axis.title.x = element_text(vjust = -0.3, size=rel(2.4)), 
        axis.title.y = element_text(vjust = 0.8, size=rel(2.4)),
        legend.background = element_blank(), 
        legend.key = element_blank(), 
        legend.margin = unit(0, 'cm'),
        #legend.title = element_text(face="plain"),
        #panel.background = element_blank(), 
        #panel.border = element_blank(),
        panel.grid = element_blank()
        #plot.background = element_blank(),
        #strip.background = element_blank()
  )

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
            To enable full font support, run: 
            install.packages('extrafont') 
            font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain")
      #panel.background = element_blank(), 
      #panel.border = element_blank(),
      #panel.grid = element_blank(),
      #plot.background = element_blank(),
      #strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
}

bigFonts <- myTheme +
  theme(text=element_text(size=rel(4)),
        legend.text=element_text(size=rel(3)),
        legend.title=element_text(size=rel(3)))
