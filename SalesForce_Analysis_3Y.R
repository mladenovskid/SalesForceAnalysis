install.packages()
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

NikeCases <- read_xls(path = 'C://Users//dmladenovski//Documents//R//Support Cases//NikeSalesForce3Y.xls', sheet = 1)
NikeCases <- NikeCases[!is.na(NikeCases$`Nike Case Number`),]
NikeCases <- NikeCases[!(NikeCases$`Nike Case Number` %in% c('(No Case # Provided)', '(SPS Submitted)')),]
NikeCases$`Case Owner`[NikeCases$`Case Owner` == 'Nike POS Services'] <- "Dusko Mladenovski"
NikeCases$`Account Name` <- gsub(" ", "", NikeCases$`Account Name`)
NikeCases$`Date/Time Opened` <-  as.Date(NikeCases$`Date/Time Opened`, '1900/1/1')
NikeCases$`Account Name` <- as.factor(NikeCases$`Account Name`)
NikeCases$`Date/Time Closed` <- as.Date(NikeCases$`Date/Time Opened` + NikeCases$`Case Age`, '1900/1/1')



ggplot(
  NikeCases[NikeCases$Closed == 1,], 
  aes(x = `Nike Case Number`, 
      y = `Case Age`, 
      col = factor(`Case Owner`)))+
  geom_point()+
  geom_hline(yintercept = 80)



theme_gantt <- function(base_size=11, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) %+replace%
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(size=0.5, colour="grey80"),
          axis.ticks=element_blank(),
          legend.position="bottom", 
          axis.title=element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y=unit(1.5, "lines"),
          legend.title = element_rect(fill = ))
  
  ret
}

#Gantt Chart Time it takes a case to close
ggplot(
  NikeCases[NikeCases$Closed == 1,], 
  aes(x = `Date/Time Opened`,
      Y = `Nike Case Number`,
       color = `Case Owner`))+
    geom_segment(aes(xend = `Date/Time Closed`, yend = `Nike Case Number`, y = `Nike Case Number`))+
  scale_y_discrete(breaks = c(seq(from = 0, to = 2000, by = 100)))+
  theme_gantt() + theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_x_date(date_breaks="3 months", labels=date_format("%b '%y")) +
  labs(x = NULL, y = NULL)


