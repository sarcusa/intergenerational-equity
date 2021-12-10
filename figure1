###############################
#
# Author: Stephanie Arcusa
# Publication: Arcusa, SH, and KS Lackner. Intergenerational equity and responsibility: a call to internalize impermanence into certificates of carbon sequestration. 
#Analysis of the Microsoft and Stripe purchases from the Carbon Plan dataset (https://carbonplan.org/research/cdr-database).

library(ggplot2)
library(scales)

setwd("D:/ASU Google/ASU Carbon Negative Postadoc/Certificate of Storage/Intergenerational equity/")

df1 <- read.csv("projects.csv", skip = 0, header = T)
df1 <- subset(df1, df1$mechanism != 1)
df1 <- df1[,c("id","source","volume","permanence")]
df1$permanence <- as.numeric(df1$permanence)
df1 <- df1[complete.cases(df1), ]

time <- seq(1,max(df1$permanence))
remaining <- vector()

for(i in time){
  
  remaining[1] <- sum(df1$volume)
  
  w <- which(df1$permanence < time[i+1])
  if(length(w) == 0){
    w <- remaining[i-1]
  }
  remaining[i+1] <- sum(df1$volume[-w])
  
  remaining <- remaining[1:length(time)]
  
}

responsibility <- sum(df1$volume)- remaining

remain <- data.frame(Time = time, Unmanaged = responsibility, Managed = remaining)
re.m <- reshape2::melt(remain, id.vars = "Time", value.name = "Total")

manage.plot <- ggplot(re.m, aes(x = Time, y = Total))+
  geom_area(aes(fill = variable))+
  scale_y_continuous(labels = comma,expand = c(0, 0))+
  #scale_x_continuous(expand = c(0, 0))+
  scale_fill_manual(values = c("mediumpurple1", "forestgreen"))+
  scale_x_log10(breaks = c(1,5,10,50,100,500,1000), expand = c(0, 0))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.title = element_blank())+
  ylab("Cumulative proposed carbon sequestered (t CO2)")+
  xlab("Stipulated permanence (years, log scale)")+
  geom_vline(xintercept = min(which(abs(remain$Managed-max(remain$Managed)/2)==min(abs(remain$Managed-max(remain$Managed)/2)))), linetype="dashed")
manage.plot

ggsave(filename = "carbon_management_time_logscale2.pdf", plot = manage.plot, device = "pdf",path = paste0(getwd(),"/Figures"))
