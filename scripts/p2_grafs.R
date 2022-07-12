require(pacman)
p_load(galacticPubs,galacticEdTools,ggplot2,readr,readxl,reshape2,dplyr)

d<-read_excel("data/orig-client-data_NoEdit/barrenense female summary 2011 RvsO.xlsx")

d2<-d %>% group_by(`ID#`) %>% filter(complete.cases(`ID#`)) %>%
  select(`Red Prop`,`Orange Prop`) %>% rename(Red=`Red Prop`,Orange=`Orange Prop`) %>% melt()

d3<-d2 %>% filter(value<0.8)#Remove one outlier

(g0<-d3 %>% ggplot(aes(x=variable,y=value))+geom_boxplot(colour="#363636",show.legend=F)+theme_galactic(pad.xlab = 25,text.cex = 1.2)+theme(axis.title=element_text(lineheight=.3))+
  labs(x="Model Type",y="Time Spent Near Model (%)"))

#### This data doesn't seem to match results from Williams, BehEco 2013 :shrug:
# Since Mendelson can't easily find this data :/ I'll just simulate it from the paper results (reported mean, SD, and general look of the whisker plots)
#

# define truncated normal distribution function
# from: browseURL("https://stackoverflow.com/questions/19343133/setting-upper-and-lower-limits-in-rnorm")
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
    qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}
set.seed(865)
sim<-tibble(
  ID = d$`ID#`[1:19],
  orange = rtnorm(19, mean = 37.57, sd = 24.1,a=0,b=100),
  red = rtnorm(19, mean = 15.94, sd = 20.23,a=0,b=100)
) %>% melt()


(g1 <- sim %>% ggplot(aes(x=variable,y=value,fill=variable))+
    geom_boxplot(colour="#363636",show.legend=F,width=.6)+
    geom_point(col="#363636",pch=21,position = position_jitterdodge(),show.legend=F)+
    theme_galactic(pad.xlab = 10)+
    scale_fill_manual(values=c("gray60","white"))+
    # theme(axis.title=element_text(lineheight=.3))+
    ylim(0,100)+
    labs(x="Model Type",y="Time Spent\nNear Model (%)"))
#Save normal version
gpsave(plot=g1,"darter experiment_original.png")

#Save teacher version of plot
g1+highlight_labs(c("x-vals"),txt_col = gpColors("hydr"))
gpsave("darter experiment_Teacher.png")

#Make Student version of lesson
   g1 + blank_labs(x_txt=NA,y_txt=NA,title=NA,xval_txt="Label the Color")
gpsave("darter experiment_Student.png")
ggsave("assets/R_outputs/x.png")


#Make table for lesson (to label boxplot)
View(sim)
(data_slice<-sim[c(1,3,5,9,17,21,25,27,29,31),])
names(data_slice)<-c("ID","model_type","perc_time_spent_near_model")
write.csv(data_slice,"assets/r_outputs/darter_data_table_for_P1.csv",row.names = F)
