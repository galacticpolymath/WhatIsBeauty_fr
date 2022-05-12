require(pacman)
p_load(dplyr,galacticPubs,readr,utils,skimr,galacticEdTools)

NA_outliers <- function(df, QUANTILE_RANGE,id=NA,ignore) {
  # df should be a dataframe or matrix; QUANTILE_RANGE should be in the form c(.01,.99);
  # optional id (e.g. "band" or "bandyear") should be column name for reporting which values were switched to NA
  # ignore (not required) should be a vector a la c("length","width") or c(1:9), which specifies columns to ignore;
  # factors are ignored automatically, but you may wish to ignore some numeric columns as well

  if(missing(QUANTILE_RANGE)){QUANTILE_RANGE<-c(0.01,0.99)} #default quantile range

  df.orig<-df #for adding ignored columns back in at the end
  if(!missing(ignore)){
    if(is.numeric(ignore)){df<-df[,-ignore]
      ignames<-names(df.orig)[ignore]
      }else{df<-df[,-match(ignore,names(df))]
      ignames<-ignore}
      IGNORED<-df.orig[,ignames]} #make subset of data frame with selected columns removed, accounting for how columns are specified (numeric or names)

  #For checking data organization
check_class<-function(dataframe){
  #Check that columns are appropriately assigned to factor, numeric, etc
  Class<- sapply(1:length(dataframe),function(x)class(dataframe[,x]))
  ColumnName<-names(dataframe)
  return(cbind(ColumnName,Class))
}


  #Define function for calculating outliers and replacing with NA for vectors (each column)
  vector.outlier.remover<-function(x,QUANTILE_RANGE,na.rm=T,...)
  {
    if(is.numeric(x)) #only runs script on numeric columns
    {
      qnt <- quantile(x, probs=QUANTILE_RANGE, na.rm = na.rm)
      H <- 1.5 * IQR(x, na.rm = na.rm)
      y <- x
      y[x < (qnt[1] - H)] <- NA
      y[x > (qnt[2] + H)] <- NA
      return(y)
    }else{return(as.character(x))}
  }#end vector.outlier.remover

  OUTPUT<-apply(df,2,function(x) {vector.outlier.remover(x,QUANTILE_RANGE)} )

  #Get indices for reporting changes
  CHANGED.index<-which(is.na(OUTPUT)&!is.na(df),arr.ind=T)
  ###MAke factors in OUTPUT match factors of columns in df
  class.df<-check_class(as.data.frame(df))[,"Class"]
  OUTPUT<-data.frame(OUTPUT,stringsAsFactors = F)
  for(i in 1: length(df))
  {
    if(class.df[i]=="factor"){OUTPUT[,i]<-as.factor(as.character(OUTPUT[,i]))
    }else{class(OUTPUT[,i])<-class.df[i]}
  }
  #Combine with ignored columns, make sure names stay same
  if(!missing(ignore)){
    OUTPUT<-cbind(data.frame(OUTPUT,stringsAsFactors = F),IGNORED)
    }else{OUTPUT<-OUTPUT}



  if(attributes(CHANGED.index)$dim[1]==0){
    return(list(newdata=OUTPUT,changelog="No Changes"))
  }else{
    CHANGED<-t(sapply(1:length(CHANGED.index[,1]),function(x) {
      id_x<-ifelse(is.na(id),
                   paste0("row ", CHANGED.index[x, "row"]),
                   as.character(OUTPUT[CHANGED.index[x, "row"], id]))
      data.frame(ID=id_x,
        COLUMN=names(df)[CHANGED.index[x, "col"]],
        OUTLIER=unlist(signif(df[CHANGED.index[x, "row"], CHANGED.index[x, "col"]], 3)),
        MEAN=signif(mean(unlist(df[, CHANGED.index[x, "col"]]), na.rm = T), 3))
    }
    ))
    CHANGED<-data.frame(CHANGED,stringsAsFactors = F)
    return(list(newdata=OUTPUT,changelog=CHANGED))
  }

}#End NA_outliers



#Read in human datasets

# SOURCE 1
# Kiru, Muhammad  (2021), “Body Measurements Datasets”, Mendeley Data, V1, doi: 10.17632/bjv6c9pmp4.1
# browseURL("https://data.mendeley.com/datasets/bjv6c9pmp4/1")
#
# UNITS ARE INCHES!
kiru0<-read_csv("data/Body Measurements _ original_CSV.csv")
skim(kiru0$Age)

#let's filter out younguns under 13
kiru<-kiru0 %>% filter(Age>13)

skim(kiru)
#still have 396 measurements
#There are some WEEEEIRD values here...e.g. 213 for Belly and 66 for arm length (with p75 of 22)
NA_outliers(kiru)$changelog
#Just take out those crazy values
kiru2<-NA_outliers(kiru)$newdata
#There's also some more crazy ones we didn't get: a TotalHeight of
kiru2$HeightTest<-ifelse(kiru2$WaistToKnee+kiru2$LegLength<kiru2$TotalHeight,"PASS","FAIL")

#Something real funky with these TotalHeight values
kiru3<-kiru2 %>% filter(HeightTest=="PASS") %>% as_tibble()
skim(kiru3) #This looks much better!

kiru4<-kiru3 %>% mutate(ArmLength_over_Height=ArmLength/TotalHeight,LegLength_over_Height=LegLength/TotalHeight)

# "Reachiness"
mean_Reachiness<-mean(kiru4$ArmLength_over_Height,na.rm=T)
Col1<-gpColors("burst")
Col2<-gpColors("burst")
kiru4 %>% ggplot() +
  geom_histogram(aes(x=ArmLength_over_Height),fill=Col1,col=gpColors("gal"),binwidth=0.1)+
  scale_x_continuous(breaks=seq(0,2,0.25),labels=sapply(seq(0,2,0.25),function(x) ifelse(x%%1==0|x%%1==0.5,x,"")),
                     expand=expansion(0))+
  theme_galactic(bg.col = "gray98",grid.col = "gray70",pad.xlab=30,pad.outer=rep(10,4),font.face = 2,text.cex = c(0,1.2,1.7,1))+
  theme(axis.line = element_line(size=1.1,colour="gray60"))+
  geom_label(x=0.49,y=100,fontface=2,family="Montserrat",label="Range of \nHuman Variation",hjust=0,vjust=1.1,col="white",fill=Col1,size=8,label.padding = unit(10,"pt"),lineheight=0.25)+
  xlab('"Reachiness" (Arm Length / Height)')+
  ylab("Count")+
  coord_cartesian(clip="off",xlim = c(0,2),expand=F,ylim=c(0,100))+
  #add arrow pointing at mean
  geom_segment(x=mean_Reachiness,xend=mean_Reachiness,y=-17,yend=-3,arrow=arrow(length=unit(10,"pt")),col=Col2)+
  annotate("text",x=mean_Reachiness,y=-20,col=Col2,label=paste0("Human Average\n(",round(mean_Reachiness,2),")"),size=8,vjust=1,lineheight=0.25)
gpsave("reachiness figure.png",height=2.5,width=7)

# "Legginess" graph
mean_Legginess<-mean(kiru4$LegLength_over_Height,na.rm=T)
Col1<-gpColors("hydrogen blue")
Col2<-gpColors("hydrogen blue")
kiru4 %>% ggplot() +
  geom_histogram(aes(x=LegLength_over_Height),fill=Col1,col=gpColors("gal"),binwidth=0.1)+
  scale_x_continuous(breaks=seq(0,2,0.25),labels=sapply(seq(0,2,0.25),function(x) ifelse(x%%1==0|x%%1==0.5,x,"")),
                     expand=expansion(0))+
  theme_galactic(bg.col = "gray98",grid.col = "gray70",pad.xlab=30,pad.outer=rep(10,4),font.face = 2,text.cex = c(0,1.2,1.7,1))+
  theme(axis.line = element_line(size=1.1,colour="gray60"))+
  geom_label(x=0.7,y=100,fontface=2,family="Montserrat",label="Range of \nHuman Variation",hjust=0,vjust=1.1,col="white",fill=Col1,size=8,label.padding = unit(10,"pt"),lineheight=0.25)+
  xlab('"Leginess" (Leg Length / Height)')+
  ylab("Count")+
  coord_cartesian(clip="off",xlim = c(0,2),expand=F,ylim=c(0,100))+
  #add arrow pointing at mean
  geom_segment(x=0.4,xend=mean_Legginess,y=-17,yend=-3,arrow=arrow(length=unit(10,"pt")),col=Col2)+
  annotate("text",x=0.4,y=-20,col=Col2,label=paste0("Human Average\n(",round(mean_Legginess,2),")"),size=8,vjust=1,lineheight=0.25)
gpsave("legginess figure.png",height=2.5,width=7)


write_csv(kiru4,"data/human_proportion_distributions.csv")
