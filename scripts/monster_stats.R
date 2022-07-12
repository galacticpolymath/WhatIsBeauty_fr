require(pacman)
p_load(dplyr,galacticPubs,readr,utils,skimr,galacticEdTools,readxl,glue)

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

# SOURCE 1: Basic Body Measurements
# Kiru, Muhammad  (2021), “Body Measurements Datasets”, Mendeley Data, V1, doi: 10.17632/bjv6c9pmp4.1
# browseURL("https://data.mendeley.com/datasets/bjv6c9pmp4/1")
#
# UNITS ARE INCHES!
kiru00<-read_csv("data/Body Measurements _ original_CSV.csv")

#convert to cm
kiru0<-kiru00 %>% mutate(across(3:13,function(x) x*2.54))

####
# SOURCE 2: Facial measurements
#  Durkee, Patrick K., and Jessica D. Ayers. "Is facial width-to-height ratio reliably associated with social inferences?." Evolution and Human Behavior 42.6 (2021): 583-592.
# https://osf.io/fahzw/

# Note on measurement from the paper: We therefore calculated new fWHR measurements
# for the 120 faces that
# were rated as part of the Jones, Schild, and Jones (2020) study based on
# more common operationalization of the bizygomatic width of the face
# divided by the distance between the upper lip and the ridge of the
# eyebrow (Lefevre et al., 2013).

# Corresponded w/ Patrick Durkee. Sent me this link to the Chicago Face Data meas. explanation
# https://www.chicagofaces.org/cfd_public/cfdmguide.pdf

durkee0<-read_csv("data/CFD_target_data.csv")
#take a similar random subset as kiru (N=290)
durkee <- durkee0 %>% slice_sample(n=290)

#####
# SOURCE 3: Chicago Face Database
# https://www.chicagofaces.org/
# A C K N O W L E D G E M E N T
# Use of the database materials should be acknowledged as follows:
# CFD: Ma, Correll, & Wittenbrink (2015). The Chicago Face Database: A Free Stimulus Set of Faces and Norming Data. Behavior Research Methods, 47, 1122-1135. https://doi.org/10.3758/s13428-014-0532-5.
# CFD-MR: Ma, Kantner, & Wittenbrink, (2020). Chicago Face Database: Multiracial Expansion. Behavior Research Methods. https://doi.org/10.3758/s13428-020-01482-5.
# CFD-INDIA: Lakshmi, Wittenbrink, Correll, & Ma (2020). The India Face Set: International and Cultural Boundaries Impact Face Impressions and Perceptions of Category Membership. Frontiers in Psychology, 12, 161. https://doi.org/10.3389/fpsyg.2021.627678.
#
# Primary trait(s) of interest
# FaceWidthBZ:	Maximum distance between left and right facial boundary
# UpperFaceLength2:	Distance between the top of upper lip and highest point of the eyelids.
# fWHR (Face Width: Height Ratio): FaceWdithBZ/UpperFaceLength2
#
# Import CFD data
cfd_us<-read_excel("data/CFD 3.0 Norming Data and Codebook.xlsx","CFD U.S. Norming Data",skip=7)[-1,] %>% mutate(dataset="cfd_us")
cfd_us_multiracial<-read_excel("data/CFD 3.0 Norming Data and Codebook.xlsx","CFD-MR U.S. Norming Data",skip=7)[-1,] %>% mutate(dataset="cfd_us_MS")
cfd_india<-read_excel("data/CFD 3.0 Norming Data and Codebook.xlsx","CFD-I U.S. Norming Data",skip=7)[-1,] %>% mutate(dataset="cfd_india")
#Combine the data
CFD0<-cfd_us %>% full_join(.,cfd_us_multiracial) %>% full_join(.,cfd_india)

# Process Data Source 1 ---------------------------------------------------


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

# Process Data Source 2 ---------------------------------------------------
skim(durkee)
NA_outliers(durkee)$changelog
#don't seem to be any bad outliers
# Calculate maximum width (i.e. bizygomatic face width)
durkee <- durkee %>% rowwise() %>%  mutate(Face_Width_Max=max(Face_Width_Mouth,Face_Width_Cheeks))


# Process Data Source 3 ---------------------------------------------------
skim(CFD0)
ggplot(CFD0) + geom_histogram(aes(x=UpperFaceLength2,fill=dataset))
ggplot(CFD0) + geom_histogram(aes(x=FaceWidthBZ,fill=dataset))
ggplot(CFD0) + geom_histogram(aes(x=fWHR2,fill=dataset))
#Make equivalent sample size to kiru4
CFD <- CFD0%>% slice_sample(n=199)

# MAKE GRAPHS -------------------------------------------------------------

# "Reachiness"
# look at traits of interest
kiru4 %>% select(ArmLength,TotalHeight) %>% skim()
mean_Reachiness<-mean(kiru4$ArmLength_over_Height,na.rm=T)
rng<-range(kiru4$ArmLength_over_Height,na.rm=T) %>% round(.,2)
range_Reachiness<-glue::glue("({paste0(rng,collapse=', ')})")
Col1<-gpColors("burst")
Col2<-gpColors("burst")
kiru4 %>% ggplot() +
  geom_histogram(aes(x=ArmLength_over_Height),fill=Col1,col=gpColors("gal"),binwidth=0.05)+
  scale_x_continuous(breaks=seq(0,2,0.25),labels=sapply(seq(0,2,0.25),function(x) ifelse(x%%1==0|x%%1==0.5,x,"")),
                     expand=expansion(0))+
  theme_galactic(bg.col = "gray98",grid.col = "gray70",pad.xlab=30,pad.outer=rep(10,4),font.face = 2,text.cex = c(0,1.2,1.7,1))+
  theme(axis.line = element_line(size=1.1,colour="gray60"))+
  geom_label(x=0.49,y=100,fontface=2,family="Montserrat",label=paste0("Range of \nHuman Variation\n",range_Reachiness),hjust=0,vjust=1.1,col="white",fill=Col1,size=8,label.padding = unit(10,"pt"),lineheight=0.25)+
  xlab('"Reachiness" (Arm Length / Height)')+
  ylab("Count")+
  coord_cartesian(clip="off",xlim = c(0,2),expand=F,ylim=c(0,100))+
  #add arrow pointing at mean
  geom_segment(x=mean_Reachiness,xend=mean_Reachiness,y=-17,yend=-3,arrow=arrow(length=unit(10,"pt")),col=Col2)+
  annotate("text",x=mean_Reachiness,y=-20,col=Col2,label=paste0("Human Average\n(",round(mean_Reachiness,2),")"),size=8,vjust=1,lineheight=0.25)
gpsave("monsterStatsGraf_reachiness figure.png",height=2.5,width=7)

# "Legginess" graph
mean_Legginess<-mean(kiru4$LegLength_over_Height,na.rm=T)
rng<-range(kiru4$LegLength_over_Height,na.rm=T) %>% round(.,2)
range_Legginess<-glue::glue("({paste0(rng,collapse=', ')})")
Col1<-gpColors("hydrogen blue")
Col2<-gpColors("hydrogen blue")
kiru4 %>% ggplot() +
  geom_histogram(aes(x=LegLength_over_Height),fill=Col1,col=gpColors("gal"),binwidth=0.05)+
  scale_x_continuous(breaks=seq(0,2,0.25),labels=sapply(seq(0,2,0.25),function(x) ifelse(x%%1==0|x%%1==0.5,x,"")),
                     expand=expansion(0))+
  theme_galactic(bg.col = "gray98",grid.col = "gray70",pad.xlab=30,pad.outer=rep(10,4),font.face = 2,text.cex = c(0,1.2,1.7,1))+
  theme(axis.line = element_line(size=1.1,colour="gray60"))+
  geom_label(x=0.7,y=100,fontface=2,family="Montserrat",label=paste0("Range of \nHuman Variation\n",range_Legginess),hjust=0,vjust=1.1,col="white",fill=Col1,size=8,label.padding = unit(10,"pt"),lineheight=0.25)+
  xlab('"Legginess" (Leg Length / Height)')+
  ylab("Count")+
  coord_cartesian(clip="off",xlim = c(0,2),expand=F,ylim=c(0,100))+
  #add arrow pointing at mean
  geom_segment(x=0.4,xend=mean_Legginess,y=-17,yend=-3,arrow=arrow(length=unit(10,"pt")),col=Col2)+
  annotate("text",x=0.4,y=-20,col=Col2,label=paste0("Human Average\n(",round(mean_Legginess,2),")"),size=8,vjust=1,lineheight=0.25)
gpsave("monsterStatsGraf_legginess figure.png",height=2.5,width=7)

# "Hammerheadedness" graph
mean_hammerheadedness<-mean(CFD$fWHR2,na.rm=T)
rng<-range(CFD$fWHR2,na.rm=T) %>% round(.,2)
range_hammerheadedness<-glue::glue("({paste0(rng,collapse=', ')})")
Col1<-gpColors("dark atomic blue")
Col2<-gpColors("dark atomic blue")
CFD %>% ggplot() +
  geom_histogram(aes(x=fWHR2),fill=Col1,col="#7971BA",binwidth=0.1)+
  scale_x_continuous(breaks=seq(0,4,0.25),labels=sapply(seq(0,4,0.25),function(x) ifelse(x%%1==0|x%%1==0.5,x,"")),
                     expand=expansion(0))+
  theme_galactic(bg.col = "gray98",grid.col = "gray70",pad.xlab=30,pad.outer=rep(10,4),font.face = 2,text.cex = c(0,1.2,1.7,1))+
  theme(axis.line = element_line(size=1.1,colour="gray60"))+
  geom_label(x=2.2,y=75,fontface=2,family="Montserrat",label=paste0("Range of \nHuman Variation\n",range_hammerheadedness),hjust=0,vjust=1.1,col="white",fill=Col1,size=8,label.padding = unit(10,"pt"),lineheight=0.25)+
  xlab('"Hammerheadedness" (Face Width / Mid-Face Length)')+
  ylab("Count")+
  coord_cartesian(clip="off",xlim = c(0,4),expand=F,ylim=c(0,100))+
  #add arrow pointing at mean
  geom_segment(x=1.9,xend=mean_hammerheadedness,y=-17,yend=-3,arrow=arrow(length=unit(10,"pt")),col=Col2)+
  annotate("text",x=1.9,y=-20,col=Col2,label=paste0("Human Average\n(",round(mean_hammerheadedness,2),")"),size=8,vjust=1,lineheight=0.25)
gpsave("monsterStatsGraf_hammerheadedness figure.png",height=2.5,width=7)

##########3
# "Eye-Roundness" graph
# Look at means and range
CFD %>% select(EyeHeightAvg,EyeWidthAvg) %>% skim()

CFD$EyeRoundNess<-CFD$EyeHeightAvg/CFD$EyeWidthAvg
mean_eyeroundness<-mean(CFD$EyeRoundNess,na.rm=T)
rng<-range(CFD$EyeRoundNess,na.rm=T) %>% round(.,2)
range_eyeroundness<-glue::glue("({paste0(rng,collapse=', ')})")
Col1<-gpColors("lightning purple")
Col2<-gpColors("lightning purple")
CFD %>% ggplot() +
  geom_histogram(aes(x=EyeRoundNess),fill=Col1,col=gpColors("light lightning purple"),binwidth=0.05)+
  scale_x_continuous(breaks=seq(0,4,0.25),labels=sapply(seq(0,4,0.25),function(x) ifelse(x%%1==0|x%%1==0.5,x,"")),
                     expand=expansion(0))+
  theme_galactic(bg.col = "gray98",grid.col = "gray70",pad.xlab=30,pad.outer=rep(10,4),font.face = 2,text.cex = c(0,1.2,1.7,1))+
  theme(axis.line = element_line(size=1.1,colour="gray60"))+
  geom_label(x=0.48,y=75,fontface=2,family="Montserrat",label=paste0("Range of \nHuman Variation\n",range_eyeroundness),hjust=0,vjust=1.1,col="white",fill=Col1,size=8,label.padding = unit(10,"pt"),lineheight=0.25)+
  xlab('"Eye-Roundness" (Eye Height / Eye Width)')+
  ylab("Count")+
  coord_cartesian(clip="off",xlim = c(0,1.5),expand=F,ylim=c(0,100))+
  #add arrow pointing at mean
  geom_segment(x=mean_eyeroundness,xend=mean_eyeroundness,y=-17,yend=-3,arrow=arrow(length=unit(10,"pt")),col=Col2)+
  annotate("text",x=mean_eyeroundness,y=-20,col=Col2,label=paste0("Human Average\n(",round(mean_eyeroundness,2),")"),size=8,vjust=1,lineheight=0.25)
gpsave("monsterStatsGraf_eyeroundness figure.png",height=2.5,width=7)

# write_csv(kiru4,"data/human_proportion_distributions.csv")
