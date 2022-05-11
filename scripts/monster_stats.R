require(pacman)
p_load(dplyr,ggplot2,readr,utils,skimr,galacticEdTools)

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
kiru3<-kiru2 %>% filter(HeightTest=="PASS")
skim(kiru3) #This looks much better!

kiru_prop<-tibble(row=1:nrow(kiru3),`Arm Length / Height`=kiru3$ArmLength/kiru3$TotalHeight)

kiru_prop %>% ggplot() +
  geom_histogram(aes(x=`Arm Length / Height`),fill="gray50",col=gpColors("gal"),bins=15)+
  theme_galactic()


write_csv(kiru_prop,"data/human_proportion_distributions.csv")
