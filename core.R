rm(list=ls(all=TRUE)) # wipes previous workspace

# Common Packages
packages<-c("curl","scales","zoo",
            "RColorBrewer","tidyverse","ggalt","gridExtra","ggridges",
            "ggpubr","testit","ggseas","readxl","grid",
            "ggplot2","ggthemes","jsonlite",
            "data.table","ggrepel","rmarkdown")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg)
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

# For the new StatCan Data Tables
getTABLE<-function(x) {
  url<-paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",x,"-eng.zip")
  temp<-tempfile()
  download.file(url,temp)
  if (has_warning(unzip(temp,paste0(x,".csv")))) { # Avoids html landing page
    download.file(url,temp)
  }
  unzip(temp,paste0(x,".csv"))
  rawdata<-read.csv(paste0(x,".csv"),encoding="UTF-8",stringsAsFactors=FALSE)
  colnames(rawdata)[1]<-"Ref_Date"
  data<-rawdata %>%
    dplyr::rename(Value=VALUE) %>%
    select(-UOM_ID,-SCALAR_ID)
  if (class(data$Ref_Date)=="character" & !grepl("/",data[1,"Ref_Date"])){
    data<-data %>%
      mutate(Ref_Date=as.yearmon(Ref_Date))
  }
  if ("GEO" %in% colnames(data)){
    data <- data %>%
      left_join(provnames,by="GEO")
  }
  if ("North.American.Industry.Classification.System..NAICS." %in% colnames(data)){
    data <- data %>%
      rename(NAICS=North.American.Industry.Classification.System..NAICS.) %>%
      mutate(NAICScode=str_match(NAICS,"\\[(.*?)\\]")[,2],
             NAICS=ifelse(regexpr(" \\[",NAICS)>1,
                          substr(NAICS,1,regexpr(" \\[",NAICS)-1),NAICS))
  }
  if (any(grepl("North.American.Product.Classification.System..NAPCS.",colnames(data)))){
    colnames(data)[grepl("North.American.Product.Classification.System..NAPCS.",colnames(data))]<-"NAPCS"
    data <- data %>%
      mutate(NAPCS=ifelse(regexpr(" \\[",NAPCS)>1,
                          substr(NAPCS,1,regexpr(" \\[",NAPCS)-1),NAPCS))
  }
  return(data)
}
loadTABLE<-function(x) {
  rawdata<-read.csv(paste0(x,".csv"),encoding="UTF-8",stringsAsFactors=FALSE)
  colnames(rawdata)[1]<-"Ref_Date"
  data<-rawdata %>%
    dplyr::rename(Value=VALUE) %>%
    select(-UOM_ID,-SCALAR_ID)
  if (class(data$Ref_Date)=="character" & !grepl("/",data[1,"Ref_Date"])){
    data<-data %>%
      mutate(Ref_Date=as.yearmon(Ref_Date))
  }
  if ("GEO" %in% colnames(data)){
    data <- data %>%
      left_join(provnames,by="GEO")
  }
  if ("North.American.Industry.Classification.System..NAICS." %in% colnames(data)){
    data <- data %>%
      rename(NAICS=North.American.Industry.Classification.System..NAICS.) %>%
      mutate(NAICScode=str_match(NAICS,"\\[(.*?)\\]")[,2],
             NAICS=ifelse(regexpr(" \\[",NAICS)>1,
                          substr(NAICS,1,regexpr(" \\[",NAICS)-1),NAICS))
  }
  if (any(grepl("North.American.Product.Classification.System..NAPCS.",colnames(data)))){
    colnames(data)[grepl("North.American.Product.Classification.System..NAPCS.",colnames(data))]<-"NAPCS"
    data <- data %>%
      mutate(NAPCS=ifelse(regexpr(" \\[",NAPCS)>1,
                          substr(NAPCS,1,regexpr(" \\[",NAPCS)-1),NAPCS))
  }
  return(data)
}

# Default theme
update_geom_defaults("point", list(colour = "firebrick"))
update_geom_defaults("line", list(colour = "firebrick"))

mytheme<-theme(
  axis.text.y = element_text(size=10),
  axis.text.x = element_text(size=10,hjust=0.5),
  axis.ticks.y=element_blank(),
  axis.ticks.x=element_blank(),
  panel.grid.major.y = element_line(size=0.5,color="black",linetype="dotted"), 
  panel.grid.major.x = element_line(size=0.5,color="black",linetype="dotted"), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  legend.text=element_text(size=8),
  legend.key = element_blank(),
  legend.position="top",
  legend.direction = "horizontal",
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  plot.title = element_text(size = 15, face = "bold",hjust=0.5),
  plot.subtitle = element_text(size = 8, face = "italic",hjust=0.5),
  plot.caption = element_text(size = 6, face = "italic")
)
mytheme<-theme_minimal()+theme(
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  legend.position = "top",
  legend.text=element_text(size=10),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  plot.caption = element_text(size = 6, color = "gray40",hjust=1),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)
mythemebar<-mytheme+theme(
  panel.grid.major.x = element_blank(),
  axis.text.x = element_text(size=12,hjust=0.5,face="bold",colour="black")
)
mythemebarflip<-theme_minimal()+theme(
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  legend.position = "top",
  legend.text=element_text(size=10),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  plot.caption = element_text(size = 6, color = "gray40"),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank()
)
mythemegif<-mytheme+theme(
  axis.title.y = element_text(size=12),
  axis.title.x = element_text(size=12),
  axis.text.y = element_text(size=12),
  axis.text.x = element_text(size=12,hjust=0.5),
  panel.grid.major.x = element_blank(),
  plot.subtitle = element_text(size = 10, face = "italic"),
  plot.title = element_text(size = 20, face = "bold")
)

mythemegray<-theme_gray()+theme(
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  axis.ticks = element_blank(),
  legend.position = "top",
  legend.text=element_text(size=10),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  plot.caption = element_text(size = 6, color = "gray40",hjust=1),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank()
)

mythememap<-theme(
  axis.text.y = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.y=element_blank(),
  axis.ticks.x=element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  legend.position="top",
  legend.text=element_text(size=10),
  plot.title = element_text(size = 16, face = "bold",hjust=0.5),
  plot.subtitle = element_text(size = 7, color="gray50",hjust=0.5),
  plot.caption = element_text(size = 6, face = "italic")
)

# Useful lists
provinces<-c("Canada","Newfoundland and Labrador","Prince Edward Island","Nova Scotia",
             "New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan",
             "Alberta","British Columbia","Yukon","Northwest Territories","Nunavut")
provinces2<-c("CAN","NL","PE","NS",
             "NB","QC","ON","MB","SK",
             "AB","BC","YT","NT","NU")
provsort<-c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL")
provnames<-data.frame(GEO=provinces,short=provinces2)
provnames$short <- factor(provnames$short, levels = c("CAN","BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU")) # Lock in factor level order

provorder<-tibble(GEO=c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL"),
                            order=as.numeric(seq(1,10)))

# This partially constructs the StatsCan trend-cycle estimate
# Doesn't do the first six periods. Need "Ref_date" defined
gettrend<-function(x,y){
  x<-x %>%
    mutate(periodsleft=12*(max(Ref_Date)-Ref_Date)) %>%
    rename(var=y) %>%
    mutate(TC=-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
             0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
             -0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
             0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1),
           TC5=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  -0.007*lead(var,5)+0.031*lead(var,4)+
                  0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027),
           TC4=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  0.031*lead(var,4)+
                  0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007),
           TC3=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031),
           TC2=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067),
           TC1=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067-0.136),
           TC0=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var)/(1+0.027+0.007-0.031-0.067-0.136-0.188)) %>%
    mutate(TC=ifelse(round(periodsleft,0)==5,TC5,TC),
           TC=ifelse(round(periodsleft,0)==4,TC4,TC),
           TC=ifelse(round(periodsleft,0)==3,TC3,TC),
           TC=ifelse(round(periodsleft,0)==2,TC2,TC),
           TC=ifelse(round(periodsleft,0)==1,TC1,TC),
           TC=ifelse(round(periodsleft,0)==0,TC0,TC)) %>%
    mutate(TC=ifelse(row_number()==1,
                     (0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                       0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067-0.136-0.188),TC),
           TC=ifelse(row_number()==2,
                     (0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067-0.136),TC),
           TC=ifelse(row_number()==3,
                     (0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067),TC),
           TC=ifelse(row_number()==4,
                     (0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031),TC),
           TC=ifelse(row_number()==5,
                     (0.031*lag(var,4)+0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007),TC),
           TC=ifelse(row_number()==6,
                     (-0.007*lag(var,5)+0.031*lag(var,4)+0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027),TC),
           TC=ifelse(row_number()==7,
                     (-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1)),TC)) %>%
  rename_(.dots = setNames("var", y))
  return(x)
}

# Construct own within-group seasonal adjustment with trend
getseas<-function(df,g){
  df<-df %>%
    rename(group_var=g)
  p<-ggsdc(df,aes(Ref_Date,Value,group=group_var,color=group_var),
           method="seas")+geom_line()
  temp<-p$data %>%
    filter(component %in% c("irregular","trend")) %>%
    group_by(x,group_var) %>%
    summarise(Value=sum(y)) %>%
    group_by(group_var) %>%
    rename(Ref_Date=x) %>%
    gettrend("Value") %>%
    rename_(.dots = setNames("group_var", g)) %>%
    ungroup()
  return(temp)
}

