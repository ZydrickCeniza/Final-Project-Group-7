library(rvest)
library(httr)
library(dplyr) 
library(polite)

library(kableExtra)



polite::use_manners(save_as = 'polite_scrape.R')


url <- 'https://www.airlinequality.com/airline-reviews/airasia/'
session <- bow(url,
               user_agent = "Educational")




title <- character(0)


titles_list <- scrape(session) %>%
  html_nodes('h3.text_sub_header') %>% 
  html_text

titles_list_sub <- as.data.frame(titles_list[1:10])
colnames(titles_list_sub) <- "ranks"


split_df <- strsplit((titles_list_sub$ranks),"\r\n","reviews",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))

title<- data.frame(
  title= split_df)

title<-title[,c(-1,-2,-3)]
split_df <- strsplit((title),"(",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))
name<-as.data.frame(split_df[,1])

split_df <- strsplit((split_df$X2),")",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))
num1<-cbind(name,split_df)


# Create a 2x3 array
xname<-array(2:30)
xone<-array(2:30)
xtwo<-array(2:30)
# Example for loop
for (i in 2:30) {
  url<-paste("https://www.airlinequality.com/airline-reviews/airasia/page/",i,"/", sep = "")
  url
  session <- bow(url,
                 user_agent = "Educational")
  
  
  title <- character(0)
  
  
  titles_list <- scrape(session) %>%
    html_nodes('h3.text_sub_header') %>% 
    html_text
  
  titles_list_sub <- as.data.frame(titles_list[1:10])
  colnames(titles_list_sub) <- "ranks"
  
  
  split_df <- strsplit((titles_list_sub$ranks),"\r\n","reviews",fixed = TRUE)
  split_df <- data.frame(do.call(rbind,split_df))
  
  title<- data.frame(
    title= split_df)

  title<-title[,c(-1,-2,-3)]

  split_df<- strsplit((title),"(",fixed = TRUE)
  split_df<- data.frame(do.call(rbind,split_df))
  split_df
  name<-as.data.frame(split_df[,1])

  split_df<- strsplit((split_df$X2),")",fixed = TRUE)
  split_df<- data.frame(do.call(rbind,split_df))
  split_df
  xname[i]<-name
  xone[i]<-as.data.frame(split_df$X1)
  xtwo[i]<-as.data.frame(split_df$X2)
}
for (i in 2:30){
  df1<-data.frame(
    data=c(xone[i])
  ) 
  colnames(df1)<-"data"
  
  if(i==2){
    resc<-df1
  }
  else if(i>2){
    resc<-rbind(resc,df1)
  }
}

for (i in 2:30){
  df1<-data.frame(
    data=c(xname[i])
  ) 
  colnames(df1)<-"data"
  
  if(i==2){
    resn<-df1
  }
  else if(i>2){
    resn<-rbind(resn,df1)
  }
}

for (i in 2:30){
  df1<-data.frame(
    data=c(xtwo[i])
  ) 
  colnames(df1)<-"data"
  
  if(i==2){
    resd<-df1
  }
  else if(i>2){
    resd<-rbind(resd,df1)
  }
}
num2<-cbind(resn,resc,resd)
colnames(num1)<-c("Name","Country","Date")
colnames(num2)<-c("Name","Country","Date")
part3<-rbind(as.data.frame(num1),as.data.frame(num2))

#-----------------------------------------------------------------------
name <- character(0)

title_list <- scrape(session) %>%
  html_nodes('td.review-value') %>% 
  html_text
title_list_sub <- as.data.frame(title_list[])


name<- title_list_sub
colnames(name)<-c("Name")

split_df <- strsplit((name[,1]),"|", fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))

colnames(split_df)<-"details"

split_df<-subset(split_df, details!="A320")
b1y5<-seq(1,50,by=5)
g1<-as.data.frame(split_df[b1y5,])
b2y5<-seq(2,50,by=5)
g2<-as.data.frame(split_df[b2y5,])

b3y5<-seq(3,50,by=5)
g3<-as.data.frame(split_df[b3y5,])



b4y5<-seq(4,50,by=5)
g4<-as.data.frame(split_df[b4y5,])
b5y5<-seq(5,50,by=5)
g5<-as.data.frame(split_df[b5y5,])

group1<-cbind(g1,g2,g3,g4,g5)

gc1<-array(2:30)
gc2<-array(2:30)
gc3<-array(2:30)
gc4<-array(2:30)
gc5<-array(2:30)

# Example for loop
for (i in 2:30) {
  url<-paste("https://www.airlinequality.com/airline-reviews/airasia/page/",i,"/", sep = "")

  session <- bow(url,
                 user_agent = "Educational")
  
  
  group<- character(0)
  
  
  group_list <- scrape(session) %>%
    html_nodes('td.review-value') %>% 
    html_text
  
  group_list_sub <- as.data.frame(group_list[])
  colnames(group_list_sub) <- "details"
  
  split_df<-subset(group_list_sub, details!="A320" & details!="A320-200" & details!="Airbu" & details!="A320 Neo" & details!="A330" & details!="A330-300" & details!="A330 Neo" & details!="a320" & details!="AirAsia airline") 
  b1y5<-seq(1,50,by=5)
  g1<-as.data.frame(split_df[b1y5,])
  colnames(g1)<-c("TypeofTraveller")
  gc1[i]<-g1
  
  b2y5<-seq(2,50,by=5)
  g2<-as.data.frame(split_df[b2y5,])
  colnames(g2)<-c("SeatType")
  gc2[i]<-g2
  
  b3y5<-seq(3,50,by=5)
  gc3[i]<-as.data.frame(split_df[b3y5,])
  
  
  b4y5<-seq(4,50,by=5)
  g4<-as.data.frame(split_df[b4y5,])
  gc4[i]<-g4
  
  b5y5<-seq(5,50,by=5)
  g5<-as.data.frame(split_df[b5y5,])
  gc5[i]<-g5
  
  
}
for (i in 2:30){
  df1<-data.frame(
    data=c(gc1[i])
  ) 
  colnames(df1)<-"TypeofTraveller"
  
  if(i==2){
    restot<-df1
  }
  else if(i>2){
    restot<-rbind(restot,df1)
  }
}
for (i in 2:30){
  df2<-data.frame(
    data=c(gc2[i])
  ) 
  colnames(df2)<-"SeatType"
  
  if(i==2){
    resst<-df2
  }
  else if(i>2){
    resst<-rbind(resst,df2)
  }
}
for (i in 2:30){
  df3<-data.frame(
    data=c(gc3[i])
  ) 
  colnames(df3)<-"Route"
  
  if(i==2){
    resroute<-df3
  }
  else if(i>2){
    resroute<-rbind(resroute,df3)
  }
}
for (i in 2:30){
  df4<-data.frame(
    data=c(gc4[i])
  ) 
  colnames(df4)<-"DateFlown"
  
  if(i==2){
    resd<-df4
  }
  else if(i>2){
    resd<-rbind(resd,df4)
  }
}
for (i in 2:30){
  df5<-data.frame(
    data=c(gc5[i])
  ) 
  colnames(df5)<-"Recomended"
  
  if(i==2){
    resrec<-df5
  }
  else if(i>2){
    resrec<-rbind(resrec,df5)
  }
}

group2<-cbind(restot,resst,resroute,resd,resrec)
colnames(group1)<-c("TypeofTraveller","SeatType","Route","DateFlown","Recommended")
colnames(group2)<-c("TypeofTraveller","SeatType","Route","DateFlown","Recommended")
part4<-rbind(group1,group2)

#---------------------------------------------------------------------------------
title <- character(0)


titles_list <- scrape(session) %>%
  html_nodes('h2.text_header') %>% 
  html_text

titles_list_sub <- as.data.frame(titles_list[1:10])
colnames(titles_list_sub) <- "Title"


title<- data.frame(
  title= titles_list_sub)

tilt<-as.data.frame(title)
split_df <- strsplit((tilt[,1]),"\"", fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))
tilt<-split_df
tilt<-tilt[,-1]
tilt<-as.data.frame(tilt)

xtitle<-array(2:30)


for (i in 2:30) {
  url<-paste("https://www.airlinequality.com/airline-reviews/airasia/page/",i,"/", sep = "")

  session <- bow(url,
                 user_agent = "Educational")
  
  
  title <- character(0)
  
  
  titles_list <- scrape(session) %>%
    html_nodes('h2.text_header') %>% 
    html_text
  
  titles_list_sub <- as.data.frame(titles_list[1:10])
  colnames(titles_list_sub) <- "Title"
  
  title<- data.frame(
    title= titles_list_sub)
  
  title<-as.data.frame(title)

  xtitle[i]<-as.data.frame(title[,1])
  
}
for (i in 2:30){
  df1<-data.frame(
    data=c(xtitle[i])
  ) 
  colnames(df1)<-"Title"
  
  if(i==2){
    rest<-df1
  }
  else if(i>2){
    rest<-rbind(rest,df1)
  }
}
colnames(tilt)<-"Title"
part5<-rbind(tilt,rest)
part5
#-------------------------------------------------------------------


