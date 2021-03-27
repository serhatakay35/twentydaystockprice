library(rvest)
library(stringr)

twentydaystockprice<-function (a) {

preurl<-"https://www.marketwatch.com/investing/stock/" 
posturl<-"/download-data"
stock_marketwatch_url<-paste(preurl,a,posturl,sep="")
stock_marketwatch_webpage<-read_html(stock_marketwatch_url)
stock_values_marketwatch<-html_nodes(stock_marketwatch_webpage,'[class="overflow--table"]')
stock_rank_data<-html_text(stock_values_marketwatch)

stock_df<-data.frame(temp=rep(0,20), Date=rep(0,20),temp=rep(0,20),Open=rep(0,20),High=rep(0,20),Low=rep(0,20),Close=rep(0,20),Volume=rep(0,20))
stock_rank_data<-stock_rank_data[1]
stock_rank_data<-as.character(stock_rank_data)
stock_rank_data<-substring(stock_rank_data,31,str_length(stock_rank_data))
stock_rank_data<-strsplit(stock_rank_data,"\n")

stock_rank_data<-stock_rank_data[[1]][-c(1:9)]

for (i in c(2,4,5,6,7,8)) {
  stock_df[,i]<-stock_rank_data[seq(i,171+i,9)]  
}
stock_df<-stock_df[-c(1,3)]

for (i in 1:20) {
  for (j in 1:6) {
    stock_df[i,j]<-str_replace_all(stock_df[i,j]," ","")
  }
}


for (i in 1:20) {
  for (j in 2:5) {
    stock_df[i,j]<-substring(stock_df[i,j],2,str_length(stock_df[i,j]))
  }
}


for (i in 1:20) {
  stock_df[i,1]<-(paste(substring(stock_df[i,1],4,5),substring(stock_df[i,1],1,2),substring(stock_df[i,1],7,10),sep=""))
}


for (i in 1:20) {
  stock_df[i,6]<-as.numeric(str_replace_all(stock_df[i,6],",",""))
}

for (i in 2:6) {
  stock_df[,i]<-as.numeric(stock_df[,i])
  
}
return(stock_df)
}