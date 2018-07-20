library(httr)
library(xml2)
library(pipeR)
library(stringi)
library(stringr)
library(rvest)
library(dplyr)
library(urlshorteneR)
library(WriteXLS)
library(mailR)
library(readr)

#爬新聞內容#####
# url
##tbs=qdr:d最近一天(tbs=qdr:w最近一週)(,sbd:1 排序根據最近日期 ;cr=countryTW 台灣)
keywords <- paste0("數位金融OR行動支付OR第三方支付OR比特幣OR區塊鏈OR金融科技")
url <- paste0("https://www.google.com.tw/search?q=",keywords,"&source=lnms&tbm=nws&tbs=qdr:w&cr=countryTW&num=100&start=0")

# parse網頁
html_nodex <- GET(url) %>>% content
# 爬新聞url
googlenews_url <- html_nodex %>>% xml_find_all("//h3[@class='r']//a") %>% xml_attr("href")
googlenews_url <- paste0("https://www.google.com.tw",googlenews_url)

# 爬新聞title
googlenews_title <- html_nodex %>>% xml_find_all("//h3[@class='r']//a") %>% xml_text
# 爬新聞time&from
googlenews_time <- html_nodex %>>% xml_find_all("//div[@class='slp']") %>% xml_text
yoyo <- t(as.data.frame(strsplit(googlenews_time,"-")))
row.names(yoyo) <- NULL
colnames(yoyo) <- c("from","time")
###各大報社#####
#1 聯合財經網
googlenews_udn1_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "聯合財經網 "])){
  tryCatch({
  googlenews_udn1_url <- googlenews_url[yoyo[,"from"] == "聯合財經網 "][i]
  googlenews_content <- GET(googlenews_udn1_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@id='story_body_content']//p") %>% xml_text
  googlenews_content <- googlenews_content[-1]
  googlenews_content <- str_replace_all(googlenews_content,"\n","")
  googlenews_content <- str_replace_all(googlenews_content,"\r","")
  googlenews_content <- str_replace_all(googlenews_content,"\t","")
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_udn1_content <- c(googlenews_udn1_content,googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#2 聯合新聞網
googlenews_udn2_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "udn 聯合新聞網 "])){
  tryCatch({
  googlenews_udn2_url <- googlenews_url[yoyo[,"from"] == "udn 聯合新聞網 "][i]
  googlenews_content <- GET(googlenews_udn2_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@id='story_body_content']//p") %>% xml_text
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_udn2_content <- c(googlenews_udn2_content,googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
#3 中時電子報
googlenews_chn_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "中時電子報 (新聞發布) "])){
  tryCatch({
  googlenews_chn_url <- googlenews_url[yoyo[,"from"] == "中時電子報 (新聞發布) "][i]
  googlenews_content <- GET(googlenews_chn_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//article[@class='clear-fix']//p") %>% xml_text
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_chn_content <- c(googlenews_chn_content,googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  # print(paste0("chn",i))
}

#4 蘋果日報
googlenews_apple_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "蘋果日報 "])){
  tryCatch({
  googlenews_apple_url <- googlenews_url[yoyo[,"from"] == "蘋果日報 "][i]
  googlenews_content <- GET(googlenews_apple_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//p[@id='summary']") %>% xml_text
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_apple_content <- c(googlenews_apple_content,googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#5 Money DJ理財網
googlenews_moneydj_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "MoneyDJ理財網 "])){
  tryCatch({
  googlenews_moneydj_url <- googlenews_url[yoyo[,"from"] == "MoneyDJ理財網 "][i]
  googlenews_content <- GET(googlenews_moneydj_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@class='wikilink']//p") %>% xml_text
  googlenews_content <- googlenews_content[-1]
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_moneydj_content <- c(googlenews_moneydj_content,googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
#6 ET NEWS
googlenews_etnews_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "ETNEWS "])){
  tryCatch({
  googlenews_etnews_url <- googlenews_url[yoyo[,"from"] == "ETNEWS "][i]
  googlenews_content <- GET(googlenews_etnews_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@class='story']//p") %>% xml_text
  googlenews_content <- googlenews_content[-c(1:3,length(googlenews_content))]
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_etnews_content <- c(googlenews_etnews_content, googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#7 臺灣新浪網
# googlenews_sina_content <- c()
# for (i in 1:length(googlenews_url[yoyo[,"from"] == "臺灣新浪網 "])){
#   tryCatch({
#   googlenews_sina_url <- googlenews_url[yoyo[,"from"] == "臺灣新浪網 "][i]
#   googlenews_content <- GET(googlenews_sina_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@class='pcont']") %>% xml_text
#   googlenews_content <- str_replace_all(googlenews_content,"\n","")
#   googlenews_content <- str_replace_all(googlenews_content,"\t","")
#   googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
#   googlenews_sina_content <- c(googlenews_sina_content, googlenews_content)
#   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# }

#8 數位時代
googlenews_bnext_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "數位時代 (新聞發布) "])){
  tryCatch({
  googlenews_bnext_url <- googlenews_url[yoyo[,"from"] == "數位時代 (新聞發布) "][i]
  googlenews_content <- GET(googlenews_bnext_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//article[@class='main_content']//p") %>% xml_text
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_bnext_content <- c(googlenews_bnext_content, googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#9 ithome
googlenews_ithome_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "iThome Online "])){
  tryCatch({
  googlenews_ithome_url <- googlenews_url[yoyo[,"from"] == "iThome Online "][i]
  googlenews_content <- GET(googlenews_ithome_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@class='contents-wrap']//p") %>% xml_text
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_ithome_content <- c(googlenews_ithome_content, googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# 10 鉅亨網
# googlenews_cnyes_content <- c()
# for (i in 1:length(googlenews_url[yoyo[,"from"] == "鉅亨網 "])){
#   tryCatch({
#   googlenews_cnyes_url <- googlenews_url[yoyo[,"from"] == "鉅亨網 "][i]
#   googlenews_content <- GET(googlenews_cnyes_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//article[@class='_1mD theme-article']//p") %>% xml_text
#   googlenews_content <- googlenews_content[-length(googlenews_content)]
#   googlenews_content <- str_replace_all(googlenews_content,"\n","")
#   googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
#   googlenews_cnyes_content <- c(googlenews_cnyes_content, googlenews_content)
#   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# }

#11 科技新報
googlenews_technews_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "科技新報 TechNews "])){
  tryCatch({
  googlenews_technews_url <- googlenews_url[yoyo[,"from"] == "科技新報 TechNews "][i]
  googlenews_content <- GET(googlenews_technews_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@class='indent']//p") %>% xml_text
  googlenews_content <- googlenews_content[-length(googlenews_content)]
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_technews_content <- c(googlenews_technews_content, googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#12 自由時報電子報
googlenews_ltn_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "自由時報電子報 "])){
  tryCatch({
  googlenews_ltn_url <- googlenews_url[yoyo[,"from"] == "自由時報電子報 "][i]
  googlenews_content <- GET(googlenews_ltn_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@class='text']//p") %>% xml_text
  googlenews_content <- googlenews_content[-length(googlenews_content)]
  googlenews_content <- googlenews_content[-length(googlenews_content)]
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_ltn_content <- c(googlenews_ltn_content, googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#13 The News Lens 關鍵評論網 
googlenews_lnl_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "The News Lens 關鍵評論網 "])){
  tryCatch({
  googlenews_lnl_url <- googlenews_url[yoyo[,"from"] == "The News Lens 關鍵評論網 "][i]
  googlenews_content <- GET(googlenews_lnl_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@class='article-content  AdAsia ']//p") %>% xml_text
  googlenews_content <- googlenews_content[-c(1,length(googlenews_content))]
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_lnl_content <- c(googlenews_lnl_content, googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#14 中天快點TV (新聞發布)
googlenews_ctn_content <- c()
for (i in 1:length(googlenews_url[yoyo[,"from"] == "中天快點TV (新聞發布) "])){
  tryCatch({
  googlenews_ctn_url <- googlenews_url[yoyo[,"from"] == "中天快點TV (新聞發布) "][i]
  googlenews_content <- GET(googlenews_ctn_url) %>>% content(encoding = "UTF-8") %>>% xml_find_all("//div[@class='td-post-content td-pb-padding-side']//p") %>% xml_text
  googlenews_content <- googlenews_content[-length(googlenews_content)]
  googlenews_content <- paste(googlenews_content, sep = "", collapse = "")
  googlenews_ctn_content <- c(googlenews_ctn_content, googlenews_content)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# 整合 ####
news_test <- data.frame(title     = googlenews_title,
                          url     = googlenews_url,
                          from    = yoyo[,"from"],
                          time    = yoyo[,"time"])

news_test$content[yoyo[,"from"] == "聯合財經網 "] <- googlenews_udn1_content
news_test$content[yoyo[,"from"] == "udn 聯合新聞網 "] <- googlenews_udn2_content
news_test$content[yoyo[,"from"] == "中時電子報 (新聞發布) "] <- googlenews_chn_content
news_test$content[yoyo[,"from"] == "蘋果日報 "] <- googlenews_apple_content
news_test$content[yoyo[,"from"] == "MoneyDJ理財網 "] <- googlenews_moneydj_content
news_test$content[yoyo[,"from"] == "ETNEWS "] <- googlenews_etnews_content
news_test$content[yoyo[,"from"] == "臺灣新浪網 "] <- NA
news_test$content[yoyo[,"from"] == "數位時代 (新聞發布) "] <- googlenews_bnext_content
news_test$content[yoyo[,"from"] == "iThome Online "] <- googlenews_ithome_content
news_test$content[yoyo[,"from"] == "鉅亨網 "] <- NA
news_test$content[yoyo[,"from"] == "科技新報 TechNews "] <- googlenews_technews_content
news_test$content[yoyo[,"from"] == "自由時報電子報 "] <- googlenews_ltn_content
news_test$content[yoyo[,"from"] == "The News Lens 關鍵評論網 "] <- googlenews_lnl_content
news_test$content[yoyo[,"from"] == "中天快點TV (新聞發布) "] <- NA

#確認資料型態
news_test$title <- as.character(news_test$title)
news_test$url <- as.character(news_test$url)
news_test$from <- as.character(news_test$from)
news_test$time <- as.character(news_test$time)
news_test$content <- as.character(news_test$content)

#關鍵字選新聞
news_test <- news_test[c(
  grep("數位",news_test$title),
  grep("支付",news_test$title),
  grep("AI",news_test$title),
  grep("Fintech",news_test$title),
  grep("Bitcoin",news_test$title),
  grep("比特幣",news_test$title),
  grep("NFC",news_test$title),
  grep("區塊鏈",news_test$title),
  grep("Blockchain",news_test$title)
),]

# 篩有值內容 隨機抽6則
news_test <- sample_n(news_test[!is.na(news_test$content) & news_test$content != "" & nchar(news_test$content)<3000,],6)
row.names(news_test) <- NULL


##縮網址
googl_token <- googl_auth(key = "1099455161745-of85u72048jpcgm8qdjelnveaubo794t.apps.googleusercontent.com",
                          secret = "L14D74y1zdVcxp4T42Cgz4Zh")
shorturl_all <- c()
for (i in 1:nrow(news_test)){
  shorturl <- isgd_LinksShorten(news_test$url[i])
  shorturl_all <- c(shorturl_all,shorturl)
  print(i)
}
news_test$url <- shorturl_all


#輸出結果
setwd("/Users/chenchingchun/Desktop")
WriteXLS(news_test, paste0(Sys.Date(),"news_test.xlsx"),AutoFilter = F,AdjWidth = T)
#休息一下
Sys.sleep(1)
#寄信
sender <- "j555338017@gmail.com"
recipients <- c("104354022@nccu.edu.tw")
send.mail(from = sender,
          to = recipients,
          subject = paste0(Sys.time(),"Digital News"),
          body = "Good Morning, Johnny!",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "youremail@gmail.com",            
                      passwd = "xxxxx", ssl = TRUE),
          authenticate = TRUE,
          html = TRUE,
          encoding = "utf-8",
          send = TRUE,
          attach.files = paste0("/Users/chenchingchun/Desktop/",Sys.Date(),"news_test.xlsx")
)
# quit(save='no')
# # 看各家報社比例
# sort(table(yoyo[,1]), decreasing = T)



