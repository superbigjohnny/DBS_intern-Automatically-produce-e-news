library(RDCOMClient)
library(taskscheduleR)
library(readr)
library(urlshorteneR)
library(xlsx)

#Basic 寄信######
OutApp <- COMCreate("Outlook.Application")  
outMail = OutApp$CreateItem(0) 

Sys.setlocale("LC_TIME", "English")
outMail[["To"]] = "chingchunchen@1bank.dbs.com" 
outMail[["subject"]] = paste0("Innovation & Digital Banking News - ",format(Sys.Date(), "%b. %d, %Y")) 

#讀樣本
eb <- read_lines("C:/Users/chingchunchen/Desktop/email.txt", n_max = -1L)
#更新本期DBS Digital Banking News市場動態(5則內容)
##本期內容
latest_news <- read.xlsx("C:/Users/chingchunchen/Downloads/news_test.xlsx",stringsAsFactors = F, sheetIndex = 1, encoding = "UTF-8")

##更新本期內容標題
eb[seq(732,741,2)] <- latest_news$title


###第一則新聞####
##新聞標題
eb[746]        <- latest_news$title[1]
##新聞內容
eb[749]      <- latest_news$content[1]
##新聞連結
eb[c(751,753)]   <- latest_news$url[1]

###第二則新聞####
##新聞標題
eb[756]        <- latest_news$title[2]
##新聞內容
eb[759]      <- latest_news$content[2]
##新聞連結
eb[c(761,763)]   <- latest_news$url[2]
###第三則新聞####
##新聞標題
eb[766]        <- latest_news$title[3]
##新聞內容
eb[769]      <- latest_news$content[3]
##新聞連結
eb[c(771,773)]   <- latest_news$url[3]
###第四則新聞####
eb[775]        <- latest_news$title[4]
##新聞內容
eb[785]      <- latest_news$content[4]
##新聞連結
eb[c(789,791)]   <- latest_news$url[4]
###第五則新聞####
eb[780]        <- latest_news$title[5]
##新聞內容
eb[793]      <- latest_news$content[5]
##新聞連結
eb[c(798,800)]   <- latest_news$url[5]

#把空白接起來####
eb2 <- paste(eb, sep = "", collapse = "")
#fucking轉馬
eb2 <- iconv(eb2, "UTF-8", "BIG-5")
outMail[["HTMLbody"]] = eb2

Encoding(eb2)


#寄出去
outMail$Display()

# outMail$Send()



