library(rvest) 
library(httr) 
library(KoNLP)
library(stringr)
library(tm)
library(rvest)
library(wordcloud)
useNIADic()

data = NULL
for(i in 1:10){
  url = 'https://search.daum.net/search?w=blog&f=section&SA=daumsec&lpp=10&nil_src=blog&q=%EB%B4%84%EC%97%AC%ED%96%89&DA=PGD&p='
  url <- paste0(url,i)
  response = GET(url)
  htxt = read_html(response) 
  comments = html_nodes(htxt, 'div.cont_inner') 
  
  data <- paste(data,html_text(comments))
  data <- paste(data,"\n\n")
}
write(data,"º½¿©Çà0.txt") 
data1 = readLines("º½¿©Çà0.txt")
data2 = sapply(data1, extractNoun, USE.NAMES=F)
data3 = unlist(data2)
data4 = Filter(function(x) {nchar(x)>=2},data3)

data5 = gsub("[0-9]","",data4)
data6 = gsub('[A-z]',"",data5)
data7 = gsub('[[:punct:]]',"",data6)
write(unlist(data7),"º½¿©Çà1.txt")

data8 = readLines("º½¿©Çà1.txt")
data9 = extractNoun(data8)
data10 = as.character(data9)
lp1 <- paste(SimplePos09(data10))

np1 <- str_match_all(lp1,'([°¡-ÆR]+)/[NP]')
np1
length(NP)
foo <- c()
alldata = c()
for(k in 1:length(np1)){
  foo <- np1[[k]][,2]
  alldata = c(alldata,foo)
  print(foo)
}
dataA = Filter(function(x){nchar(x)>=2},alldata)
dataA

######################################################################
dataB = dataA
tolower(dataB)
stripWhitespace(dataB)
removePunctuation (dataB)
removeNumbers(dataB)

dataC <- Corpus(VectorSource(dataB)) 
wordC <- function(dataC){
  dataC <- as.character(dataC)
  extractNoun(dataC)
  str_replace_all(dataC,"[^[:graph:]]", "") 
}

tdm <- TermDocumentMatrix(dataC,
                          control=list(
                            tokenize=words,
                            removeNumbers=T,
                            removePunctuation=T,
                            wordLengths=c(1, 10),
                            stopwords = c("¿©Çà","uaua","º½¿©Çà","¿©ÇàÀ»","ºí·Î±×","º½À»","³»","º½","º½ÀÇ","ÀÌÁ¦","Å¸°í","ÃßÃµ","¾ÆÁ÷","»çÁøÀ¸·Î","´Ù³à¿Ô´Ù","ÀúÀÛÀÚÇ¥½Ã","º½¹Ù¶÷","ÀÌ¹ø","¿¹»Û","È­¿äÀÏ","°¡º¼¸¸ÇÑ°÷","µû¶æÇÔ°ú","¼Ò°³ÇØ","ÀÖ³×¿ä","°Ë»ö","ÇÔ²²","µî","¿ù","¸Å³â","¾Õ¿¡¼­","¶°³³´Ï´Ù","À§ÇÏ¿©","Á¦ÀÏ","Áö³­","³ªµéÀÌ","ÀÖ´Â","³â","Ã£¾Æ","°¡Àå","°ü±¤","Ã¹","º¸´Ï","ÀÏ","uf","ÁÁÀº","¶°³ª´Â","±×","¾Æ¸§´Ù¿î","º¯°æÇã¶ô","º½¿©ÇàÁÖ°£","¸ÂÀÌ","¼ö","º½ÀÌ","¶°³­","°¡´Â","¿ì¸®","ÀÌÀ¯","°Í","¼¶","½ÍÀº","È°Â¦","ÀÏÂ÷","¿Â","¾Ë°í","¿À´ÃÀº","ÇÏ°í","ÇÑ","º½¹Ù","¾ÆÁÜ","Á¤¸®","ÄÉÀÌ","È°¼º","¼ö¿ä","±×·¹°í","ºí·Î","¶óÀÌÇÁ","·¹Æ®·Î","½ºÅä","°ü±¤","½ÅÈ¥","ÇÑ±¹","ÀÏ¿ä","´ë±¸ÆÔ","¿ù¿ä")
                          ))

tdm2 <- as.matrix(tdm) 
tdm2
tdm3 = rowSums(tdm2)
tdm3
tdm4 = tdm3[order(tdm3, decreasing = T)]
tdm4
class(tdm4)
as.data.frame(tdm4)
as.data.frame(tdm4[7:20])
wordTdm= as.data.frame(tdm4)
wordTdm
set.seed(1234)
display.brewer.all()
wordcloud(words = rownames(wordTdm), freq = wordTdm$tdm4, min.freq = 2,
          random.order=FALSE, rot.per=0.35,max.words = 100,colors=brewer.pal(10, "RdBu"))

