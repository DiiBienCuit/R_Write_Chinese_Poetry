library(jiebaR)
library(wordcloud2)

fileName <- "唐詩三百首.txt" 

SC <- readChar(fileName, file.info(fileName)$size)

#读取中文不出现乱码
Encoding(SC) <-  "UTF-8"

substr(SC, 1000, 1100)

#分词
mixseg = worker()

analysis_2 <- as.data.frame(table(mixseg[SC]))

#clean alphabet values 



# 简单改变一下文件的命名、格式
names(analysis_2) <- c("word","freq")
analysis_2$word <- as.character(analysis_2$word)
# 重新排序
analysis_2 <- analysis_2[order(-analysis_2$freq),]
# 看一下这个分词文件的开头


#wordcloud2(analysis_2)

#wordcloud2(analysis_2[analysis_2$freq>1& analysis_2$freq < 50 & nchar(analysis_2$word) == 1,])
#wordcloud2(analysis_2[analysis_2$freq>1& analysis_2$freq < 13 & nchar(analysis_2$word) == 2,])
#wordcloud2(analysis_2[analysis_2$freq>1 & analysis_2$freq < 4 & nchar(analysis_2$word) == 3,])

cipai <- "昨夜裙帶解，今朝蟢子飛。鉛華不可棄，莫是藁砧歸。"
tagger <- worker("tag")
cipai_2 <- tagger <= cipai
cipai_2

example <- subset(analysis_2, freq >1 & nchar(word) <4 & freq < 50)

# 提取词性文件
cixing <- attributes(cipai_2)$names

# 将素材库进行词性分类
example_2 <- tagger <= example$word

write_tangshi <- function(m){
  set.seed(m)
  empty <- ""
  for (i in 1:length(cipai_2)){
    #get the first chr who has the same type as the original one
    temp_file <- example_2[attributes(example_2)$name == cixing[i]]
    
    #who gets the same nomber of chrs as the original one
    temp_file <- temp_file[nchar(temp_file) == nchar(cipai_2[i])] 
    empty <- paste0(empty, sample(temp_file,1))
  }
  
  result <- paste0(substr(empty, 1,5), ",", substr(empty,6,10),"。", 
                   substr(empty, 11,15), ",", substr(empty, 16,20),"。")
  
}

index = c(1:10)
lapply(index, write_tangshi)