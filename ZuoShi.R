library(jiebaR)
library(wordcloud2)

fileName <- "宋詞三百首.txt" 

SC <- readChar(fileName, file.info(fileName)$size)

#读取中文不出现乱码
Encoding(SC) <-  "UTF-8"

substr(SC, 1000, 1100)

#分词
mixseg = worker()

analysis <- as.data.frame(table(mixseg[SC]))


#clean alphabet values 
indice <- c(1:13)
analysis_2 <- analysis[-indice,]
# 重新排序
analysis_2 <- analysis_2[order(-analysis_2$freq),]

# 简单改变一下文件的命名、格式
names(analysis) <- c("word","freq")
analysis$word <- as.character(analysis$word)

# 看一下这个分词文件的开头


#wordcloud2(analysis_2)

#wordcloud2(analysis[analysis$freq>1& analysis$freq < 300 & nchar(analysis$word) == 1,])
#wordcloud2(analysis[analysis$freq>1& analysis$freq < 300 & nchar(analysis$word) == 2,])
#wordcloud2(analysis[analysis$freq>1 & analysis$freq < 300 & nchar(analysis$word) == 3,])

cipai <- "画堂晨起，来报雪花坠。高卷帘栊 看 佳瑞，皓色远 迷 庭砌。盛气光引 炉烟，素草寒生玉佩。应是天仙狂醉，乱把白云揉碎。"
tagger <- worker("tag")
cipai_2 <- tagger <= cipai
cipai_2

example <- subset(analysis_2, freq >1 & nchar(word) <3 & freq < 300)

# 提取词性文件
cixing <- attributes(cipai_2)$names

# 将素材库进行词性分类
example_2 <- tagger <= example$word

write_songci <- function(m){
  set.seed(m)
  empty <- ""
  for (i in 1:length(cipai_2)){
    #get the first chr who has the same type as the original one
    temp_file <- example_2[attributes(example_2)$name == cixing[i]]
    
    #who gets the same nomber of chrs as the original one
    temp_file <- temp_file[nchar(temp_file) == nchar(cipai_2[i])] 
    empty <- paste0(empty, sample(temp_file,1))
  }
  
  result <- paste0(substr(empty, 1,4), ",", substr(empty,5,9),"。", 
                   substr(empty, 10,16), ",", substr(empty, 17,22),"。",
                   substr(empty, 23,28), ",", substr(empty, 29,34),"。",
                   substr(empty, 35,40), ",", substr(empty, 41,46),"。")
  
}

index = c(1:10)
lapply(index, write_songci)