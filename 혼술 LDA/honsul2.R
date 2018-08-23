#Packages
packages = c("Rfacebook", "tm", "lsa", "wordcloud","ggplot2","KoNLP",
             "GPArotation","cluster","RWeka","ROAuth","fpc","stringr",
             "ape","devtools","rJava","readxl","dplyr")

for (i in packages){
  if(!require( i , character.only = TRUE))
  {install.packages(i, dependencies = TRUE)}
}
install_github("youngwoos/kospacing")

#Setting
pdf.options(family="Korea1deb") #not to tear down the letters
options(java.parameters=c("-Xmx8g","-Dfile.encoding=UTF-8")) #to increse heap size of rjava
pal <- brewer.pal(9,"Set1")
options(mc.cores=1)

useNIADic()

#Processing
##File Load
f1 <- read_excel("instagram_soldrink_1.xlsx")
f1_txt <- f1$TEXT
f1_txt[is.na(f1_txt)] <- " "
f1_tag <- f1$TAGS

##Paste tags and text
f1_corp <- paste(f1_txt,f1_tag)

##Remove #, punctuation, space
tag_remove <- function(x){
  x <- unlist(x)
  x <- gsub("[[:punct:]]"," ",x)
  x <- gsub("\\s+"," ",x)
}
f1_corp <- tag_remove(f1_corp)

##Abstract NC
words <- function(doc){ 
  
  doc <- as.character(doc)
  
  doc2 <- paste(SimplePos22(doc))
  
  doc3 <- str_match(doc2, "([가-힣]+)/NC")
  
  if( dim(doc3)[2] == 2){
    doc4 <- doc3[,2]
    doc4 <- doc4[!is.na(doc4)]
    return(doc4)
  }
}
nouns = sapply(f1_corp,words, USE.NAMES = F)
nouns1 <- nouns

x <- subset(nouns,length(nouns)==0)

##Remove Charater Length = 0 (아직 완벽한지 확인이 안 됨)
for (i in 1:length(nouns1)) {
  if (length(nouns1[[i]]) == 0) {
    nouns1[[i]] <- NULL
  } else {
    nouns1[[i]] <- nouns1[[i]]
  }
}

##Top 100 words (아직 진행 중)
nouns1 <- Corpus(VectorSource(nouns1))
nouns1 <- tm_map(nouns1, removeNumbers)
nouns1 <- tm_map(nouns1, removeWords, words)
