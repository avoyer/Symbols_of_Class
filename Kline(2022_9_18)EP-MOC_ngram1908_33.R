
##  EP(Emily Post): CMDist analysis
# File Name:Kline(2022_9_18)EP-CMD
# Date:     2022_9_18
# Who:      Zachary D. Kline

# Paper: Symbols of class: A computational analysis of class distinction-making through etiquette, 1922 -2017"
# by Andrea Voyer, Zachary D. Kline, and Madison Danton

##  To do before running this script in RStudio:
#   1) copy txt from git-hub working directory to a folder in your R working directory
#             titled "txt_2022_3_28".
#     Note 1: Text comes from 2022-3-28. All changes to the corpus made
#                 after this date are excluded from analysis.
#     Note 2: Workflow follows format presented by Julia Silge and David Robinson: Tidy Text
#       https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html

#     1  Set Working Directory
#UPDATE WITH YOUR DIRECTORY!
setwd("C:/Users/zdk15001/Desktop/Academia/University_of_Connecticut/Emily_Post/Work/Analysis/")
wd <- "C:/Users/zdk15001/Desktop/Academia/University_of_Connecticut/Emily_Post/Work/Analysis/"

#     2  Install and Load Packages (if not using remote server, you may only need to do this once)
#install.packages("tidytext")
#install.packages("tidyverse")
#install Rtools from https://cran.r-project.org/bin/windows/Rtools/
# Run function CMDist from Stoltz(2019_11_11)Concept_movers_distance_git
#install.packages("pacman")
#install.packages("devtools")
#install.packages('BiocManager')
#install.packages('stringr')
#install.packages('backports')
#devtools::install_github("UrbanInstitute/urbnthemes")
#devtools::install_github("statsmaths/fasttextM")           
#devtools::install_github("lionel-/ggstance")
#devtools::install_github("quanteda/quanteda.corpora")
#devtools::install_github("dustinstoltz/CMDist")
#devtools::install_github("mukul13/rword2vec")

#   Load in packages
library(dplyr)
library(backports)
library(stringr)
library(tidytext)
library(tidyverse)
library(tokenizers)
library(tidyr)
library(ggplot2)
library(devtools)
library(pacman)
library(BiocManager)
library(CMDist)
library(rtext2vec)##
library(fasttextM)
library(urbnthemes)##

pacman::p_load(text2vec,fasttextM,
               tm, Matrix,tidytext,
               textstem,dplyr,
               tidyverse,textclean,
               quanteda.corpora,
               stringr,extrafont,
               ggplot2,ggrepel,
               gutenbergr,ggstance,
               ggpubr,GGally,
               install=TRUE)


set_urbn_defaults()


# only need to run once per directory; gathers word embeddings from fasttextM
#ft_download_model("en", mb = 3000, location = wd)

ngram1908_33 <- readRDS("ngram1908_33.Rds")
rownames(ngram1908_33) <- tolower(rownames(ngram1908_33))

# load word embeddings from fasttexM
#ft_load_model("en", location = wd)

#     3  Load in and combine text into "source_EP"

# Folder where editions.txt are stored (Change as updated)
path2file <- "txt_2022_3_28"

# Create a list of editions
fileList <- list.files(path2file, full.names = TRUE) # Create a list of files in a folder

# Function to read in multiple texts and paste them into a tbl 


readTextFiles <- function(file) { 
  message(file)
  rawText = paste(scan(file, sep="\n",what="raw",strip.white = TRUE))
  output = tibble(filename=gsub("data/sample_texts/","",file),text=rawText) %>% 
    group_by(filename) %>% 
    summarise(text = paste(rawText, collapse = " "))
  return(output)
}

# Run the function to create a tbl of combined files

source_EP <- tibble(filename=fileList) %>% 
  group_by(filename) %>% 
  do(readTextFiles(.$filename))  %>% 
  ungroup()

#   4 Create tidy data set

tidy_EP <- source_EP %>% 
  mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = replace_white(text)) %>%
  mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
  mutate(text = replace_number(text))  %>%
  unnest_tokens(word, text, to_lower = TRUE) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[0-9]+") ) 

#   5 Rename filename 

tidy_EP <- mutate(tidy_EP, filename =  
           ifelse(grepl("txt_2022_3_28/1922EditionFull.txt", filename), "1922",
           ifelse(grepl("txt_2022_3_28/1927EditionFull.txt", filename), "1927",
           ifelse(grepl("txt_2022_3_28/1931EditionFull.txt", filename), "1931",
           ifelse(grepl("txt_2022_3_28/1934EditionFull.txt", filename), "1934",
           ifelse(grepl("txt_2022_3_28/1937EditionFull.txt", filename), "1937",
           ifelse(grepl("txt_2022_3_28/1940EditionFull.txt", filename), "1940",
           ifelse(grepl("txt_2022_3_28/1942EditionFull.txt", filename), "1942",
           ifelse(grepl("txt_2022_3_28/1945EditionFull.txt", filename), "1945",
           ifelse(grepl("txt_2022_3_28/1950EditionFull.txt", filename), "1950",
           ifelse(grepl("txt_2022_3_28/1956EditionFull.txt", filename), "1956",
           ifelse(grepl("txt_2022_3_28/1960EditionFull.txt", filename), "1960",
           ifelse(grepl("txt_2022_3_28/1965EditionFull.txt", filename), "1965",
           ifelse(grepl("txt_2022_3_28/1969EditionFull.txt", filename), "1969",
           ifelse(grepl("txt_2022_3_28/1975EditionFull.txt", filename), "1975",
           ifelse(grepl("txt_2022_3_28/1984EditionFull.txt", filename), "1984",
           ifelse(grepl("txt_2022_3_28/1992EditionFull.txt", filename), "1992",
           ifelse(grepl("txt_2022_3_28/1997EditionFull.txt", filename), "1997",
           ifelse(grepl("txt_2022_3_28/2004EditionFull.txt", filename), "2004",
           ifelse(grepl("txt_2022_3_28/2011EditionFull.txt", filename), "2011",
           ifelse(grepl("txt_2022_3_28/2017EditionFull.txt", filename), "2017","x")))))))))))))))))))))


#   6 determine chunk size

tidy_EP_chunk_n <-   mutate(tidy_EP, id = as.numeric(rownames(tidy_EP)))

write.table(tidy_EP_chunk_n, "kline(2022_5_17)EP-MOC_chunkn_ngram1908_33.csv", row.names = FALSE)



tidy_EP_chunk_n <- tidy_EP_chunk_n  %>% 
  count(filename)           %>%
  mutate(chunk_n = n / 69)




# Chunk size per edition
### MUST CHANGE WITH CORPUS CHANGE ### Round up and add 1
# 1922: 1043.214
c1922 <- 1060
c1927 <- 1426
c1931 <- 1512
c1934 <- 1465
c1937 <- 1791
c1940 <- 1839
c1942 <- 2066
c1945 <- 1542
c1950 <- 1514
c1956 <- 1656
c1960 <- 1630
c1965 <- 1562
c1969 <- 1472
c1975 <- 2095
c1984 <- 2007
c1992 <- 1851
c1997 <- 1883
c2004 <- 2159
c2011 <- 1959
c2017 <- 1909


#   7 Create ID variables for each row

#1922
tidy_EP_1922 <- tidy_EP %>%
  filter(filename == "1922")

tidy_EP_1922 <- tidy_EP_1922 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1922))) %>%
  mutate(chunkx = as.numeric(id)/c1922 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1922*1000) %>%
  mutate(year = 1922)
  

#1927
tidy_EP_1927 <- tidy_EP %>%
  filter(filename == "1927")

tidy_EP_1927 <- tidy_EP_1927 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1927))) %>%
  mutate(chunkx = as.numeric(id)/c1927 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1927*1000) %>%
  mutate(year = 1927)

#1931
tidy_EP_1931 <- tidy_EP %>%
  filter(filename == "1931")

tidy_EP_1931 <- tidy_EP_1931 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1931))) %>%
  mutate(chunkx = as.numeric(id)/c1931 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1931*1000) %>%
  mutate(year = 1931)

#1934
tidy_EP_1934 <- tidy_EP %>%
  filter(filename == "1934")

tidy_EP_1934 <- tidy_EP_1934 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1934))) %>%
  mutate(chunkx = as.numeric(id)/c1934 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1934*1000) %>%
  mutate(year = 1934)

#1937
tidy_EP_1937 <- tidy_EP %>%
  filter(filename == "1937")

tidy_EP_1937 <- tidy_EP_1937 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1937))) %>%
  mutate(chunkx = as.numeric(id)/c1937 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 ))  %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1937*1000) %>%
  mutate(year = 1937)

#1940
tidy_EP_1940 <- tidy_EP %>%
  filter(filename == "1940")

tidy_EP_1940 <- tidy_EP_1940 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1940))) %>%
  mutate(chunkx = as.numeric(id)/c1940 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1940*1000) %>%
  mutate(year = 1940)

#1942
tidy_EP_1942 <- tidy_EP %>%
  filter(filename == "1942")

tidy_EP_1942 <- tidy_EP_1942 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1942))) %>%
  mutate(chunkx = as.numeric(id)/c1942 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1942*1000) %>%
  mutate(year = 1942)

#1945
tidy_EP_1945 <- tidy_EP %>%
  filter(filename == "1945")

tidy_EP_1945 <- tidy_EP_1945 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1945))) %>%
  mutate(chunkx = as.numeric(id)/c1945 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1945*1000) %>%
  mutate(year = 1945)

#1950
tidy_EP_1950 <- tidy_EP %>%
  filter(filename == "1950")

tidy_EP_1950 <- tidy_EP_1950 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1950))) %>%
  mutate(chunkx = as.numeric(id)/c1950 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1950*1000) %>%
  mutate(year = 1950)

#1956
tidy_EP_1956 <- tidy_EP %>%
  filter(filename == "1956")

tidy_EP_1956 <- tidy_EP_1956 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1956))) %>%
  mutate(chunkx = as.numeric(id)/c1956 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1956*1000) %>%
  mutate(year = 1956)


#1960
tidy_EP_1960 <- tidy_EP %>%
  filter(filename == "1960")

tidy_EP_1960 <- tidy_EP_1960 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1960))) %>%
  mutate(chunkx = as.numeric(id)/c1960 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1960*1000) %>%
  mutate(year = 1960)


#1965
tidy_EP_1965 <- tidy_EP %>%
  filter(filename == "1965")

tidy_EP_1965 <- tidy_EP_1965 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1965))) %>%
  mutate(chunkx = as.numeric(id)/c1965 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 ))  %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1965*1000) %>%
  mutate(year = 1965)


#1969
tidy_EP_1969 <- tidy_EP %>%
  filter(filename == "1969")

tidy_EP_1969 <- tidy_EP_1969 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1969))) %>%
  mutate(chunkx = as.numeric(id)/c1969 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 ))  %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1969*1000) %>%
  mutate(year = 1969)


#1975
tidy_EP_1975 <- tidy_EP %>%
  filter(filename == "1975")

tidy_EP_1975 <- tidy_EP_1975 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1975))) %>%
  mutate(chunkx = as.numeric(id)/c1975 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 ))  %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1975*1000) %>%
  mutate(year = 1975)


#1984
tidy_EP_1984 <- tidy_EP %>%
  filter(filename == "1984")

tidy_EP_1984 <- tidy_EP_1984 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1984))) %>%
  mutate(chunkx = as.numeric(id)/c1984 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1984*1000) %>%
  mutate(year = 1984)


#1992
tidy_EP_1992 <- tidy_EP %>%
  filter(filename == "1992")

tidy_EP_1992 <- tidy_EP_1992 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1992))) %>%
  mutate(chunkx = as.numeric(id)/c1992 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1992*1000) %>%
  mutate(year = 1992)


#1997
tidy_EP_1997 <- tidy_EP %>%
  filter(filename == "1997")

tidy_EP_1997 <- tidy_EP_1997 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1997))) %>%
  mutate(chunkx = as.numeric(id)/c1997 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1997*1000) %>%
  mutate(year = 1997)


#2004
tidy_EP_2004 <- tidy_EP %>%
  filter(filename == "2004")

tidy_EP_2004 <- tidy_EP_2004 %>%
  mutate(id = as.numeric(rownames(tidy_EP_2004))) %>%
  mutate(chunkx = as.numeric(id)/c2004 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 2004*1000) %>%
  mutate(year = 2004)


#2011
tidy_EP_2011 <- tidy_EP %>%
  filter(filename == "2011")

tidy_EP_2011 <- tidy_EP_2011 %>%
  mutate(id = as.numeric(rownames(tidy_EP_2011))) %>%
  mutate(chunkx = as.numeric(id)/c2011 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 2011*1000) %>%
  mutate(year = 2011)


#2017
tidy_EP_2017 <- tidy_EP %>%
  filter(filename == "2017")

tidy_EP_2017 <- tidy_EP_2017 %>%
  mutate(id = as.numeric(rownames(tidy_EP_2017))) %>%
  mutate(chunkx = as.numeric(id)/c2017 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 2017*1000) %>%
  mutate(year = 2017)

# full tidy_ep

tidy_EP_full <- tidy_EP



tidy_EP_full <- tidy_EP %>%
  mutate(id = as.numeric(rownames(tidy_EP))) %>%
  count(filename, word) %>%
  bind_tf_idf(word, filename, n)


##  combine sets

tidy_EP_chunks <- full_join(tidy_EP_1922, tidy_EP_1927)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1931)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1934)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1937)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1940)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1942)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1945)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1950)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1956)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1960)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1965)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1969)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1975)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1984)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1992)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1997)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_2004)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_2011)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_2017)



#   8 generate random chunks for iterations
#   14/71 is 20 percent


set.seed(1)
strap1 <- sample(1:70, 70, replace=TRUE)

set.seed(2)
strap2 <- sample(1:70, 70, replace=TRUE)

set.seed(3)
strap3 <- sample(1:70, 70, replace=TRUE)

set.seed(4)
strap4 <- sample(1:70, 70, replace=TRUE)

set.seed(5)
strap5 <- sample(1:70, 70, replace=TRUE)

set.seed(6)
strap6 <- sample(1:70, 70, replace=TRUE)

set.seed(7)
strap7 <- sample(1:70, 70, replace=TRUE)

set.seed(8)
strap8 <- sample(1:70, 70, replace=TRUE)

set.seed(9)
strap9 <- sample(1:70, 70, replace=TRUE)



set.seed(10)
strap10 <- sample(1:70, 70, replace=TRUE)

set.seed(11)
strap11 <- sample(1:70, 70, replace=TRUE)

set.seed(12)
strap12 <- sample(1:70, 70, replace=TRUE)

set.seed(13)
strap13 <- sample(1:70, 70, replace=TRUE)

set.seed(14)
strap14 <- sample(1:70, 70, replace=TRUE)

set.seed(15)
strap15 <- sample(1:70, 70, replace=TRUE)

set.seed(16)
strap16 <- sample(1:70, 70, replace=TRUE)

set.seed(17)
strap17 <- sample(1:70, 70, replace=TRUE)

set.seed(18)
strap18 <- sample(1:70, 70, replace=TRUE)

set.seed(19)
strap19 <- sample(1:70, 70, replace=TRUE)







set.seed(20)
strap20 <- sample(1:70, 70, replace=TRUE)

set.seed(21)
strap21 <- sample(1:70, 70, replace=TRUE)

set.seed(22)
strap22 <- sample(1:70, 70, replace=TRUE)

set.seed(23)
strap23 <- sample(1:70, 70, replace=TRUE)

set.seed(24)
strap24 <- sample(1:70, 70, replace=TRUE)

set.seed(25)
strap25 <- sample(1:70, 70, replace=TRUE)

set.seed(26)
strap26 <- sample(1:70, 70, replace=TRUE)

set.seed(27)
strap27 <- sample(1:70, 70, replace=TRUE)

set.seed(28)
strap28 <- sample(1:70, 70, replace=TRUE)

set.seed(29)
strap29 <- sample(1:70, 70, replace=TRUE)





set.seed(30)
strap30 <- sample(1:70, 70, replace=TRUE)

set.seed(31)
strap31 <- sample(1:70, 70, replace=TRUE)

set.seed(32)
strap32 <- sample(1:70, 70, replace=TRUE)

set.seed(33)
strap33 <- sample(1:70, 70, replace=TRUE)

set.seed(34)
strap34 <- sample(1:70, 70, replace=TRUE)

set.seed(35)
strap35 <- sample(1:70, 70, replace=TRUE)

set.seed(36)
strap36 <- sample(1:70, 70, replace=TRUE)

set.seed(37)
strap37 <- sample(1:70, 70, replace=TRUE)

set.seed(38)
strap38 <- sample(1:70, 70, replace=TRUE)

set.seed(39)
strap39 <- sample(1:70, 70, replace=TRUE)






set.seed(40)
strap40 <- sample(1:70, 70, replace=TRUE)

set.seed(41)
strap41 <- sample(1:70, 70, replace=TRUE)

set.seed(42)
strap42 <- sample(1:70, 70, replace=TRUE)

set.seed(43)
strap43 <- sample(1:70, 70, replace=TRUE)

set.seed(44)
strap44 <- sample(1:70, 70, replace=TRUE)

set.seed(45)
strap45 <- sample(1:70, 70, replace=TRUE)

set.seed(46)
strap46 <- sample(1:70, 70, replace=TRUE)

set.seed(47)
strap47 <- sample(1:70, 70, replace=TRUE)

set.seed(48)
strap48 <- sample(1:70, 70, replace=TRUE)

set.seed(49)
strap49 <- sample(1:70, 70, replace=TRUE)






set.seed(50)
strap50 <- sample(1:70, 70, replace=TRUE)

set.seed(51)
strap51 <- sample(1:70, 70, replace=TRUE)

set.seed(52)
strap52 <- sample(1:70, 70, replace=TRUE)

set.seed(53)
strap53 <- sample(1:70, 70, replace=TRUE)

set.seed(54)
strap54 <- sample(1:70, 70, replace=TRUE)

set.seed(55)
strap55 <- sample(1:70, 70, replace=TRUE)

set.seed(56)
strap56 <- sample(1:70, 70, replace=TRUE)

set.seed(57)
strap57 <- sample(1:70, 70, replace=TRUE)

set.seed(58)
strap58 <- sample(1:70, 70, replace=TRUE)

set.seed(59)
strap59 <- sample(1:70, 70, replace=TRUE)






set.seed(60)
strap60 <- sample(1:70, 70, replace=TRUE)

set.seed(61)
strap61 <- sample(1:70, 70, replace=TRUE)

set.seed(62)
strap62 <- sample(1:70, 70, replace=TRUE)

set.seed(63)
strap63 <- sample(1:70, 70, replace=TRUE)

set.seed(64)
strap64 <- sample(1:70, 70, replace=TRUE)

set.seed(65)
strap65 <- sample(1:70, 70, replace=TRUE)

set.seed(66)
strap66 <- sample(1:70, 70, replace=TRUE)

set.seed(67)
strap67 <- sample(1:70, 70, replace=TRUE)

set.seed(68)
strap68 <- sample(1:70, 70, replace=TRUE)

set.seed(69)
strap69 <- sample(1:70, 70, replace=TRUE)





set.seed(70)
strap70 <- sample(1:70, 70, replace=TRUE)

set.seed(71)
strap71 <- sample(1:70, 70, replace=TRUE)

set.seed(72)
strap72 <- sample(1:70, 70, replace=TRUE)

set.seed(73)
strap73 <- sample(1:70, 70, replace=TRUE)

set.seed(74)
strap74 <- sample(1:70, 70, replace=TRUE)

set.seed(75)
strap75 <- sample(1:70, 70, replace=TRUE)

set.seed(76)
strap76 <- sample(1:70, 70, replace=TRUE)

set.seed(77)
strap77 <- sample(1:70, 70, replace=TRUE)

set.seed(78)
strap78 <- sample(1:70, 70, replace=TRUE)

set.seed(79)
strap79 <- sample(1:70, 70, replace=TRUE)



set.seed(80)
strap80 <- sample(1:70, 70, replace=TRUE)

set.seed(81)
strap81 <- sample(1:70, 70, replace=TRUE)

set.seed(82)
strap82 <- sample(1:70, 70, replace=TRUE)

set.seed(83)
strap83 <- sample(1:70, 70, replace=TRUE)

set.seed(84)
strap84 <- sample(1:70, 70, replace=TRUE)

set.seed(85)
strap85 <- sample(1:70, 70, replace=TRUE)

set.seed(86)
strap86 <- sample(1:70, 70, replace=TRUE)

set.seed(87)
strap87 <- sample(1:70, 70, replace=TRUE)

set.seed(88)
strap88 <- sample(1:70, 70, replace=TRUE)

set.seed(89)
strap89 <- sample(1:70, 70, replace=TRUE)



set.seed(90)
strap90 <- sample(1:70, 70, replace=TRUE)

set.seed(91)
strap91 <- sample(1:70, 70, replace=TRUE)

set.seed(92)
strap92 <- sample(1:70, 70, replace=TRUE)

set.seed(93)
strap93 <- sample(1:70, 70, replace=TRUE)

set.seed(94)
strap94 <- sample(1:70, 70, replace=TRUE)

set.seed(95)
strap95 <- sample(1:70, 70, replace=TRUE)

set.seed(96)
strap96 <- sample(1:70, 70, replace=TRUE)

set.seed(97)
strap97 <- sample(1:70, 70, replace=TRUE)

set.seed(98)
strap98 <- sample(1:70, 70, replace=TRUE)

set.seed(99)
strap99 <- sample(1:70, 70, replace=TRUE)

set.seed(100)
strap100 <- sample(1:70, 70, replace=TRUE)


#   9  use random chunk selection to merge and create iterations
                             
tidy_EP_chunks_1 <- filter(tidy_EP_chunks, chunk %in% strap1)
tidy_EP_chunks_2 <- filter(tidy_EP_chunks, chunk %in% strap2)
tidy_EP_chunks_3 <- filter(tidy_EP_chunks, chunk %in% strap3)
tidy_EP_chunks_4 <- filter(tidy_EP_chunks, chunk %in% strap4)
tidy_EP_chunks_5 <- filter(tidy_EP_chunks, chunk %in% strap5)
tidy_EP_chunks_6 <- filter(tidy_EP_chunks, chunk %in% strap6)
tidy_EP_chunks_7 <- filter(tidy_EP_chunks, chunk %in% strap7)
tidy_EP_chunks_8 <- filter(tidy_EP_chunks, chunk %in% strap8)
tidy_EP_chunks_9 <- filter(tidy_EP_chunks, chunk %in% strap9)



tidy_EP_chunks_10 <- filter(tidy_EP_chunks, chunk %in% strap10)
tidy_EP_chunks_11 <- filter(tidy_EP_chunks, chunk %in% strap11)
tidy_EP_chunks_12 <- filter(tidy_EP_chunks, chunk %in% strap12)
tidy_EP_chunks_13 <- filter(tidy_EP_chunks, chunk %in% strap13)
tidy_EP_chunks_14 <- filter(tidy_EP_chunks, chunk %in% strap14)
tidy_EP_chunks_15 <- filter(tidy_EP_chunks, chunk %in% strap15)
tidy_EP_chunks_16 <- filter(tidy_EP_chunks, chunk %in% strap16)
tidy_EP_chunks_17 <- filter(tidy_EP_chunks, chunk %in% strap17)
tidy_EP_chunks_18 <- filter(tidy_EP_chunks, chunk %in% strap18)
tidy_EP_chunks_19 <- filter(tidy_EP_chunks, chunk %in% strap19)



tidy_EP_chunks_20 <- filter(tidy_EP_chunks, chunk %in% strap20)
tidy_EP_chunks_21 <- filter(tidy_EP_chunks, chunk %in% strap21)
tidy_EP_chunks_22 <- filter(tidy_EP_chunks, chunk %in% strap22)
tidy_EP_chunks_23 <- filter(tidy_EP_chunks, chunk %in% strap23)
tidy_EP_chunks_24 <- filter(tidy_EP_chunks, chunk %in% strap24)
tidy_EP_chunks_25 <- filter(tidy_EP_chunks, chunk %in% strap25)
tidy_EP_chunks_26 <- filter(tidy_EP_chunks, chunk %in% strap26)
tidy_EP_chunks_27 <- filter(tidy_EP_chunks, chunk %in% strap27)
tidy_EP_chunks_28 <- filter(tidy_EP_chunks, chunk %in% strap28)
tidy_EP_chunks_29 <- filter(tidy_EP_chunks, chunk %in% strap29)


tidy_EP_chunks_20 <- filter(tidy_EP_chunks, chunk %in% strap20)
tidy_EP_chunks_21 <- filter(tidy_EP_chunks, chunk %in% strap21)
tidy_EP_chunks_22 <- filter(tidy_EP_chunks, chunk %in% strap22)
tidy_EP_chunks_23 <- filter(tidy_EP_chunks, chunk %in% strap23)
tidy_EP_chunks_24 <- filter(tidy_EP_chunks, chunk %in% strap24)
tidy_EP_chunks_25 <- filter(tidy_EP_chunks, chunk %in% strap25)
tidy_EP_chunks_26 <- filter(tidy_EP_chunks, chunk %in% strap26)
tidy_EP_chunks_27 <- filter(tidy_EP_chunks, chunk %in% strap27)
tidy_EP_chunks_28 <- filter(tidy_EP_chunks, chunk %in% strap28)
tidy_EP_chunks_29 <- filter(tidy_EP_chunks, chunk %in% strap29)


tidy_EP_chunks_20 <- filter(tidy_EP_chunks, chunk %in% strap20)
tidy_EP_chunks_21 <- filter(tidy_EP_chunks, chunk %in% strap21)
tidy_EP_chunks_22 <- filter(tidy_EP_chunks, chunk %in% strap22)
tidy_EP_chunks_23 <- filter(tidy_EP_chunks, chunk %in% strap23)
tidy_EP_chunks_24 <- filter(tidy_EP_chunks, chunk %in% strap24)
tidy_EP_chunks_25 <- filter(tidy_EP_chunks, chunk %in% strap25)
tidy_EP_chunks_26 <- filter(tidy_EP_chunks, chunk %in% strap26)
tidy_EP_chunks_27 <- filter(tidy_EP_chunks, chunk %in% strap27)
tidy_EP_chunks_28 <- filter(tidy_EP_chunks, chunk %in% strap28)
tidy_EP_chunks_29 <- filter(tidy_EP_chunks, chunk %in% strap29)


tidy_EP_chunks_30 <- filter(tidy_EP_chunks, chunk %in% strap30)
tidy_EP_chunks_31 <- filter(tidy_EP_chunks, chunk %in% strap31)
tidy_EP_chunks_32 <- filter(tidy_EP_chunks, chunk %in% strap32)
tidy_EP_chunks_33 <- filter(tidy_EP_chunks, chunk %in% strap33)
tidy_EP_chunks_34 <- filter(tidy_EP_chunks, chunk %in% strap34)
tidy_EP_chunks_35 <- filter(tidy_EP_chunks, chunk %in% strap35)
tidy_EP_chunks_36 <- filter(tidy_EP_chunks, chunk %in% strap36)
tidy_EP_chunks_37 <- filter(tidy_EP_chunks, chunk %in% strap37)
tidy_EP_chunks_38 <- filter(tidy_EP_chunks, chunk %in% strap38)
tidy_EP_chunks_39 <- filter(tidy_EP_chunks, chunk %in% strap39)


tidy_EP_chunks_40 <- filter(tidy_EP_chunks, chunk %in% strap40)
tidy_EP_chunks_41 <- filter(tidy_EP_chunks, chunk %in% strap41)
tidy_EP_chunks_42 <- filter(tidy_EP_chunks, chunk %in% strap42)
tidy_EP_chunks_43 <- filter(tidy_EP_chunks, chunk %in% strap43)
tidy_EP_chunks_44 <- filter(tidy_EP_chunks, chunk %in% strap44)
tidy_EP_chunks_45 <- filter(tidy_EP_chunks, chunk %in% strap45)
tidy_EP_chunks_46 <- filter(tidy_EP_chunks, chunk %in% strap46)
tidy_EP_chunks_47 <- filter(tidy_EP_chunks, chunk %in% strap47)
tidy_EP_chunks_48 <- filter(tidy_EP_chunks, chunk %in% strap48)
tidy_EP_chunks_49 <- filter(tidy_EP_chunks, chunk %in% strap49)


tidy_EP_chunks_40 <- filter(tidy_EP_chunks, chunk %in% strap40)
tidy_EP_chunks_41 <- filter(tidy_EP_chunks, chunk %in% strap41)
tidy_EP_chunks_42 <- filter(tidy_EP_chunks, chunk %in% strap42)
tidy_EP_chunks_43 <- filter(tidy_EP_chunks, chunk %in% strap43)
tidy_EP_chunks_44 <- filter(tidy_EP_chunks, chunk %in% strap44)
tidy_EP_chunks_45 <- filter(tidy_EP_chunks, chunk %in% strap45)
tidy_EP_chunks_46 <- filter(tidy_EP_chunks, chunk %in% strap46)
tidy_EP_chunks_47 <- filter(tidy_EP_chunks, chunk %in% strap47)
tidy_EP_chunks_48 <- filter(tidy_EP_chunks, chunk %in% strap48)
tidy_EP_chunks_49 <- filter(tidy_EP_chunks, chunk %in% strap49)


tidy_EP_chunks_50 <- filter(tidy_EP_chunks, chunk %in% strap50)
tidy_EP_chunks_51 <- filter(tidy_EP_chunks, chunk %in% strap51)
tidy_EP_chunks_52 <- filter(tidy_EP_chunks, chunk %in% strap52)
tidy_EP_chunks_53 <- filter(tidy_EP_chunks, chunk %in% strap53)
tidy_EP_chunks_54 <- filter(tidy_EP_chunks, chunk %in% strap54)
tidy_EP_chunks_55 <- filter(tidy_EP_chunks, chunk %in% strap55)
tidy_EP_chunks_56 <- filter(tidy_EP_chunks, chunk %in% strap56)
tidy_EP_chunks_57 <- filter(tidy_EP_chunks, chunk %in% strap57)
tidy_EP_chunks_58 <- filter(tidy_EP_chunks, chunk %in% strap58)
tidy_EP_chunks_59 <- filter(tidy_EP_chunks, chunk %in% strap59)


tidy_EP_chunks_60 <- filter(tidy_EP_chunks, chunk %in% strap60)
tidy_EP_chunks_61 <- filter(tidy_EP_chunks, chunk %in% strap61)
tidy_EP_chunks_62 <- filter(tidy_EP_chunks, chunk %in% strap62)
tidy_EP_chunks_63 <- filter(tidy_EP_chunks, chunk %in% strap63)
tidy_EP_chunks_64 <- filter(tidy_EP_chunks, chunk %in% strap64)
tidy_EP_chunks_65 <- filter(tidy_EP_chunks, chunk %in% strap65)
tidy_EP_chunks_66 <- filter(tidy_EP_chunks, chunk %in% strap66)
tidy_EP_chunks_67 <- filter(tidy_EP_chunks, chunk %in% strap67)
tidy_EP_chunks_68 <- filter(tidy_EP_chunks, chunk %in% strap68)
tidy_EP_chunks_69 <- filter(tidy_EP_chunks, chunk %in% strap69)


tidy_EP_chunks_60 <- filter(tidy_EP_chunks, chunk %in% strap60)
tidy_EP_chunks_61 <- filter(tidy_EP_chunks, chunk %in% strap61)
tidy_EP_chunks_62 <- filter(tidy_EP_chunks, chunk %in% strap62)
tidy_EP_chunks_63 <- filter(tidy_EP_chunks, chunk %in% strap63)
tidy_EP_chunks_64 <- filter(tidy_EP_chunks, chunk %in% strap64)
tidy_EP_chunks_65 <- filter(tidy_EP_chunks, chunk %in% strap65)
tidy_EP_chunks_66 <- filter(tidy_EP_chunks, chunk %in% strap66)
tidy_EP_chunks_67 <- filter(tidy_EP_chunks, chunk %in% strap67)
tidy_EP_chunks_68 <- filter(tidy_EP_chunks, chunk %in% strap68)
tidy_EP_chunks_69 <- filter(tidy_EP_chunks, chunk %in% strap69)


tidy_EP_chunks_60 <- filter(tidy_EP_chunks, chunk %in% strap60)
tidy_EP_chunks_61 <- filter(tidy_EP_chunks, chunk %in% strap61)
tidy_EP_chunks_62 <- filter(tidy_EP_chunks, chunk %in% strap62)
tidy_EP_chunks_63 <- filter(tidy_EP_chunks, chunk %in% strap63)
tidy_EP_chunks_64 <- filter(tidy_EP_chunks, chunk %in% strap64)
tidy_EP_chunks_65 <- filter(tidy_EP_chunks, chunk %in% strap65)
tidy_EP_chunks_66 <- filter(tidy_EP_chunks, chunk %in% strap66)
tidy_EP_chunks_67 <- filter(tidy_EP_chunks, chunk %in% strap67)
tidy_EP_chunks_68 <- filter(tidy_EP_chunks, chunk %in% strap68)
tidy_EP_chunks_69 <- filter(tidy_EP_chunks, chunk %in% strap69)


tidy_EP_chunks_70 <- filter(tidy_EP_chunks, chunk %in% strap70)
tidy_EP_chunks_71 <- filter(tidy_EP_chunks, chunk %in% strap71)
tidy_EP_chunks_72 <- filter(tidy_EP_chunks, chunk %in% strap72)
tidy_EP_chunks_73 <- filter(tidy_EP_chunks, chunk %in% strap73)
tidy_EP_chunks_74 <- filter(tidy_EP_chunks, chunk %in% strap74)
tidy_EP_chunks_75 <- filter(tidy_EP_chunks, chunk %in% strap75)
tidy_EP_chunks_76 <- filter(tidy_EP_chunks, chunk %in% strap76)
tidy_EP_chunks_77 <- filter(tidy_EP_chunks, chunk %in% strap77)
tidy_EP_chunks_78 <- filter(tidy_EP_chunks, chunk %in% strap78)
tidy_EP_chunks_79 <- filter(tidy_EP_chunks, chunk %in% strap79)


tidy_EP_chunks_80 <- filter(tidy_EP_chunks, chunk %in% strap80)
tidy_EP_chunks_81 <- filter(tidy_EP_chunks, chunk %in% strap81)
tidy_EP_chunks_82 <- filter(tidy_EP_chunks, chunk %in% strap82)
tidy_EP_chunks_83 <- filter(tidy_EP_chunks, chunk %in% strap83)
tidy_EP_chunks_84 <- filter(tidy_EP_chunks, chunk %in% strap84)
tidy_EP_chunks_85 <- filter(tidy_EP_chunks, chunk %in% strap85)
tidy_EP_chunks_86 <- filter(tidy_EP_chunks, chunk %in% strap86)
tidy_EP_chunks_87 <- filter(tidy_EP_chunks, chunk %in% strap87)
tidy_EP_chunks_88 <- filter(tidy_EP_chunks, chunk %in% strap88)
tidy_EP_chunks_89 <- filter(tidy_EP_chunks, chunk %in% strap89)


tidy_EP_chunks_90 <- filter(tidy_EP_chunks, chunk %in% strap90)
tidy_EP_chunks_91 <- filter(tidy_EP_chunks, chunk %in% strap91)
tidy_EP_chunks_92 <- filter(tidy_EP_chunks, chunk %in% strap92)
tidy_EP_chunks_93 <- filter(tidy_EP_chunks, chunk %in% strap93)
tidy_EP_chunks_94 <- filter(tidy_EP_chunks, chunk %in% strap94)
tidy_EP_chunks_95 <- filter(tidy_EP_chunks, chunk %in% strap95)
tidy_EP_chunks_96 <- filter(tidy_EP_chunks, chunk %in% strap96)
tidy_EP_chunks_97 <- filter(tidy_EP_chunks, chunk %in% strap97)
tidy_EP_chunks_98 <- filter(tidy_EP_chunks, chunk %in% strap98)
tidy_EP_chunks_99 <- filter(tidy_EP_chunks, chunk %in% strap99)

tidy_EP_chunks_100 <- filter(tidy_EP_chunks, chunk %in% strap100)

#   10   Build Document-Term Matrix - DTM - for each term in each iteration

# iterations separated by:
#-- CHUNK 

##  components of class



affluence_list <- list( "rich" , "wealthy")

employment_list <- list("work" , "job" , "career" , "occupation" , "profession")

status_list <- list("distinguished" , "rank")

cultivation_list <- list("cultured" , "civilized" , "proper" , "refined" , "cultivated")

education_list <- list("education", "college", "graduate")

morality_list <- list("good" , "virtuous"  , "decent" , "moral")



#test as list

tidy_EP_DT_1_employment_list <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c(employment_list), wv=ngram1908_33)

tidy_EP_DT_1_status_list <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c(status_list), wv=ngram1908_33)

tidy_EP_DT_1_cultivation_list <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c(cultivation_list), wv=ngram1908_33)

tidy_EP_DT_1_edu_list <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c(education_list), wv=ngram1908_33)

tidy_EP_DT_1_morality_list <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c(morality_list), wv=ngram1908_33)











#  bind concept around centroid


affluence_bind <- c("rich" , "wealthy")

employment_bind <- c("work" , "job" , "career" , "occupation" , "profession")

status_bind <- c("distinguished" , "rank")

cultivation_bind <- c("cultured" , "civilized" , "proper" , "refined" , "cultivated")

education_bind <- c("education", "graduate" , "college")

morality_bind <- c("good" , "virtuous"  , "decent" , "moral")



affluence <- get_centroid(affluence_bind, ngram1908_33)
employment <- get_centroid(employment_bind, ngram1908_33)
status <- get_centroid(status_bind, ngram1908_33)
cultivation <- get_centroid(cultivation_bind, ngram1908_33)
education <- get_centroid(education_bind, ngram1908_33)
morality <- get_centroid(morality_bind, ngram1908_33)




## Graph all together: figure 1

tidy_EP_affluence <- tidy_EP_full %>% 
  cast_dtm(term = word, 
           document = filename, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)


tidy_EP_employment <- tidy_EP_full %>% 
  cast_dtm(term = word, 
           document = filename, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_status <- tidy_EP_full %>% 
  cast_dtm(term = word, 
           document = filename, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_cultivation <- tidy_EP_full %>% 
  cast_dtm(term = word, 
           document = filename, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_education <- tidy_EP_full %>% 
  cast_dtm(term = word, 
           document = filename, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_morality <- tidy_EP_full %>% 
  cast_dtm(term = word, 
           document = filename, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)


tidy_EP_all <- full_join(tidy_EP_affluence, tidy_EP_employment)
tidy_EP_all <- full_join(tidy_EP_all, tidy_EP_status)
tidy_EP_all <- full_join(tidy_EP_all, tidy_EP_cultivation)
tidy_EP_all <- full_join(tidy_EP_all, tidy_EP_education)
tidy_EP_all <- full_join(tidy_EP_all, tidy_EP_morality)

##  export to CSV

write.table(tidy_EP_all, "kline(2022_5_17)EP-MOC_all_ngram1908_33.csv", row.names = FALSE)


## create figure of all combined




















## Begin as grouped-concept

tidy_EP_DT_1_affluence <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

tidy_EP_DT_1_employment <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_1_status <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_1_cultivation <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_1_edu <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_1_morality <- tidy_EP_chunks_1 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_1 <- inner_join(tidy_EP_DT_1_affluence, tidy_EP_DT_1_employment)
tidy_EP_DT_1 <- inner_join(tidy_EP_DT_1, tidy_EP_DT_1_status)
tidy_EP_DT_1 <- inner_join(tidy_EP_DT_1, tidy_EP_DT_1_cultivation)
tidy_EP_DT_1 <- inner_join(tidy_EP_DT_1, tidy_EP_DT_1_edu)
tidy_EP_DT_1 <- inner_join(tidy_EP_DT_1, tidy_EP_DT_1_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_2_affluence <- tidy_EP_chunks_2 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_2_employment <- tidy_EP_chunks_2 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_2_status <- tidy_EP_chunks_2 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_2_cultivation <- tidy_EP_chunks_2 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_2_edu <- tidy_EP_chunks_2 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_2_morality <- tidy_EP_chunks_2 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_2 <- inner_join(tidy_EP_DT_2_affluence, tidy_EP_DT_2_employment)
tidy_EP_DT_2 <- inner_join(tidy_EP_DT_2, tidy_EP_DT_2_status)
tidy_EP_DT_2 <- inner_join(tidy_EP_DT_2, tidy_EP_DT_2_cultivation)
tidy_EP_DT_2 <- inner_join(tidy_EP_DT_2, tidy_EP_DT_2_edu)
tidy_EP_DT_2 <- inner_join(tidy_EP_DT_2, tidy_EP_DT_2_morality)


#-- CHUNK 
##  components of class
tidy_EP_DT_3_affluence <- tidy_EP_chunks_3 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_3_employment <- tidy_EP_chunks_3 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_3_status <- tidy_EP_chunks_3 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_3_cultivation <- tidy_EP_chunks_3 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_3_edu <- tidy_EP_chunks_3 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_3_morality <- tidy_EP_chunks_3 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_3 <- inner_join(tidy_EP_DT_3_affluence, tidy_EP_DT_3_employment)
tidy_EP_DT_3 <- inner_join(tidy_EP_DT_3, tidy_EP_DT_3_status)
tidy_EP_DT_3 <- inner_join(tidy_EP_DT_3, tidy_EP_DT_3_cultivation)
tidy_EP_DT_3 <- inner_join(tidy_EP_DT_3, tidy_EP_DT_3_edu)
tidy_EP_DT_3 <- inner_join(tidy_EP_DT_3, tidy_EP_DT_3_morality)


#-- CHUNK 

##  components of class
tidy_EP_DT_4_affluence <- tidy_EP_chunks_4 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_4_employment <- tidy_EP_chunks_4 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_4_status <- tidy_EP_chunks_4 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_4_cultivation <- tidy_EP_chunks_4 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_4_edu <- tidy_EP_chunks_4 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_4_morality <- tidy_EP_chunks_4 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_4 <- inner_join(tidy_EP_DT_4_affluence, tidy_EP_DT_4_employment)
tidy_EP_DT_4 <- inner_join(tidy_EP_DT_4, tidy_EP_DT_4_status)
tidy_EP_DT_4 <- inner_join(tidy_EP_DT_4, tidy_EP_DT_4_cultivation)
tidy_EP_DT_4 <- inner_join(tidy_EP_DT_4, tidy_EP_DT_4_edu)
tidy_EP_DT_4 <- inner_join(tidy_EP_DT_4, tidy_EP_DT_4_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_5_affluence <- tidy_EP_chunks_5 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_5_employment <- tidy_EP_chunks_5 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_5_status <- tidy_EP_chunks_5 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_5_cultivation <- tidy_EP_chunks_5 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_5_edu <- tidy_EP_chunks_5 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_5_morality <- tidy_EP_chunks_5 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_5 <- inner_join(tidy_EP_DT_5_affluence, tidy_EP_DT_5_employment)
tidy_EP_DT_5 <- inner_join(tidy_EP_DT_5, tidy_EP_DT_5_status)
tidy_EP_DT_5 <- inner_join(tidy_EP_DT_5, tidy_EP_DT_5_cultivation)
tidy_EP_DT_5 <- inner_join(tidy_EP_DT_5, tidy_EP_DT_5_edu)
tidy_EP_DT_5 <- inner_join(tidy_EP_DT_5, tidy_EP_DT_5_morality)
#   6   Build Document-Term Matrix - DTM
#-- CHUNK 6

##  components of class
tidy_EP_DT_6_affluence <- tidy_EP_chunks_6 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_6_employment <- tidy_EP_chunks_6 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_6_status <- tidy_EP_chunks_6 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_6_cultivation <- tidy_EP_chunks_6 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_6_edu <- tidy_EP_chunks_6 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_6_morality <- tidy_EP_chunks_6 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_6 <- inner_join(tidy_EP_DT_6_affluence, tidy_EP_DT_6_employment)
tidy_EP_DT_6 <- inner_join(tidy_EP_DT_6, tidy_EP_DT_6_status)
tidy_EP_DT_6 <- inner_join(tidy_EP_DT_6, tidy_EP_DT_6_cultivation)
tidy_EP_DT_6 <- inner_join(tidy_EP_DT_6, tidy_EP_DT_6_edu)
tidy_EP_DT_6 <- inner_join(tidy_EP_DT_6, tidy_EP_DT_6_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_7_affluence <- tidy_EP_chunks_7 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_7_employment <- tidy_EP_chunks_7 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_7_status <- tidy_EP_chunks_7 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_7_cultivation <- tidy_EP_chunks_7 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_7_edu <- tidy_EP_chunks_7 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_7_morality <- tidy_EP_chunks_7 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_7 <- inner_join(tidy_EP_DT_7_affluence, tidy_EP_DT_7_employment)
tidy_EP_DT_7 <- inner_join(tidy_EP_DT_7, tidy_EP_DT_7_status)
tidy_EP_DT_7 <- inner_join(tidy_EP_DT_7, tidy_EP_DT_7_cultivation)
tidy_EP_DT_7 <- inner_join(tidy_EP_DT_7, tidy_EP_DT_7_edu)
tidy_EP_DT_7 <- inner_join(tidy_EP_DT_7, tidy_EP_DT_7_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_8_affluence <- tidy_EP_chunks_8 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_8_employment <- tidy_EP_chunks_8 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_8_status <- tidy_EP_chunks_8 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_8_cultivation <- tidy_EP_chunks_8 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_8_edu <- tidy_EP_chunks_8 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_8_morality <- tidy_EP_chunks_8 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_8 <- inner_join(tidy_EP_DT_8_affluence, tidy_EP_DT_8_employment)
tidy_EP_DT_8 <- inner_join(tidy_EP_DT_8, tidy_EP_DT_8_status)
tidy_EP_DT_8 <- inner_join(tidy_EP_DT_8, tidy_EP_DT_8_cultivation)
tidy_EP_DT_8 <- inner_join(tidy_EP_DT_8, tidy_EP_DT_8_edu)
tidy_EP_DT_8 <- inner_join(tidy_EP_DT_8, tidy_EP_DT_8_morality)


#-- CHUNK 

##  components of class
tidy_EP_DT_9_affluence <- tidy_EP_chunks_9 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_9_employment <- tidy_EP_chunks_9 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_9_status <- tidy_EP_chunks_9 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_9_cultivation <- tidy_EP_chunks_9 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_9_edu <- tidy_EP_chunks_9 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_9_morality <- tidy_EP_chunks_9 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_9 <- inner_join(tidy_EP_DT_9_affluence, tidy_EP_DT_9_employment)
tidy_EP_DT_9 <- inner_join(tidy_EP_DT_9, tidy_EP_DT_9_status)
tidy_EP_DT_9 <- inner_join(tidy_EP_DT_9, tidy_EP_DT_9_cultivation)
tidy_EP_DT_9 <- inner_join(tidy_EP_DT_9, tidy_EP_DT_9_edu)
tidy_EP_DT_9 <- inner_join(tidy_EP_DT_9, tidy_EP_DT_9_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_10_affluence <- tidy_EP_chunks_10 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_10_employment <- tidy_EP_chunks_10 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_10_status <- tidy_EP_chunks_10 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_10_cultivation <- tidy_EP_chunks_10 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_10_edu <- tidy_EP_chunks_10 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_10_morality <- tidy_EP_chunks_10 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_10 <- inner_join(tidy_EP_DT_10_affluence, tidy_EP_DT_10_employment)
tidy_EP_DT_10 <- inner_join(tidy_EP_DT_10, tidy_EP_DT_10_status)
tidy_EP_DT_10 <- inner_join(tidy_EP_DT_10, tidy_EP_DT_10_cultivation)
tidy_EP_DT_10 <- inner_join(tidy_EP_DT_10, tidy_EP_DT_10_edu)
tidy_EP_DT_10 <- inner_join(tidy_EP_DT_10, tidy_EP_DT_10_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_11_affluence <- tidy_EP_chunks_11 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_11_employment <- tidy_EP_chunks_11 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_11_status <- tidy_EP_chunks_11 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_11_cultivation <- tidy_EP_chunks_11 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_11_edu <- tidy_EP_chunks_11 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_11_morality <- tidy_EP_chunks_11 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_11 <- inner_join(tidy_EP_DT_11_affluence, tidy_EP_DT_11_employment)
tidy_EP_DT_11 <- inner_join(tidy_EP_DT_11, tidy_EP_DT_11_status)
tidy_EP_DT_11 <- inner_join(tidy_EP_DT_11, tidy_EP_DT_11_cultivation)
tidy_EP_DT_11 <- inner_join(tidy_EP_DT_11, tidy_EP_DT_11_edu)
tidy_EP_DT_11 <- inner_join(tidy_EP_DT_11, tidy_EP_DT_11_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_12_affluence <- tidy_EP_chunks_12 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_12_employment <- tidy_EP_chunks_12 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_12_status <- tidy_EP_chunks_12 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_12_cultivation <- tidy_EP_chunks_12 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_12_edu <- tidy_EP_chunks_12 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_12_morality <- tidy_EP_chunks_12 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_12 <- inner_join(tidy_EP_DT_12_affluence, tidy_EP_DT_12_employment)
tidy_EP_DT_12 <- inner_join(tidy_EP_DT_12, tidy_EP_DT_12_status)
tidy_EP_DT_12 <- inner_join(tidy_EP_DT_12, tidy_EP_DT_12_cultivation)
tidy_EP_DT_12 <- inner_join(tidy_EP_DT_12, tidy_EP_DT_12_edu)
tidy_EP_DT_12 <- inner_join(tidy_EP_DT_12, tidy_EP_DT_12_morality)


#-- CHUNK 

##  components of class
tidy_EP_DT_13_affluence <- tidy_EP_chunks_13 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_13_employment <- tidy_EP_chunks_13 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_13_status <- tidy_EP_chunks_13 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_13_cultivation <- tidy_EP_chunks_13 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_13_edu <- tidy_EP_chunks_13 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_13_morality <- tidy_EP_chunks_13 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_13 <- inner_join(tidy_EP_DT_13_affluence, tidy_EP_DT_13_employment)
tidy_EP_DT_13 <- inner_join(tidy_EP_DT_13, tidy_EP_DT_13_status)
tidy_EP_DT_13 <- inner_join(tidy_EP_DT_13, tidy_EP_DT_13_cultivation)
tidy_EP_DT_13 <- inner_join(tidy_EP_DT_13, tidy_EP_DT_13_edu)
tidy_EP_DT_13 <- inner_join(tidy_EP_DT_13, tidy_EP_DT_13_morality)


#-- CHUNK 

##  components of class
tidy_EP_DT_14_affluence <- tidy_EP_chunks_14 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_14_employment <- tidy_EP_chunks_14 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_14_status <- tidy_EP_chunks_14 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_14_cultivation <- tidy_EP_chunks_14 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_14_edu <- tidy_EP_chunks_14 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_14_morality <- tidy_EP_chunks_14 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_14 <- inner_join(tidy_EP_DT_14_affluence, tidy_EP_DT_14_employment)
tidy_EP_DT_14 <- inner_join(tidy_EP_DT_14, tidy_EP_DT_14_status)
tidy_EP_DT_14 <- inner_join(tidy_EP_DT_14, tidy_EP_DT_14_cultivation)
tidy_EP_DT_14 <- inner_join(tidy_EP_DT_14, tidy_EP_DT_14_edu)
tidy_EP_DT_14 <- inner_join(tidy_EP_DT_14, tidy_EP_DT_14_morality)


#-- CHUNK 

##  components of class
tidy_EP_DT_15_affluence <- tidy_EP_chunks_15 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_15_employment <- tidy_EP_chunks_15 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_15_status <- tidy_EP_chunks_15 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_15_cultivation <- tidy_EP_chunks_15 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_15_edu <- tidy_EP_chunks_15 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_15_morality <- tidy_EP_chunks_15 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_15 <- inner_join(tidy_EP_DT_15_affluence, tidy_EP_DT_15_employment)
tidy_EP_DT_15 <- inner_join(tidy_EP_DT_15, tidy_EP_DT_15_status)
tidy_EP_DT_15 <- inner_join(tidy_EP_DT_15, tidy_EP_DT_15_cultivation)
tidy_EP_DT_15 <- inner_join(tidy_EP_DT_15, tidy_EP_DT_15_edu)
tidy_EP_DT_15 <- inner_join(tidy_EP_DT_15, tidy_EP_DT_15_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_16_affluence <- tidy_EP_chunks_16 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_16_employment <- tidy_EP_chunks_16 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_16_status <- tidy_EP_chunks_16 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_16_cultivation <- tidy_EP_chunks_16 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_16_edu <- tidy_EP_chunks_16 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_16_morality <- tidy_EP_chunks_16 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_16 <- inner_join(tidy_EP_DT_16_affluence, tidy_EP_DT_16_employment)
tidy_EP_DT_16 <- inner_join(tidy_EP_DT_16, tidy_EP_DT_16_status)
tidy_EP_DT_16 <- inner_join(tidy_EP_DT_16, tidy_EP_DT_16_cultivation)
tidy_EP_DT_16 <- inner_join(tidy_EP_DT_16, tidy_EP_DT_16_edu)
tidy_EP_DT_16 <- inner_join(tidy_EP_DT_16, tidy_EP_DT_16_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_17_affluence <- tidy_EP_chunks_17 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_17_employment <- tidy_EP_chunks_17 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_17_status <- tidy_EP_chunks_17 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_17_cultivation <- tidy_EP_chunks_17 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_17_edu <- tidy_EP_chunks_17 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_17_morality <- tidy_EP_chunks_17 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_17 <- inner_join(tidy_EP_DT_17_affluence, tidy_EP_DT_17_employment)
tidy_EP_DT_17 <- inner_join(tidy_EP_DT_17, tidy_EP_DT_17_status)
tidy_EP_DT_17 <- inner_join(tidy_EP_DT_17, tidy_EP_DT_17_cultivation)
tidy_EP_DT_17 <- inner_join(tidy_EP_DT_17, tidy_EP_DT_17_edu)
tidy_EP_DT_17 <- inner_join(tidy_EP_DT_17, tidy_EP_DT_17_morality)


#-- CHUNK 

##  components of class
tidy_EP_DT_18_affluence <- tidy_EP_chunks_18 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_18_employment <- tidy_EP_chunks_18 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_18_status <- tidy_EP_chunks_18 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_18_cultivation <- tidy_EP_chunks_18 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_18_edu <- tidy_EP_chunks_18 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_18_morality <- tidy_EP_chunks_18 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_18 <- inner_join(tidy_EP_DT_18_affluence, tidy_EP_DT_18_employment)
tidy_EP_DT_18 <- inner_join(tidy_EP_DT_18, tidy_EP_DT_18_status)
tidy_EP_DT_18 <- inner_join(tidy_EP_DT_18, tidy_EP_DT_18_cultivation)
tidy_EP_DT_18 <- inner_join(tidy_EP_DT_18, tidy_EP_DT_18_edu)
tidy_EP_DT_18 <- inner_join(tidy_EP_DT_18, tidy_EP_DT_18_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_19_affluence <- tidy_EP_chunks_19 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_19_employment <- tidy_EP_chunks_19 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_19_status <- tidy_EP_chunks_19 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_19_cultivation <- tidy_EP_chunks_19 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_19_edu <- tidy_EP_chunks_19 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_19_morality <- tidy_EP_chunks_19 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_19 <- inner_join(tidy_EP_DT_19_affluence, tidy_EP_DT_19_employment)
tidy_EP_DT_19 <- inner_join(tidy_EP_DT_19, tidy_EP_DT_19_status)
tidy_EP_DT_19 <- inner_join(tidy_EP_DT_19, tidy_EP_DT_19_cultivation)
tidy_EP_DT_19 <- inner_join(tidy_EP_DT_19, tidy_EP_DT_19_edu)
tidy_EP_DT_19 <- inner_join(tidy_EP_DT_19, tidy_EP_DT_19_morality)


#-- CHUNK 

##  components of class
tidy_EP_DT_20_affluence <- tidy_EP_chunks_20 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_20_employment <- tidy_EP_chunks_20 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_20_status <- tidy_EP_chunks_20 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_20_cultivation <- tidy_EP_chunks_20 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_20_edu <- tidy_EP_chunks_20 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_20_morality <- tidy_EP_chunks_20 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_20 <- inner_join(tidy_EP_DT_20_affluence, tidy_EP_DT_20_employment)
tidy_EP_DT_20 <- inner_join(tidy_EP_DT_20, tidy_EP_DT_20_status)
tidy_EP_DT_20 <- inner_join(tidy_EP_DT_20, tidy_EP_DT_20_cultivation)
tidy_EP_DT_20 <- inner_join(tidy_EP_DT_20, tidy_EP_DT_20_edu)
tidy_EP_DT_20 <- inner_join(tidy_EP_DT_20, tidy_EP_DT_20_morality)


#-- CHUNK 

##  components of class
tidy_EP_DT_21_affluence <- tidy_EP_chunks_21 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_21_employment <- tidy_EP_chunks_21 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_21_status <- tidy_EP_chunks_21 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_21_cultivation <- tidy_EP_chunks_21 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_21_edu <- tidy_EP_chunks_21 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_21_morality <- tidy_EP_chunks_21 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_21 <- inner_join(tidy_EP_DT_21_affluence, tidy_EP_DT_21_employment)
tidy_EP_DT_21 <- inner_join(tidy_EP_DT_21, tidy_EP_DT_21_status)
tidy_EP_DT_21 <- inner_join(tidy_EP_DT_21, tidy_EP_DT_21_cultivation)
tidy_EP_DT_21 <- inner_join(tidy_EP_DT_21, tidy_EP_DT_21_edu)
tidy_EP_DT_21 <- inner_join(tidy_EP_DT_21, tidy_EP_DT_21_morality)
#   12   Build Document-Term Matrix - DTM
#-- CHUNK 12

##  components of class
tidy_EP_DT_22_affluence <- tidy_EP_chunks_22 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_22_employment <- tidy_EP_chunks_22 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_22_status <- tidy_EP_chunks_22 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_22_cultivation <- tidy_EP_chunks_22 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_22_edu <- tidy_EP_chunks_22 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_22_morality <- tidy_EP_chunks_22 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_22 <- inner_join(tidy_EP_DT_22_affluence, tidy_EP_DT_22_employment)
tidy_EP_DT_22 <- inner_join(tidy_EP_DT_22, tidy_EP_DT_22_status)
tidy_EP_DT_22 <- inner_join(tidy_EP_DT_22, tidy_EP_DT_22_cultivation)
tidy_EP_DT_22 <- inner_join(tidy_EP_DT_22, tidy_EP_DT_22_edu)
tidy_EP_DT_22 <- inner_join(tidy_EP_DT_22, tidy_EP_DT_22_morality)


#-- CHUNK 

##  components of class
tidy_EP_DT_23_affluence <- tidy_EP_chunks_23 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_23_employment <- tidy_EP_chunks_23 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_23_status <- tidy_EP_chunks_23 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_23_cultivation <- tidy_EP_chunks_23 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_23_edu <- tidy_EP_chunks_23 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_23_morality <- tidy_EP_chunks_23 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_23 <- inner_join(tidy_EP_DT_23_affluence, tidy_EP_DT_23_employment)
tidy_EP_DT_23 <- inner_join(tidy_EP_DT_23, tidy_EP_DT_23_status)
tidy_EP_DT_23 <- inner_join(tidy_EP_DT_23, tidy_EP_DT_23_cultivation)
tidy_EP_DT_23 <- inner_join(tidy_EP_DT_23, tidy_EP_DT_23_edu)
tidy_EP_DT_23 <- inner_join(tidy_EP_DT_23, tidy_EP_DT_23_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_24_affluence <- tidy_EP_chunks_24 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_24_employment <- tidy_EP_chunks_24 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_24_status <- tidy_EP_chunks_24 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_24_cultivation <- tidy_EP_chunks_24 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_24_edu <- tidy_EP_chunks_24 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_24_morality <- tidy_EP_chunks_24 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_24 <- inner_join(tidy_EP_DT_24_affluence, tidy_EP_DT_24_employment)
tidy_EP_DT_24 <- inner_join(tidy_EP_DT_24, tidy_EP_DT_24_status)
tidy_EP_DT_24 <- inner_join(tidy_EP_DT_24, tidy_EP_DT_24_cultivation)
tidy_EP_DT_24 <- inner_join(tidy_EP_DT_24, tidy_EP_DT_24_edu)
tidy_EP_DT_24 <- inner_join(tidy_EP_DT_24, tidy_EP_DT_24_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_25_affluence <- tidy_EP_chunks_25 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_25_employment <- tidy_EP_chunks_25 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_25_status <- tidy_EP_chunks_25 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_25_cultivation <- tidy_EP_chunks_25 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_25_edu <- tidy_EP_chunks_25 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_25_morality <- tidy_EP_chunks_25 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_25 <- inner_join(tidy_EP_DT_25_affluence, tidy_EP_DT_25_employment)
tidy_EP_DT_25 <- inner_join(tidy_EP_DT_25, tidy_EP_DT_25_status)
tidy_EP_DT_25 <- inner_join(tidy_EP_DT_25, tidy_EP_DT_25_cultivation)
tidy_EP_DT_25 <- inner_join(tidy_EP_DT_25, tidy_EP_DT_25_edu)
tidy_EP_DT_25 <- inner_join(tidy_EP_DT_25, tidy_EP_DT_25_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_26_affluence <- tidy_EP_chunks_26 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_26_employment <- tidy_EP_chunks_26 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_26_status <- tidy_EP_chunks_26 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_26_cultivation <- tidy_EP_chunks_26 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_26_edu <- tidy_EP_chunks_26 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_26_morality <- tidy_EP_chunks_26 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_26 <- inner_join(tidy_EP_DT_26_affluence, tidy_EP_DT_26_employment)
tidy_EP_DT_26 <- inner_join(tidy_EP_DT_26, tidy_EP_DT_26_status)
tidy_EP_DT_26 <- inner_join(tidy_EP_DT_26, tidy_EP_DT_26_cultivation)
tidy_EP_DT_26 <- inner_join(tidy_EP_DT_26, tidy_EP_DT_26_edu)
tidy_EP_DT_26 <- inner_join(tidy_EP_DT_26, tidy_EP_DT_26_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_27_affluence <- tidy_EP_chunks_27 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_27_employment <- tidy_EP_chunks_27 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_27_status <- tidy_EP_chunks_27 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_27_cultivation <- tidy_EP_chunks_27 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_27_edu <- tidy_EP_chunks_27 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_27_morality <- tidy_EP_chunks_27 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_27 <- inner_join(tidy_EP_DT_27_affluence, tidy_EP_DT_27_employment)
tidy_EP_DT_27 <- inner_join(tidy_EP_DT_27, tidy_EP_DT_27_status)
tidy_EP_DT_27 <- inner_join(tidy_EP_DT_27, tidy_EP_DT_27_cultivation)
tidy_EP_DT_27 <- inner_join(tidy_EP_DT_27, tidy_EP_DT_27_edu)
tidy_EP_DT_27 <- inner_join(tidy_EP_DT_27, tidy_EP_DT_27_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_28_affluence <- tidy_EP_chunks_28 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_28_employment <- tidy_EP_chunks_28 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_28_status <- tidy_EP_chunks_28 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_28_cultivation <- tidy_EP_chunks_28 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_28_edu <- tidy_EP_chunks_28 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_28_morality <- tidy_EP_chunks_28 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_28 <- inner_join(tidy_EP_DT_28_affluence, tidy_EP_DT_28_employment)
tidy_EP_DT_28 <- inner_join(tidy_EP_DT_28, tidy_EP_DT_28_status)
tidy_EP_DT_28 <- inner_join(tidy_EP_DT_28, tidy_EP_DT_28_cultivation)
tidy_EP_DT_28 <- inner_join(tidy_EP_DT_28, tidy_EP_DT_28_edu)
tidy_EP_DT_28 <- inner_join(tidy_EP_DT_28, tidy_EP_DT_28_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_29_affluence <- tidy_EP_chunks_29 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_29_employment <- tidy_EP_chunks_29 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_29_status <- tidy_EP_chunks_29 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_29_cultivation <- tidy_EP_chunks_29 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_29_edu <- tidy_EP_chunks_29 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_29_morality <- tidy_EP_chunks_29 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_29 <- inner_join(tidy_EP_DT_29_affluence, tidy_EP_DT_29_employment)
tidy_EP_DT_29 <- inner_join(tidy_EP_DT_29, tidy_EP_DT_29_status)
tidy_EP_DT_29 <- inner_join(tidy_EP_DT_29, tidy_EP_DT_29_cultivation)
tidy_EP_DT_29 <- inner_join(tidy_EP_DT_29, tidy_EP_DT_29_edu)
tidy_EP_DT_29 <- inner_join(tidy_EP_DT_29, tidy_EP_DT_29_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_30_affluence <- tidy_EP_chunks_30 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_30_employment <- tidy_EP_chunks_30 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_30_status <- tidy_EP_chunks_30 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_30_cultivation <- tidy_EP_chunks_30 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_30_edu <- tidy_EP_chunks_30 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_30_morality <- tidy_EP_chunks_30 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_30 <- inner_join(tidy_EP_DT_30_affluence, tidy_EP_DT_30_employment)
tidy_EP_DT_30 <- inner_join(tidy_EP_DT_30, tidy_EP_DT_30_status)
tidy_EP_DT_30 <- inner_join(tidy_EP_DT_30, tidy_EP_DT_30_cultivation)
tidy_EP_DT_30 <- inner_join(tidy_EP_DT_30, tidy_EP_DT_30_edu)
tidy_EP_DT_30 <- inner_join(tidy_EP_DT_30, tidy_EP_DT_30_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_31_affluence <- tidy_EP_chunks_31 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_31_employment <- tidy_EP_chunks_31 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_31_status <- tidy_EP_chunks_31 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_31_cultivation <- tidy_EP_chunks_31 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_31_edu <- tidy_EP_chunks_31 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_31_morality <- tidy_EP_chunks_31 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_31 <- inner_join(tidy_EP_DT_31_affluence, tidy_EP_DT_31_employment)
tidy_EP_DT_31 <- inner_join(tidy_EP_DT_31, tidy_EP_DT_31_status)
tidy_EP_DT_31 <- inner_join(tidy_EP_DT_31, tidy_EP_DT_31_cultivation)
tidy_EP_DT_31 <- inner_join(tidy_EP_DT_31, tidy_EP_DT_31_edu)
tidy_EP_DT_31 <- inner_join(tidy_EP_DT_31, tidy_EP_DT_31_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_32_affluence <- tidy_EP_chunks_32 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
    CMDist(cw =c("affluence"), wv=ngram1908_33)

## class continued

tidy_EP_DT_32_employment <- tidy_EP_chunks_32 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

tidy_EP_DT_32_status <- tidy_EP_chunks_32 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_32_cultivation <- tidy_EP_chunks_32 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_32_edu <- tidy_EP_chunks_32 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_32_morality <- tidy_EP_chunks_32 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_32 <- inner_join(tidy_EP_DT_32_affluence, tidy_EP_DT_32_employment)
tidy_EP_DT_32 <- inner_join(tidy_EP_DT_32, tidy_EP_DT_32_status)
tidy_EP_DT_32 <- inner_join(tidy_EP_DT_32, tidy_EP_DT_32_cultivation)
tidy_EP_DT_32 <- inner_join(tidy_EP_DT_32, tidy_EP_DT_32_edu)
tidy_EP_DT_32 <- inner_join(tidy_EP_DT_32, tidy_EP_DT_32_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_33_affluence <- tidy_EP_chunks_33 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_33_employment <- tidy_EP_chunks_33 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_33_status <- tidy_EP_chunks_33 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_33_cultivation <- tidy_EP_chunks_33 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_33_edu <- tidy_EP_chunks_33 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_33_morality <- tidy_EP_chunks_33 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_33 <- inner_join(tidy_EP_DT_33_affluence, tidy_EP_DT_33_employment)
tidy_EP_DT_33 <- inner_join(tidy_EP_DT_33, tidy_EP_DT_33_status)
tidy_EP_DT_33 <- inner_join(tidy_EP_DT_33, tidy_EP_DT_33_cultivation)
tidy_EP_DT_33 <- inner_join(tidy_EP_DT_33, tidy_EP_DT_33_edu)
tidy_EP_DT_33 <- inner_join(tidy_EP_DT_33, tidy_EP_DT_33_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_34_affluence <- tidy_EP_chunks_34 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_34_employment <- tidy_EP_chunks_34 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_34_status <- tidy_EP_chunks_34 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_34_cultivation <- tidy_EP_chunks_34 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_34_edu <- tidy_EP_chunks_34 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_34_morality <- tidy_EP_chunks_34 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_34 <- inner_join(tidy_EP_DT_34_affluence, tidy_EP_DT_34_employment)
tidy_EP_DT_34 <- inner_join(tidy_EP_DT_34, tidy_EP_DT_34_status)
tidy_EP_DT_34 <- inner_join(tidy_EP_DT_34, tidy_EP_DT_34_cultivation)
tidy_EP_DT_34 <- inner_join(tidy_EP_DT_34, tidy_EP_DT_34_edu)
tidy_EP_DT_34 <- inner_join(tidy_EP_DT_34, tidy_EP_DT_34_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_35_affluence <- tidy_EP_chunks_35 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_35_employment <- tidy_EP_chunks_35 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_35_status <- tidy_EP_chunks_35 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_35_cultivation <- tidy_EP_chunks_35 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_35_edu <- tidy_EP_chunks_35 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_35_morality <- tidy_EP_chunks_35 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_35 <- inner_join(tidy_EP_DT_35_affluence, tidy_EP_DT_35_employment)
tidy_EP_DT_35 <- inner_join(tidy_EP_DT_35, tidy_EP_DT_35_status)
tidy_EP_DT_35 <- inner_join(tidy_EP_DT_35, tidy_EP_DT_35_cultivation)
tidy_EP_DT_35 <- inner_join(tidy_EP_DT_35, tidy_EP_DT_35_edu)
tidy_EP_DT_35 <- inner_join(tidy_EP_DT_35, tidy_EP_DT_35_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_36_affluence <- tidy_EP_chunks_36 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_36_employment <- tidy_EP_chunks_36 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_36_status <- tidy_EP_chunks_36 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_36_cultivation <- tidy_EP_chunks_36 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_36_edu <- tidy_EP_chunks_36 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_36_morality <- tidy_EP_chunks_36 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_36 <- inner_join(tidy_EP_DT_36_affluence, tidy_EP_DT_36_employment)
tidy_EP_DT_36 <- inner_join(tidy_EP_DT_36, tidy_EP_DT_36_status)
tidy_EP_DT_36 <- inner_join(tidy_EP_DT_36, tidy_EP_DT_36_cultivation)
tidy_EP_DT_36 <- inner_join(tidy_EP_DT_36, tidy_EP_DT_36_edu)
tidy_EP_DT_36 <- inner_join(tidy_EP_DT_36, tidy_EP_DT_36_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_37_affluence <- tidy_EP_chunks_37 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_37_employment <- tidy_EP_chunks_37 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_37_status <- tidy_EP_chunks_37 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_37_cultivation <- tidy_EP_chunks_37 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_37_edu <- tidy_EP_chunks_37 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_37_morality <- tidy_EP_chunks_37 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_37 <- inner_join(tidy_EP_DT_37_affluence, tidy_EP_DT_37_employment)
tidy_EP_DT_37 <- inner_join(tidy_EP_DT_37, tidy_EP_DT_37_status)
tidy_EP_DT_37 <- inner_join(tidy_EP_DT_37, tidy_EP_DT_37_cultivation)
tidy_EP_DT_37 <- inner_join(tidy_EP_DT_37, tidy_EP_DT_37_edu)
tidy_EP_DT_37 <- inner_join(tidy_EP_DT_37, tidy_EP_DT_37_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_38_affluence <- tidy_EP_chunks_38 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_38_employment <- tidy_EP_chunks_38 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_38_status <- tidy_EP_chunks_38 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_38_cultivation <- tidy_EP_chunks_38 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_38_edu <- tidy_EP_chunks_38 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_38_morality <- tidy_EP_chunks_38 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_38 <- inner_join(tidy_EP_DT_38_affluence, tidy_EP_DT_38_employment)
tidy_EP_DT_38 <- inner_join(tidy_EP_DT_38, tidy_EP_DT_38_status)
tidy_EP_DT_38 <- inner_join(tidy_EP_DT_38, tidy_EP_DT_38_cultivation)
tidy_EP_DT_38 <- inner_join(tidy_EP_DT_38, tidy_EP_DT_38_edu)
tidy_EP_DT_38 <- inner_join(tidy_EP_DT_38, tidy_EP_DT_38_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_39_affluence <- tidy_EP_chunks_39 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_39_employment <- tidy_EP_chunks_39 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_39_status <- tidy_EP_chunks_39 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_39_cultivation <- tidy_EP_chunks_39 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_39_edu <- tidy_EP_chunks_39 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_39_morality <- tidy_EP_chunks_39 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_39 <- inner_join(tidy_EP_DT_39_affluence, tidy_EP_DT_39_employment)
tidy_EP_DT_39 <- inner_join(tidy_EP_DT_39, tidy_EP_DT_39_status)
tidy_EP_DT_39 <- inner_join(tidy_EP_DT_39, tidy_EP_DT_39_cultivation)
tidy_EP_DT_39 <- inner_join(tidy_EP_DT_39, tidy_EP_DT_39_edu)
tidy_EP_DT_39 <- inner_join(tidy_EP_DT_39, tidy_EP_DT_39_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_40_affluence <- tidy_EP_chunks_40 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_40_employment <- tidy_EP_chunks_40 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_40_status <- tidy_EP_chunks_40 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_40_cultivation <- tidy_EP_chunks_40 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_40_edu <- tidy_EP_chunks_40 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_40_morality <- tidy_EP_chunks_40 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_40 <- inner_join(tidy_EP_DT_40_affluence, tidy_EP_DT_40_employment)
tidy_EP_DT_40 <- inner_join(tidy_EP_DT_40, tidy_EP_DT_40_status)
tidy_EP_DT_40 <- inner_join(tidy_EP_DT_40, tidy_EP_DT_40_cultivation)
tidy_EP_DT_40 <- inner_join(tidy_EP_DT_40, tidy_EP_DT_40_edu)
tidy_EP_DT_40 <- inner_join(tidy_EP_DT_40, tidy_EP_DT_40_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_41_affluence <- tidy_EP_chunks_41 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_41_employment <- tidy_EP_chunks_41 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_41_status <- tidy_EP_chunks_41 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_41_cultivation <- tidy_EP_chunks_41 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_41_edu <- tidy_EP_chunks_41 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_41_morality <- tidy_EP_chunks_41 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_41 <- inner_join(tidy_EP_DT_41_affluence, tidy_EP_DT_41_employment)
tidy_EP_DT_41 <- inner_join(tidy_EP_DT_41, tidy_EP_DT_41_status)
tidy_EP_DT_41 <- inner_join(tidy_EP_DT_41, tidy_EP_DT_41_cultivation)
tidy_EP_DT_41 <- inner_join(tidy_EP_DT_41, tidy_EP_DT_41_edu)
tidy_EP_DT_41 <- inner_join(tidy_EP_DT_41, tidy_EP_DT_41_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_42_affluence <- tidy_EP_chunks_42 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_42_employment <- tidy_EP_chunks_42 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_42_status <- tidy_EP_chunks_42 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_42_cultivation <- tidy_EP_chunks_42 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_42_edu <- tidy_EP_chunks_42 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_42_morality <- tidy_EP_chunks_42 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_42 <- inner_join(tidy_EP_DT_42_affluence, tidy_EP_DT_42_employment)
tidy_EP_DT_42 <- inner_join(tidy_EP_DT_42, tidy_EP_DT_42_status)
tidy_EP_DT_42 <- inner_join(tidy_EP_DT_42, tidy_EP_DT_42_cultivation)
tidy_EP_DT_42 <- inner_join(tidy_EP_DT_42, tidy_EP_DT_42_edu)
tidy_EP_DT_42 <- inner_join(tidy_EP_DT_42, tidy_EP_DT_42_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_43_affluence <- tidy_EP_chunks_43 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_43_employment <- tidy_EP_chunks_43 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_43_status <- tidy_EP_chunks_43 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_43_cultivation <- tidy_EP_chunks_43 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_43_edu <- tidy_EP_chunks_43 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_43_morality <- tidy_EP_chunks_43 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_43 <- inner_join(tidy_EP_DT_43_affluence, tidy_EP_DT_43_employment)
tidy_EP_DT_43 <- inner_join(tidy_EP_DT_43, tidy_EP_DT_43_status)
tidy_EP_DT_43 <- inner_join(tidy_EP_DT_43, tidy_EP_DT_43_cultivation)
tidy_EP_DT_43 <- inner_join(tidy_EP_DT_43, tidy_EP_DT_43_edu)
tidy_EP_DT_43 <- inner_join(tidy_EP_DT_43, tidy_EP_DT_43_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_44_affluence <- tidy_EP_chunks_44 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_44_employment <- tidy_EP_chunks_44 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_44_status <- tidy_EP_chunks_44 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_44_cultivation <- tidy_EP_chunks_44 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_44_edu <- tidy_EP_chunks_44 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_44_morality <- tidy_EP_chunks_44 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_44 <- inner_join(tidy_EP_DT_44_affluence, tidy_EP_DT_44_employment)
tidy_EP_DT_44 <- inner_join(tidy_EP_DT_44, tidy_EP_DT_44_status)
tidy_EP_DT_44 <- inner_join(tidy_EP_DT_44, tidy_EP_DT_44_cultivation)
tidy_EP_DT_44 <- inner_join(tidy_EP_DT_44, tidy_EP_DT_44_edu)
tidy_EP_DT_44 <- inner_join(tidy_EP_DT_44, tidy_EP_DT_44_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_45_affluence <- tidy_EP_chunks_45 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_45_employment <- tidy_EP_chunks_45 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_45_status <- tidy_EP_chunks_45 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_45_cultivation <- tidy_EP_chunks_45 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_45_edu <- tidy_EP_chunks_45 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_45_morality <- tidy_EP_chunks_45 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_45 <- inner_join(tidy_EP_DT_45_affluence, tidy_EP_DT_45_employment)
tidy_EP_DT_45 <- inner_join(tidy_EP_DT_45, tidy_EP_DT_45_status)
tidy_EP_DT_45 <- inner_join(tidy_EP_DT_45, tidy_EP_DT_45_cultivation)
tidy_EP_DT_45 <- inner_join(tidy_EP_DT_45, tidy_EP_DT_45_edu)
tidy_EP_DT_45 <- inner_join(tidy_EP_DT_45, tidy_EP_DT_45_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_46_affluence <- tidy_EP_chunks_46 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_46_employment <- tidy_EP_chunks_46 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_46_status <- tidy_EP_chunks_46 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_46_cultivation <- tidy_EP_chunks_46 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_46_edu <- tidy_EP_chunks_46 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_46_morality <- tidy_EP_chunks_46 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_46 <- inner_join(tidy_EP_DT_46_affluence, tidy_EP_DT_46_employment)
tidy_EP_DT_46 <- inner_join(tidy_EP_DT_46, tidy_EP_DT_46_status)
tidy_EP_DT_46 <- inner_join(tidy_EP_DT_46, tidy_EP_DT_46_cultivation)
tidy_EP_DT_46 <- inner_join(tidy_EP_DT_46, tidy_EP_DT_46_edu)
tidy_EP_DT_46 <- inner_join(tidy_EP_DT_46, tidy_EP_DT_46_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_47_affluence <- tidy_EP_chunks_47 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_47_employment <- tidy_EP_chunks_47 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_47_status <- tidy_EP_chunks_47 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_47_cultivation <- tidy_EP_chunks_47 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_47_edu <- tidy_EP_chunks_47 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_47_morality <- tidy_EP_chunks_47 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_47 <- inner_join(tidy_EP_DT_47_affluence, tidy_EP_DT_47_employment)
tidy_EP_DT_47 <- inner_join(tidy_EP_DT_47, tidy_EP_DT_47_status)
tidy_EP_DT_47 <- inner_join(tidy_EP_DT_47, tidy_EP_DT_47_cultivation)
tidy_EP_DT_47 <- inner_join(tidy_EP_DT_47, tidy_EP_DT_47_edu)
tidy_EP_DT_47 <- inner_join(tidy_EP_DT_47, tidy_EP_DT_47_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_48_affluence <- tidy_EP_chunks_48 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_48_employment <- tidy_EP_chunks_48 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_48_status <- tidy_EP_chunks_48 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_48_cultivation <- tidy_EP_chunks_48 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_48_edu <- tidy_EP_chunks_48 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_48_morality <- tidy_EP_chunks_48 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_48 <- inner_join(tidy_EP_DT_48_affluence, tidy_EP_DT_48_employment)
tidy_EP_DT_48 <- inner_join(tidy_EP_DT_48, tidy_EP_DT_48_status)
tidy_EP_DT_48 <- inner_join(tidy_EP_DT_48, tidy_EP_DT_48_cultivation)
tidy_EP_DT_48 <- inner_join(tidy_EP_DT_48, tidy_EP_DT_48_edu)
tidy_EP_DT_48 <- inner_join(tidy_EP_DT_48, tidy_EP_DT_48_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_49_affluence <- tidy_EP_chunks_49 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_49_employment <- tidy_EP_chunks_49 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_49_status <- tidy_EP_chunks_49 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_49_cultivation <- tidy_EP_chunks_49 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_49_edu <- tidy_EP_chunks_49 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_49_morality <- tidy_EP_chunks_49 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_49 <- inner_join(tidy_EP_DT_49_affluence, tidy_EP_DT_49_employment)
tidy_EP_DT_49 <- inner_join(tidy_EP_DT_49, tidy_EP_DT_49_status)
tidy_EP_DT_49 <- inner_join(tidy_EP_DT_49, tidy_EP_DT_49_cultivation)
tidy_EP_DT_49 <- inner_join(tidy_EP_DT_49, tidy_EP_DT_49_edu)
tidy_EP_DT_49 <- inner_join(tidy_EP_DT_49, tidy_EP_DT_49_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_50_affluence <- tidy_EP_chunks_50 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_50_employment <- tidy_EP_chunks_50 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_50_status <- tidy_EP_chunks_50 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_50_cultivation <- tidy_EP_chunks_50 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_50_edu <- tidy_EP_chunks_50 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_50_morality <- tidy_EP_chunks_50 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_50 <- inner_join(tidy_EP_DT_50_affluence, tidy_EP_DT_50_employment)
tidy_EP_DT_50 <- inner_join(tidy_EP_DT_50, tidy_EP_DT_50_status)
tidy_EP_DT_50 <- inner_join(tidy_EP_DT_50, tidy_EP_DT_50_cultivation)
tidy_EP_DT_50 <- inner_join(tidy_EP_DT_50, tidy_EP_DT_50_edu)
tidy_EP_DT_50 <- inner_join(tidy_EP_DT_50, tidy_EP_DT_50_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_51_affluence <- tidy_EP_chunks_51 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_51_employment <- tidy_EP_chunks_51 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_51_status <- tidy_EP_chunks_51 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_51_cultivation <- tidy_EP_chunks_51 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_51_edu <- tidy_EP_chunks_51 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_51_morality <- tidy_EP_chunks_51 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_51 <- inner_join(tidy_EP_DT_51_affluence, tidy_EP_DT_51_employment)
tidy_EP_DT_51 <- inner_join(tidy_EP_DT_51, tidy_EP_DT_51_status)
tidy_EP_DT_51 <- inner_join(tidy_EP_DT_51, tidy_EP_DT_51_cultivation)
tidy_EP_DT_51 <- inner_join(tidy_EP_DT_51, tidy_EP_DT_51_edu)
tidy_EP_DT_51 <- inner_join(tidy_EP_DT_51, tidy_EP_DT_51_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_52_affluence <- tidy_EP_chunks_52 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_52_employment <- tidy_EP_chunks_52 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_52_status <- tidy_EP_chunks_52 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_52_cultivation <- tidy_EP_chunks_52 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_52_edu <- tidy_EP_chunks_52 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_52_morality <- tidy_EP_chunks_52 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_52 <- inner_join(tidy_EP_DT_52_affluence, tidy_EP_DT_52_employment)
tidy_EP_DT_52 <- inner_join(tidy_EP_DT_52, tidy_EP_DT_52_status)
tidy_EP_DT_52 <- inner_join(tidy_EP_DT_52, tidy_EP_DT_52_cultivation)
tidy_EP_DT_52 <- inner_join(tidy_EP_DT_52, tidy_EP_DT_52_edu)
tidy_EP_DT_52 <- inner_join(tidy_EP_DT_52, tidy_EP_DT_52_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_53_affluence <- tidy_EP_chunks_53 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_53_employment <- tidy_EP_chunks_53 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_53_status <- tidy_EP_chunks_53 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_53_cultivation <- tidy_EP_chunks_53 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_53_edu <- tidy_EP_chunks_53 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_53_morality <- tidy_EP_chunks_53 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_53 <- inner_join(tidy_EP_DT_53_affluence, tidy_EP_DT_53_employment)
tidy_EP_DT_53 <- inner_join(tidy_EP_DT_53, tidy_EP_DT_53_status)
tidy_EP_DT_53 <- inner_join(tidy_EP_DT_53, tidy_EP_DT_53_cultivation)
tidy_EP_DT_53 <- inner_join(tidy_EP_DT_53, tidy_EP_DT_53_edu)
tidy_EP_DT_53 <- inner_join(tidy_EP_DT_53, tidy_EP_DT_53_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_54_affluence <- tidy_EP_chunks_54 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_54_employment <- tidy_EP_chunks_54 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_54_status <- tidy_EP_chunks_54 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_54_cultivation <- tidy_EP_chunks_54 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_54_edu <- tidy_EP_chunks_54 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_54_morality <- tidy_EP_chunks_54 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_54 <- inner_join(tidy_EP_DT_54_affluence, tidy_EP_DT_54_employment)
tidy_EP_DT_54 <- inner_join(tidy_EP_DT_54, tidy_EP_DT_54_status)
tidy_EP_DT_54 <- inner_join(tidy_EP_DT_54, tidy_EP_DT_54_cultivation)
tidy_EP_DT_54 <- inner_join(tidy_EP_DT_54, tidy_EP_DT_54_edu)
tidy_EP_DT_54 <- inner_join(tidy_EP_DT_54, tidy_EP_DT_54_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_55_affluence <- tidy_EP_chunks_55 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_55_employment <- tidy_EP_chunks_55 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_55_status <- tidy_EP_chunks_55 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_55_cultivation <- tidy_EP_chunks_55 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_55_edu <- tidy_EP_chunks_55 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_55_morality <- tidy_EP_chunks_55 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_55 <- inner_join(tidy_EP_DT_55_affluence, tidy_EP_DT_55_employment)
tidy_EP_DT_55 <- inner_join(tidy_EP_DT_55, tidy_EP_DT_55_status)
tidy_EP_DT_55 <- inner_join(tidy_EP_DT_55, tidy_EP_DT_55_cultivation)
tidy_EP_DT_55 <- inner_join(tidy_EP_DT_55, tidy_EP_DT_55_edu)
tidy_EP_DT_55 <- inner_join(tidy_EP_DT_55, tidy_EP_DT_55_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_56_affluence <- tidy_EP_chunks_56 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_56_employment <- tidy_EP_chunks_56 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_56_status <- tidy_EP_chunks_56 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_56_cultivation <- tidy_EP_chunks_56 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_56_edu <- tidy_EP_chunks_56 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_56_morality <- tidy_EP_chunks_56 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_56 <- inner_join(tidy_EP_DT_56_affluence, tidy_EP_DT_56_employment)
tidy_EP_DT_56 <- inner_join(tidy_EP_DT_56, tidy_EP_DT_56_status)
tidy_EP_DT_56 <- inner_join(tidy_EP_DT_56, tidy_EP_DT_56_cultivation)
tidy_EP_DT_56 <- inner_join(tidy_EP_DT_56, tidy_EP_DT_56_edu)
tidy_EP_DT_56 <- inner_join(tidy_EP_DT_56, tidy_EP_DT_56_morality)
#-- CHUNK 

##  components of class
tidy_EP_DT_57_affluence <- tidy_EP_chunks_57 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_57_employment <- tidy_EP_chunks_57 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_57_status <- tidy_EP_chunks_57 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_57_cultivation <- tidy_EP_chunks_57 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_57_edu <- tidy_EP_chunks_57 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_57_morality <- tidy_EP_chunks_57 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_57 <- inner_join(tidy_EP_DT_57_affluence, tidy_EP_DT_57_employment)
tidy_EP_DT_57 <- inner_join(tidy_EP_DT_57, tidy_EP_DT_57_status)
tidy_EP_DT_57 <- inner_join(tidy_EP_DT_57, tidy_EP_DT_57_cultivation)
tidy_EP_DT_57 <- inner_join(tidy_EP_DT_57, tidy_EP_DT_57_edu)
tidy_EP_DT_57 <- inner_join(tidy_EP_DT_57, tidy_EP_DT_57_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_58_affluence <- tidy_EP_chunks_58 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_58_employment <- tidy_EP_chunks_58 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_58_status <- tidy_EP_chunks_58 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_58_cultivation <- tidy_EP_chunks_58 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_58_edu <- tidy_EP_chunks_58 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_58_morality <- tidy_EP_chunks_58 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_58 <- inner_join(tidy_EP_DT_58_affluence, tidy_EP_DT_58_employment)
tidy_EP_DT_58 <- inner_join(tidy_EP_DT_58, tidy_EP_DT_58_status)
tidy_EP_DT_58 <- inner_join(tidy_EP_DT_58, tidy_EP_DT_58_cultivation)
tidy_EP_DT_58 <- inner_join(tidy_EP_DT_58, tidy_EP_DT_58_edu)
tidy_EP_DT_58 <- inner_join(tidy_EP_DT_58, tidy_EP_DT_58_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_59_affluence <- tidy_EP_chunks_59 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_59_employment <- tidy_EP_chunks_59 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_59_status <- tidy_EP_chunks_59 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_59_cultivation <- tidy_EP_chunks_59 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_59_edu <- tidy_EP_chunks_59 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_59_morality <- tidy_EP_chunks_59 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_59 <- inner_join(tidy_EP_DT_59_affluence, tidy_EP_DT_59_employment)
tidy_EP_DT_59 <- inner_join(tidy_EP_DT_59, tidy_EP_DT_59_status)
tidy_EP_DT_59 <- inner_join(tidy_EP_DT_59, tidy_EP_DT_59_cultivation)
tidy_EP_DT_59 <- inner_join(tidy_EP_DT_59, tidy_EP_DT_59_edu)
tidy_EP_DT_59 <- inner_join(tidy_EP_DT_59, tidy_EP_DT_59_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_60_affluence <- tidy_EP_chunks_60 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_60_employment <- tidy_EP_chunks_60 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_60_status <- tidy_EP_chunks_60 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_60_cultivation <- tidy_EP_chunks_60 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_60_edu <- tidy_EP_chunks_60 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_60_morality <- tidy_EP_chunks_60 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_60 <- inner_join(tidy_EP_DT_60_affluence, tidy_EP_DT_60_employment)
tidy_EP_DT_60 <- inner_join(tidy_EP_DT_60, tidy_EP_DT_60_status)
tidy_EP_DT_60 <- inner_join(tidy_EP_DT_60, tidy_EP_DT_60_cultivation)
tidy_EP_DT_60 <- inner_join(tidy_EP_DT_60, tidy_EP_DT_60_edu)
tidy_EP_DT_60 <- inner_join(tidy_EP_DT_60, tidy_EP_DT_60_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_61_affluence <- tidy_EP_chunks_61 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_61_employment <- tidy_EP_chunks_61 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_61_status <- tidy_EP_chunks_61 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_61_cultivation <- tidy_EP_chunks_61 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_61_edu <- tidy_EP_chunks_61 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_61_morality <- tidy_EP_chunks_61 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_61 <- inner_join(tidy_EP_DT_61_affluence, tidy_EP_DT_61_employment)
tidy_EP_DT_61 <- inner_join(tidy_EP_DT_61, tidy_EP_DT_61_status)
tidy_EP_DT_61 <- inner_join(tidy_EP_DT_61, tidy_EP_DT_61_cultivation)
tidy_EP_DT_61 <- inner_join(tidy_EP_DT_61, tidy_EP_DT_61_edu)
tidy_EP_DT_61 <- inner_join(tidy_EP_DT_61, tidy_EP_DT_61_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_62_affluence <- tidy_EP_chunks_62 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_62_employment <- tidy_EP_chunks_62 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_62_status <- tidy_EP_chunks_62 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_62_cultivation <- tidy_EP_chunks_62 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_62_edu <- tidy_EP_chunks_62 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_62_morality <- tidy_EP_chunks_62 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_62 <- inner_join(tidy_EP_DT_62_affluence, tidy_EP_DT_62_employment)
tidy_EP_DT_62 <- inner_join(tidy_EP_DT_62, tidy_EP_DT_62_status)
tidy_EP_DT_62 <- inner_join(tidy_EP_DT_62, tidy_EP_DT_62_cultivation)
tidy_EP_DT_62 <- inner_join(tidy_EP_DT_62, tidy_EP_DT_62_edu)
tidy_EP_DT_62 <- inner_join(tidy_EP_DT_62, tidy_EP_DT_62_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_63_affluence <- tidy_EP_chunks_63 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_63_employment <- tidy_EP_chunks_63 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_63_status <- tidy_EP_chunks_63 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_63_cultivation <- tidy_EP_chunks_63 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_63_edu <- tidy_EP_chunks_63 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_63_morality <- tidy_EP_chunks_63 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_63 <- inner_join(tidy_EP_DT_63_affluence, tidy_EP_DT_63_employment)
tidy_EP_DT_63 <- inner_join(tidy_EP_DT_63, tidy_EP_DT_63_status)
tidy_EP_DT_63 <- inner_join(tidy_EP_DT_63, tidy_EP_DT_63_cultivation)
tidy_EP_DT_63 <- inner_join(tidy_EP_DT_63, tidy_EP_DT_63_edu)
tidy_EP_DT_63 <- inner_join(tidy_EP_DT_63, tidy_EP_DT_63_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_64_affluence <- tidy_EP_chunks_64 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_64_employment <- tidy_EP_chunks_64 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_64_status <- tidy_EP_chunks_64 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_64_cultivation <- tidy_EP_chunks_64 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_64_edu <- tidy_EP_chunks_64 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_64_morality <- tidy_EP_chunks_64 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_64 <- inner_join(tidy_EP_DT_64_affluence, tidy_EP_DT_64_employment)
tidy_EP_DT_64 <- inner_join(tidy_EP_DT_64, tidy_EP_DT_64_status)
tidy_EP_DT_64 <- inner_join(tidy_EP_DT_64, tidy_EP_DT_64_cultivation)
tidy_EP_DT_64 <- inner_join(tidy_EP_DT_64, tidy_EP_DT_64_edu)
tidy_EP_DT_64 <- inner_join(tidy_EP_DT_64, tidy_EP_DT_64_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_65_affluence <- tidy_EP_chunks_65 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_65_employment <- tidy_EP_chunks_65 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_65_status <- tidy_EP_chunks_65 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_65_cultivation <- tidy_EP_chunks_65 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_65_edu <- tidy_EP_chunks_65 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_65_morality <- tidy_EP_chunks_65 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_65 <- inner_join(tidy_EP_DT_65_affluence, tidy_EP_DT_65_employment)
tidy_EP_DT_65 <- inner_join(tidy_EP_DT_65, tidy_EP_DT_65_status)
tidy_EP_DT_65 <- inner_join(tidy_EP_DT_65, tidy_EP_DT_65_cultivation)
tidy_EP_DT_65 <- inner_join(tidy_EP_DT_65, tidy_EP_DT_65_edu)
tidy_EP_DT_65 <- inner_join(tidy_EP_DT_65, tidy_EP_DT_65_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_66_affluence <- tidy_EP_chunks_66 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_66_employment <- tidy_EP_chunks_66 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_66_status <- tidy_EP_chunks_66 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_66_cultivation <- tidy_EP_chunks_66 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_66_edu <- tidy_EP_chunks_66 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_66_morality <- tidy_EP_chunks_66 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_66 <- inner_join(tidy_EP_DT_66_affluence, tidy_EP_DT_66_employment)
tidy_EP_DT_66 <- inner_join(tidy_EP_DT_66, tidy_EP_DT_66_status)
tidy_EP_DT_66 <- inner_join(tidy_EP_DT_66, tidy_EP_DT_66_cultivation)
tidy_EP_DT_66 <- inner_join(tidy_EP_DT_66, tidy_EP_DT_66_edu)
tidy_EP_DT_66 <- inner_join(tidy_EP_DT_66, tidy_EP_DT_66_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_67_affluence <- tidy_EP_chunks_67 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_67_employment <- tidy_EP_chunks_67 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_67_status <- tidy_EP_chunks_67 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_67_cultivation <- tidy_EP_chunks_67 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_67_edu <- tidy_EP_chunks_67 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_67_morality <- tidy_EP_chunks_67 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_67 <- inner_join(tidy_EP_DT_67_affluence, tidy_EP_DT_67_employment)
tidy_EP_DT_67 <- inner_join(tidy_EP_DT_67, tidy_EP_DT_67_status)
tidy_EP_DT_67 <- inner_join(tidy_EP_DT_67, tidy_EP_DT_67_cultivation)
tidy_EP_DT_67 <- inner_join(tidy_EP_DT_67, tidy_EP_DT_67_edu)
tidy_EP_DT_67 <- inner_join(tidy_EP_DT_67, tidy_EP_DT_67_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_68_affluence <- tidy_EP_chunks_68 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_68_employment <- tidy_EP_chunks_68 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_68_status <- tidy_EP_chunks_68 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_68_cultivation <- tidy_EP_chunks_68 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_68_edu <- tidy_EP_chunks_68 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_68_morality <- tidy_EP_chunks_68 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_68 <- inner_join(tidy_EP_DT_68_affluence, tidy_EP_DT_68_employment)
tidy_EP_DT_68 <- inner_join(tidy_EP_DT_68, tidy_EP_DT_68_status)
tidy_EP_DT_68 <- inner_join(tidy_EP_DT_68, tidy_EP_DT_68_cultivation)
tidy_EP_DT_68 <- inner_join(tidy_EP_DT_68, tidy_EP_DT_68_edu)
tidy_EP_DT_68 <- inner_join(tidy_EP_DT_68, tidy_EP_DT_68_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_69_affluence <- tidy_EP_chunks_69 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_69_employment <- tidy_EP_chunks_69 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_69_status <- tidy_EP_chunks_69 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_69_cultivation <- tidy_EP_chunks_69 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_69_edu <- tidy_EP_chunks_69 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_69_morality <- tidy_EP_chunks_69 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_69 <- inner_join(tidy_EP_DT_69_affluence, tidy_EP_DT_69_employment)
tidy_EP_DT_69 <- inner_join(tidy_EP_DT_69, tidy_EP_DT_69_status)
tidy_EP_DT_69 <- inner_join(tidy_EP_DT_69, tidy_EP_DT_69_cultivation)
tidy_EP_DT_69 <- inner_join(tidy_EP_DT_69, tidy_EP_DT_69_edu)
tidy_EP_DT_69 <- inner_join(tidy_EP_DT_69, tidy_EP_DT_69_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_70_affluence <- tidy_EP_chunks_70 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_70_employment <- tidy_EP_chunks_70 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_70_status <- tidy_EP_chunks_70 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_70_cultivation <- tidy_EP_chunks_70 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_70_edu <- tidy_EP_chunks_70 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_70_morality <- tidy_EP_chunks_70 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_70 <- inner_join(tidy_EP_DT_70_affluence, tidy_EP_DT_70_employment)
tidy_EP_DT_70 <- inner_join(tidy_EP_DT_70, tidy_EP_DT_70_status)
tidy_EP_DT_70 <- inner_join(tidy_EP_DT_70, tidy_EP_DT_70_cultivation)
tidy_EP_DT_70 <- inner_join(tidy_EP_DT_70, tidy_EP_DT_70_edu)
tidy_EP_DT_70 <- inner_join(tidy_EP_DT_70, tidy_EP_DT_70_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_71_affluence <- tidy_EP_chunks_71 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_71_employment <- tidy_EP_chunks_71 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_71_status <- tidy_EP_chunks_71 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_71_cultivation <- tidy_EP_chunks_71 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_71_edu <- tidy_EP_chunks_71 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_71_morality <- tidy_EP_chunks_71 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_71 <- inner_join(tidy_EP_DT_71_affluence, tidy_EP_DT_71_employment)
tidy_EP_DT_71 <- inner_join(tidy_EP_DT_71, tidy_EP_DT_71_status)
tidy_EP_DT_71 <- inner_join(tidy_EP_DT_71, tidy_EP_DT_71_cultivation)
tidy_EP_DT_71 <- inner_join(tidy_EP_DT_71, tidy_EP_DT_71_edu)
tidy_EP_DT_71 <- inner_join(tidy_EP_DT_71, tidy_EP_DT_71_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_72_affluence <- tidy_EP_chunks_72 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_72_employment <- tidy_EP_chunks_72 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_72_status <- tidy_EP_chunks_72 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_72_cultivation <- tidy_EP_chunks_72 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_72_edu <- tidy_EP_chunks_72 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_72_morality <- tidy_EP_chunks_72 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_72 <- inner_join(tidy_EP_DT_72_affluence, tidy_EP_DT_72_employment)
tidy_EP_DT_72 <- inner_join(tidy_EP_DT_72, tidy_EP_DT_72_status)
tidy_EP_DT_72 <- inner_join(tidy_EP_DT_72, tidy_EP_DT_72_cultivation)
tidy_EP_DT_72 <- inner_join(tidy_EP_DT_72, tidy_EP_DT_72_edu)
tidy_EP_DT_72 <- inner_join(tidy_EP_DT_72, tidy_EP_DT_72_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_73_affluence <- tidy_EP_chunks_73 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_73_employment <- tidy_EP_chunks_73 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_73_status <- tidy_EP_chunks_73 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_73_cultivation <- tidy_EP_chunks_73 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_73_edu <- tidy_EP_chunks_73 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_73_morality <- tidy_EP_chunks_73 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_73 <- inner_join(tidy_EP_DT_73_affluence, tidy_EP_DT_73_employment)
tidy_EP_DT_73 <- inner_join(tidy_EP_DT_73, tidy_EP_DT_73_status)
tidy_EP_DT_73 <- inner_join(tidy_EP_DT_73, tidy_EP_DT_73_cultivation)
tidy_EP_DT_73 <- inner_join(tidy_EP_DT_73, tidy_EP_DT_73_edu)
tidy_EP_DT_73 <- inner_join(tidy_EP_DT_73, tidy_EP_DT_73_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_74_affluence <- tidy_EP_chunks_74 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_74_employment <- tidy_EP_chunks_74 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_74_status <- tidy_EP_chunks_74 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_74_cultivation <- tidy_EP_chunks_74 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_74_edu <- tidy_EP_chunks_74 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_74_morality <- tidy_EP_chunks_74 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_74 <- inner_join(tidy_EP_DT_74_affluence, tidy_EP_DT_74_employment)
tidy_EP_DT_74 <- inner_join(tidy_EP_DT_74, tidy_EP_DT_74_status)
tidy_EP_DT_74 <- inner_join(tidy_EP_DT_74, tidy_EP_DT_74_cultivation)
tidy_EP_DT_74 <- inner_join(tidy_EP_DT_74, tidy_EP_DT_74_edu)
tidy_EP_DT_74 <- inner_join(tidy_EP_DT_74, tidy_EP_DT_74_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_75_affluence <- tidy_EP_chunks_75 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_75_employment <- tidy_EP_chunks_75 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_75_status <- tidy_EP_chunks_75 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_75_cultivation <- tidy_EP_chunks_75 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_75_edu <- tidy_EP_chunks_75 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_75_morality <- tidy_EP_chunks_75 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_75 <- inner_join(tidy_EP_DT_75_affluence, tidy_EP_DT_75_employment)
tidy_EP_DT_75 <- inner_join(tidy_EP_DT_75, tidy_EP_DT_75_status)
tidy_EP_DT_75 <- inner_join(tidy_EP_DT_75, tidy_EP_DT_75_cultivation)
tidy_EP_DT_75 <- inner_join(tidy_EP_DT_75, tidy_EP_DT_75_edu)
tidy_EP_DT_75 <- inner_join(tidy_EP_DT_75, tidy_EP_DT_75_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_76_affluence <- tidy_EP_chunks_76 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_76_employment <- tidy_EP_chunks_76 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_76_status <- tidy_EP_chunks_76 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_76_cultivation <- tidy_EP_chunks_76 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_76_edu <- tidy_EP_chunks_76 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_76_morality <- tidy_EP_chunks_76 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_76 <- inner_join(tidy_EP_DT_76_affluence, tidy_EP_DT_76_employment)
tidy_EP_DT_76 <- inner_join(tidy_EP_DT_76, tidy_EP_DT_76_status)
tidy_EP_DT_76 <- inner_join(tidy_EP_DT_76, tidy_EP_DT_76_cultivation)
tidy_EP_DT_76 <- inner_join(tidy_EP_DT_76, tidy_EP_DT_76_edu)
tidy_EP_DT_76 <- inner_join(tidy_EP_DT_76, tidy_EP_DT_76_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_77_affluence <- tidy_EP_chunks_77 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_77_employment <- tidy_EP_chunks_77 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_77_status <- tidy_EP_chunks_77 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_77_cultivation <- tidy_EP_chunks_77 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_77_edu <- tidy_EP_chunks_77 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_77_morality <- tidy_EP_chunks_77 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_77 <- inner_join(tidy_EP_DT_77_affluence, tidy_EP_DT_77_employment)
tidy_EP_DT_77 <- inner_join(tidy_EP_DT_77, tidy_EP_DT_77_status)
tidy_EP_DT_77 <- inner_join(tidy_EP_DT_77, tidy_EP_DT_77_cultivation)
tidy_EP_DT_77 <- inner_join(tidy_EP_DT_77, tidy_EP_DT_77_edu)
tidy_EP_DT_77 <- inner_join(tidy_EP_DT_77, tidy_EP_DT_77_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_78_affluence <- tidy_EP_chunks_78 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_78_employment <- tidy_EP_chunks_78 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_78_status <- tidy_EP_chunks_78 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_78_cultivation <- tidy_EP_chunks_78 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_78_edu <- tidy_EP_chunks_78 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_78_morality <- tidy_EP_chunks_78 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_78 <- inner_join(tidy_EP_DT_78_affluence, tidy_EP_DT_78_employment)
tidy_EP_DT_78 <- inner_join(tidy_EP_DT_78, tidy_EP_DT_78_status)
tidy_EP_DT_78 <- inner_join(tidy_EP_DT_78, tidy_EP_DT_78_cultivation)
tidy_EP_DT_78 <- inner_join(tidy_EP_DT_78, tidy_EP_DT_78_edu)
tidy_EP_DT_78 <- inner_join(tidy_EP_DT_78, tidy_EP_DT_78_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_79_affluence <- tidy_EP_chunks_79 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_79_employment <- tidy_EP_chunks_79 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_79_status <- tidy_EP_chunks_79 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_79_cultivation <- tidy_EP_chunks_79 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_79_edu <- tidy_EP_chunks_79 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_79_morality <- tidy_EP_chunks_79 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_79 <- inner_join(tidy_EP_DT_79_affluence, tidy_EP_DT_79_employment)
tidy_EP_DT_79 <- inner_join(tidy_EP_DT_79, tidy_EP_DT_79_status)
tidy_EP_DT_79 <- inner_join(tidy_EP_DT_79, tidy_EP_DT_79_cultivation)
tidy_EP_DT_79 <- inner_join(tidy_EP_DT_79, tidy_EP_DT_79_edu)
tidy_EP_DT_79 <- inner_join(tidy_EP_DT_79, tidy_EP_DT_79_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_80_affluence <- tidy_EP_chunks_80 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_80_employment <- tidy_EP_chunks_80 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_80_status <- tidy_EP_chunks_80 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_80_cultivation <- tidy_EP_chunks_80 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_80_edu <- tidy_EP_chunks_80 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_80_morality <- tidy_EP_chunks_80 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_80 <- inner_join(tidy_EP_DT_80_affluence, tidy_EP_DT_80_employment)
tidy_EP_DT_80 <- inner_join(tidy_EP_DT_80, tidy_EP_DT_80_status)
tidy_EP_DT_80 <- inner_join(tidy_EP_DT_80, tidy_EP_DT_80_cultivation)
tidy_EP_DT_80 <- inner_join(tidy_EP_DT_80, tidy_EP_DT_80_edu)
tidy_EP_DT_80 <- inner_join(tidy_EP_DT_80, tidy_EP_DT_80_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_81_affluence <- tidy_EP_chunks_81 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_81_employment <- tidy_EP_chunks_81 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_81_status <- tidy_EP_chunks_81 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_81_cultivation <- tidy_EP_chunks_81 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_81_edu <- tidy_EP_chunks_81 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_81_morality <- tidy_EP_chunks_81 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_81 <- inner_join(tidy_EP_DT_81_affluence, tidy_EP_DT_81_employment)
tidy_EP_DT_81 <- inner_join(tidy_EP_DT_81, tidy_EP_DT_81_status)
tidy_EP_DT_81 <- inner_join(tidy_EP_DT_81, tidy_EP_DT_81_cultivation)
tidy_EP_DT_81 <- inner_join(tidy_EP_DT_81, tidy_EP_DT_81_edu)
tidy_EP_DT_81 <- inner_join(tidy_EP_DT_81, tidy_EP_DT_81_morality)
#-- CHUNK 

##  components of class
tidy_EP_DT_82_affluence <- tidy_EP_chunks_82 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_82_employment <- tidy_EP_chunks_82 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_82_status <- tidy_EP_chunks_82 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_82_cultivation <- tidy_EP_chunks_82 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_82_edu <- tidy_EP_chunks_82 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_82_morality <- tidy_EP_chunks_82 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_82 <- inner_join(tidy_EP_DT_82_affluence, tidy_EP_DT_82_employment)
tidy_EP_DT_82 <- inner_join(tidy_EP_DT_82, tidy_EP_DT_82_status)
tidy_EP_DT_82 <- inner_join(tidy_EP_DT_82, tidy_EP_DT_82_cultivation)
tidy_EP_DT_82 <- inner_join(tidy_EP_DT_82, tidy_EP_DT_82_edu)
tidy_EP_DT_82 <- inner_join(tidy_EP_DT_82, tidy_EP_DT_82_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_83_affluence <- tidy_EP_chunks_83 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_83_employment <- tidy_EP_chunks_83 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_83_status <- tidy_EP_chunks_83 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_83_cultivation <- tidy_EP_chunks_83 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_83_edu <- tidy_EP_chunks_83 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_83_morality <- tidy_EP_chunks_83 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_83 <- inner_join(tidy_EP_DT_83_affluence, tidy_EP_DT_83_employment)
tidy_EP_DT_83 <- inner_join(tidy_EP_DT_83, tidy_EP_DT_83_status)
tidy_EP_DT_83 <- inner_join(tidy_EP_DT_83, tidy_EP_DT_83_cultivation)
tidy_EP_DT_83 <- inner_join(tidy_EP_DT_83, tidy_EP_DT_83_edu)
tidy_EP_DT_83 <- inner_join(tidy_EP_DT_83, tidy_EP_DT_83_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_84_affluence <- tidy_EP_chunks_84 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_84_employment <- tidy_EP_chunks_84 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_84_status <- tidy_EP_chunks_84 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_84_cultivation <- tidy_EP_chunks_84 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_84_edu <- tidy_EP_chunks_84 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_84_morality <- tidy_EP_chunks_84 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_84 <- inner_join(tidy_EP_DT_84_affluence, tidy_EP_DT_84_employment)
tidy_EP_DT_84 <- inner_join(tidy_EP_DT_84, tidy_EP_DT_84_status)
tidy_EP_DT_84 <- inner_join(tidy_EP_DT_84, tidy_EP_DT_84_cultivation)
tidy_EP_DT_84 <- inner_join(tidy_EP_DT_84, tidy_EP_DT_84_edu)
tidy_EP_DT_84 <- inner_join(tidy_EP_DT_84, tidy_EP_DT_84_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_85_affluence <- tidy_EP_chunks_85 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_85_employment <- tidy_EP_chunks_85 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_85_status <- tidy_EP_chunks_85 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_85_cultivation <- tidy_EP_chunks_85 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_85_edu <- tidy_EP_chunks_85 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_85_morality <- tidy_EP_chunks_85 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_85 <- inner_join(tidy_EP_DT_85_affluence, tidy_EP_DT_85_employment)
tidy_EP_DT_85 <- inner_join(tidy_EP_DT_85, tidy_EP_DT_85_status)
tidy_EP_DT_85 <- inner_join(tidy_EP_DT_85, tidy_EP_DT_85_cultivation)
tidy_EP_DT_85 <- inner_join(tidy_EP_DT_85, tidy_EP_DT_85_edu)
tidy_EP_DT_85 <- inner_join(tidy_EP_DT_85, tidy_EP_DT_85_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_86_affluence <- tidy_EP_chunks_86 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_86_employment <- tidy_EP_chunks_86 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_86_status <- tidy_EP_chunks_86 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_86_cultivation <- tidy_EP_chunks_86 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_86_edu <- tidy_EP_chunks_86 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_86_morality <- tidy_EP_chunks_86 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_86 <- inner_join(tidy_EP_DT_86_affluence, tidy_EP_DT_86_employment)
tidy_EP_DT_86 <- inner_join(tidy_EP_DT_86, tidy_EP_DT_86_status)
tidy_EP_DT_86 <- inner_join(tidy_EP_DT_86, tidy_EP_DT_86_cultivation)
tidy_EP_DT_86 <- inner_join(tidy_EP_DT_86, tidy_EP_DT_86_edu)
tidy_EP_DT_86 <- inner_join(tidy_EP_DT_86, tidy_EP_DT_86_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_87_affluence <- tidy_EP_chunks_87 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_87_employment <- tidy_EP_chunks_87 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_87_status <- tidy_EP_chunks_87 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_87_cultivation <- tidy_EP_chunks_87 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_87_edu <- tidy_EP_chunks_87 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_87_morality <- tidy_EP_chunks_87 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_87 <- inner_join(tidy_EP_DT_87_affluence, tidy_EP_DT_87_employment)
tidy_EP_DT_87 <- inner_join(tidy_EP_DT_87, tidy_EP_DT_87_status)
tidy_EP_DT_87 <- inner_join(tidy_EP_DT_87, tidy_EP_DT_87_cultivation)
tidy_EP_DT_87 <- inner_join(tidy_EP_DT_87, tidy_EP_DT_87_edu)
tidy_EP_DT_87 <- inner_join(tidy_EP_DT_87, tidy_EP_DT_87_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_88_affluence <- tidy_EP_chunks_88 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_88_employment <- tidy_EP_chunks_88 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_88_status <- tidy_EP_chunks_88 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_88_cultivation <- tidy_EP_chunks_88 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_88_edu <- tidy_EP_chunks_88 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_88_morality <- tidy_EP_chunks_88 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_88 <- inner_join(tidy_EP_DT_88_affluence, tidy_EP_DT_88_employment)
tidy_EP_DT_88 <- inner_join(tidy_EP_DT_88, tidy_EP_DT_88_status)
tidy_EP_DT_88 <- inner_join(tidy_EP_DT_88, tidy_EP_DT_88_cultivation)
tidy_EP_DT_88 <- inner_join(tidy_EP_DT_88, tidy_EP_DT_88_edu)
tidy_EP_DT_88 <- inner_join(tidy_EP_DT_88, tidy_EP_DT_88_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_89_affluence <- tidy_EP_chunks_89 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_89_employment <- tidy_EP_chunks_89 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_89_status <- tidy_EP_chunks_89 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_89_cultivation <- tidy_EP_chunks_89 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_89_edu <- tidy_EP_chunks_89 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_89_morality <- tidy_EP_chunks_89 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_89 <- inner_join(tidy_EP_DT_89_affluence, tidy_EP_DT_89_employment)
tidy_EP_DT_89 <- inner_join(tidy_EP_DT_89, tidy_EP_DT_89_status)
tidy_EP_DT_89 <- inner_join(tidy_EP_DT_89, tidy_EP_DT_89_cultivation)
tidy_EP_DT_89 <- inner_join(tidy_EP_DT_89, tidy_EP_DT_89_edu)
tidy_EP_DT_89 <- inner_join(tidy_EP_DT_89, tidy_EP_DT_89_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_90_affluence <- tidy_EP_chunks_90 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_90_employment <- tidy_EP_chunks_90 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_90_status <- tidy_EP_chunks_90 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_90_cultivation <- tidy_EP_chunks_90 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_90_edu <- tidy_EP_chunks_90 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_90_morality <- tidy_EP_chunks_90 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_90 <- inner_join(tidy_EP_DT_90_affluence, tidy_EP_DT_90_employment)
tidy_EP_DT_90 <- inner_join(tidy_EP_DT_90, tidy_EP_DT_90_status)
tidy_EP_DT_90 <- inner_join(tidy_EP_DT_90, tidy_EP_DT_90_cultivation)
tidy_EP_DT_90 <- inner_join(tidy_EP_DT_90, tidy_EP_DT_90_edu)
tidy_EP_DT_90 <- inner_join(tidy_EP_DT_90, tidy_EP_DT_90_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_91_affluence <- tidy_EP_chunks_91 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_91_employment <- tidy_EP_chunks_91 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_91_status <- tidy_EP_chunks_91 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_91_cultivation <- tidy_EP_chunks_91 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_91_edu <- tidy_EP_chunks_91 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_91_morality <- tidy_EP_chunks_91 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_91 <- inner_join(tidy_EP_DT_91_affluence, tidy_EP_DT_91_employment)
tidy_EP_DT_91 <- inner_join(tidy_EP_DT_91, tidy_EP_DT_91_status)
tidy_EP_DT_91 <- inner_join(tidy_EP_DT_91, tidy_EP_DT_91_cultivation)
tidy_EP_DT_91 <- inner_join(tidy_EP_DT_91, tidy_EP_DT_91_edu)
tidy_EP_DT_91 <- inner_join(tidy_EP_DT_91, tidy_EP_DT_91_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_92_affluence <- tidy_EP_chunks_92 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_92_employment <- tidy_EP_chunks_92 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_92_status <- tidy_EP_chunks_92 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_92_cultivation <- tidy_EP_chunks_92 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_92_edu <- tidy_EP_chunks_92 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_92_morality <- tidy_EP_chunks_92 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_92 <- inner_join(tidy_EP_DT_92_affluence, tidy_EP_DT_92_employment)
tidy_EP_DT_92 <- inner_join(tidy_EP_DT_92, tidy_EP_DT_92_status)
tidy_EP_DT_92 <- inner_join(tidy_EP_DT_92, tidy_EP_DT_92_cultivation)
tidy_EP_DT_92 <- inner_join(tidy_EP_DT_92, tidy_EP_DT_92_edu)
tidy_EP_DT_92 <- inner_join(tidy_EP_DT_92, tidy_EP_DT_92_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_93_affluence <- tidy_EP_chunks_93 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_93_employment <- tidy_EP_chunks_93 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_93_status <- tidy_EP_chunks_93 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_93_cultivation <- tidy_EP_chunks_93 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_93_edu <- tidy_EP_chunks_93 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_93_morality <- tidy_EP_chunks_93 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_93 <- inner_join(tidy_EP_DT_93_affluence, tidy_EP_DT_93_employment)
tidy_EP_DT_93 <- inner_join(tidy_EP_DT_93, tidy_EP_DT_93_status)
tidy_EP_DT_93 <- inner_join(tidy_EP_DT_93, tidy_EP_DT_93_cultivation)
tidy_EP_DT_93 <- inner_join(tidy_EP_DT_93, tidy_EP_DT_93_edu)
tidy_EP_DT_93 <- inner_join(tidy_EP_DT_93, tidy_EP_DT_93_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_94_affluence <- tidy_EP_chunks_94 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_94_employment <- tidy_EP_chunks_94 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_94_status <- tidy_EP_chunks_94 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_94_cultivation <- tidy_EP_chunks_94 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_94_edu <- tidy_EP_chunks_94 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_94_morality <- tidy_EP_chunks_94 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_94 <- inner_join(tidy_EP_DT_94_affluence, tidy_EP_DT_94_employment)
tidy_EP_DT_94 <- inner_join(tidy_EP_DT_94, tidy_EP_DT_94_status)
tidy_EP_DT_94 <- inner_join(tidy_EP_DT_94, tidy_EP_DT_94_cultivation)
tidy_EP_DT_94 <- inner_join(tidy_EP_DT_94, tidy_EP_DT_94_edu)
tidy_EP_DT_94 <- inner_join(tidy_EP_DT_94, tidy_EP_DT_94_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_95_affluence <- tidy_EP_chunks_95 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_95_employment <- tidy_EP_chunks_95 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_95_status <- tidy_EP_chunks_95 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_95_cultivation <- tidy_EP_chunks_95 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_95_edu <- tidy_EP_chunks_95 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_95_morality <- tidy_EP_chunks_95 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_95 <- inner_join(tidy_EP_DT_95_affluence, tidy_EP_DT_95_employment)
tidy_EP_DT_95 <- inner_join(tidy_EP_DT_95, tidy_EP_DT_95_status)
tidy_EP_DT_95 <- inner_join(tidy_EP_DT_95, tidy_EP_DT_95_cultivation)
tidy_EP_DT_95 <- inner_join(tidy_EP_DT_95, tidy_EP_DT_95_edu)
tidy_EP_DT_95 <- inner_join(tidy_EP_DT_95, tidy_EP_DT_95_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_96_affluence <- tidy_EP_chunks_96 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_96_employment <- tidy_EP_chunks_96 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_96_status <- tidy_EP_chunks_96 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_96_cultivation <- tidy_EP_chunks_96 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_96_edu <- tidy_EP_chunks_96 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_96_morality <- tidy_EP_chunks_96 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_96 <- inner_join(tidy_EP_DT_96_affluence, tidy_EP_DT_96_employment)
tidy_EP_DT_96 <- inner_join(tidy_EP_DT_96, tidy_EP_DT_96_status)
tidy_EP_DT_96 <- inner_join(tidy_EP_DT_96, tidy_EP_DT_96_cultivation)
tidy_EP_DT_96 <- inner_join(tidy_EP_DT_96, tidy_EP_DT_96_edu)
tidy_EP_DT_96 <- inner_join(tidy_EP_DT_96, tidy_EP_DT_96_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_97_affluence <- tidy_EP_chunks_97 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_97_employment <- tidy_EP_chunks_97 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_97_status <- tidy_EP_chunks_97 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_97_cultivation <- tidy_EP_chunks_97 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_97_edu <- tidy_EP_chunks_97 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_97_morality <- tidy_EP_chunks_97 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_97 <- inner_join(tidy_EP_DT_97_affluence, tidy_EP_DT_97_employment)
tidy_EP_DT_97 <- inner_join(tidy_EP_DT_97, tidy_EP_DT_97_status)
tidy_EP_DT_97 <- inner_join(tidy_EP_DT_97, tidy_EP_DT_97_cultivation)
tidy_EP_DT_97 <- inner_join(tidy_EP_DT_97, tidy_EP_DT_97_edu)
tidy_EP_DT_97 <- inner_join(tidy_EP_DT_97, tidy_EP_DT_97_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_98_affluence <- tidy_EP_chunks_98 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_98_employment <- tidy_EP_chunks_98 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_98_status <- tidy_EP_chunks_98 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_98_cultivation <- tidy_EP_chunks_98 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_98_edu <- tidy_EP_chunks_98 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_98_morality <- tidy_EP_chunks_98 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_98 <- inner_join(tidy_EP_DT_98_affluence, tidy_EP_DT_98_employment)
tidy_EP_DT_98 <- inner_join(tidy_EP_DT_98, tidy_EP_DT_98_status)
tidy_EP_DT_98 <- inner_join(tidy_EP_DT_98, tidy_EP_DT_98_cultivation)
tidy_EP_DT_98 <- inner_join(tidy_EP_DT_98, tidy_EP_DT_98_edu)
tidy_EP_DT_98 <- inner_join(tidy_EP_DT_98, tidy_EP_DT_98_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_99_affluence <- tidy_EP_chunks_99 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_99_employment <- tidy_EP_chunks_99 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_99_status <- tidy_EP_chunks_99 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_99_cultivation <- tidy_EP_chunks_99 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_99_edu <- tidy_EP_chunks_99 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_99_morality <- tidy_EP_chunks_99 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_99 <- inner_join(tidy_EP_DT_99_affluence, tidy_EP_DT_99_employment)
tidy_EP_DT_99 <- inner_join(tidy_EP_DT_99, tidy_EP_DT_99_status)
tidy_EP_DT_99 <- inner_join(tidy_EP_DT_99, tidy_EP_DT_99_cultivation)
tidy_EP_DT_99 <- inner_join(tidy_EP_DT_99, tidy_EP_DT_99_edu)
tidy_EP_DT_99 <- inner_join(tidy_EP_DT_99, tidy_EP_DT_99_morality)

#-- CHUNK 

##  components of class
tidy_EP_DT_100_affluence <- tidy_EP_chunks_100 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = affluence, wv=ngram1908_33)

## class continued

tidy_EP_DT_100_employment <- tidy_EP_chunks_100 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = employment, wv=ngram1908_33)

tidy_EP_DT_100_status <- tidy_EP_chunks_100 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = status, wv=ngram1908_33)

tidy_EP_DT_100_cultivation <- tidy_EP_chunks_100 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = cultivation, wv=ngram1908_33)

tidy_EP_DT_100_edu <- tidy_EP_chunks_100 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = education, wv=ngram1908_33)

tidy_EP_DT_100_morality <- tidy_EP_chunks_100 %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = morality, wv=ngram1908_33)

### merge all into meta data

tidy_EP_DT_100 <- inner_join(tidy_EP_DT_100_affluence, tidy_EP_DT_100_employment)
tidy_EP_DT_100 <- inner_join(tidy_EP_DT_100, tidy_EP_DT_100_status)
tidy_EP_DT_100 <- inner_join(tidy_EP_DT_100, tidy_EP_DT_100_cultivation)
tidy_EP_DT_100 <- inner_join(tidy_EP_DT_100, tidy_EP_DT_100_edu)
tidy_EP_DT_100 <- inner_join(tidy_EP_DT_100, tidy_EP_DT_100_morality)



############### Merge 100 chunks

tidy_EP_DT_1 <- mutate(tidy_EP_DT_1, unique = as.integer(docs)*1000 + 1)
tidy_EP_DT_2 <- mutate(tidy_EP_DT_2, unique = as.integer(docs)*1000 + 2)
tidy_EP_DT_3 <- mutate(tidy_EP_DT_3, unique = as.integer(docs)*1000 + 3)
tidy_EP_DT_4 <- mutate(tidy_EP_DT_4, unique = as.integer(docs)*1000 + 4)
tidy_EP_DT_5 <- mutate(tidy_EP_DT_5, unique = as.integer(docs)*1000 + 5)
tidy_EP_DT_6 <- mutate(tidy_EP_DT_6, unique = as.integer(docs)*1000 + 6)
tidy_EP_DT_7 <- mutate(tidy_EP_DT_7, unique = as.integer(docs)*1000 + 7)
tidy_EP_DT_8 <- mutate(tidy_EP_DT_8, unique = as.integer(docs)*1000 + 8)
tidy_EP_DT_9 <- mutate(tidy_EP_DT_9, unique = as.integer(docs)*1000 + 9)

tidy_EP_DT_10 <- mutate(tidy_EP_DT_10, unique = as.integer(docs)*1000 + 10)

tidy_EP_DT_11 <- mutate(tidy_EP_DT_11, unique = as.integer(docs)*1000 + 11)
tidy_EP_DT_12 <- mutate(tidy_EP_DT_12, unique = as.integer(docs)*1000 + 12)
tidy_EP_DT_13 <- mutate(tidy_EP_DT_13, unique = as.integer(docs)*1000 + 13)
tidy_EP_DT_14 <- mutate(tidy_EP_DT_14, unique = as.integer(docs)*1000 + 14)
tidy_EP_DT_15 <- mutate(tidy_EP_DT_15, unique = as.integer(docs)*1000 + 15)
tidy_EP_DT_16 <- mutate(tidy_EP_DT_16, unique = as.integer(docs)*1000 + 16)
tidy_EP_DT_17 <- mutate(tidy_EP_DT_17, unique = as.integer(docs)*1000 + 17)
tidy_EP_DT_18 <- mutate(tidy_EP_DT_18, unique = as.integer(docs)*1000 + 18)
tidy_EP_DT_19 <- mutate(tidy_EP_DT_19, unique = as.integer(docs)*1000 + 19)

tidy_EP_DT_20 <- mutate(tidy_EP_DT_20, unique = as.integer(docs)*1000 + 20)

tidy_EP_DT_21 <- mutate(tidy_EP_DT_21, unique = as.integer(docs)*1000 + 21)
tidy_EP_DT_22 <- mutate(tidy_EP_DT_22, unique = as.integer(docs)*1000 + 22)
tidy_EP_DT_23 <- mutate(tidy_EP_DT_23, unique = as.integer(docs)*1000 + 23)
tidy_EP_DT_24 <- mutate(tidy_EP_DT_24, unique = as.integer(docs)*1000 + 24)
tidy_EP_DT_25 <- mutate(tidy_EP_DT_25, unique = as.integer(docs)*1000 + 25)
tidy_EP_DT_26 <- mutate(tidy_EP_DT_26, unique = as.integer(docs)*1000 + 26)
tidy_EP_DT_27 <- mutate(tidy_EP_DT_27, unique = as.integer(docs)*1000 + 27)
tidy_EP_DT_28 <- mutate(tidy_EP_DT_28, unique = as.integer(docs)*1000 + 28)
tidy_EP_DT_29 <- mutate(tidy_EP_DT_29, unique = as.integer(docs)*1000 + 29)

tidy_EP_DT_30 <- mutate(tidy_EP_DT_30, unique = as.integer(docs)*1000 + 30)

tidy_EP_DT_31 <- mutate(tidy_EP_DT_31, unique = as.integer(docs)*1000 + 31)
tidy_EP_DT_32 <- mutate(tidy_EP_DT_32, unique = as.integer(docs)*1000 + 32)
tidy_EP_DT_33 <- mutate(tidy_EP_DT_33, unique = as.integer(docs)*1000 + 33)
tidy_EP_DT_34 <- mutate(tidy_EP_DT_34, unique = as.integer(docs)*1000 + 34)
tidy_EP_DT_35 <- mutate(tidy_EP_DT_35, unique = as.integer(docs)*1000 + 35)
tidy_EP_DT_36 <- mutate(tidy_EP_DT_36, unique = as.integer(docs)*1000 + 36)
tidy_EP_DT_37 <- mutate(tidy_EP_DT_37, unique = as.integer(docs)*1000 + 37)
tidy_EP_DT_38 <- mutate(tidy_EP_DT_38, unique = as.integer(docs)*1000 + 38)
tidy_EP_DT_39 <- mutate(tidy_EP_DT_39, unique = as.integer(docs)*1000 + 39)

tidy_EP_DT_40 <- mutate(tidy_EP_DT_40, unique = as.integer(docs)*1000 + 40)

tidy_EP_DT_41 <- mutate(tidy_EP_DT_41, unique = as.integer(docs)*1000 + 41)
tidy_EP_DT_42 <- mutate(tidy_EP_DT_42, unique = as.integer(docs)*1000 + 42)
tidy_EP_DT_43 <- mutate(tidy_EP_DT_43, unique = as.integer(docs)*1000 + 43)
tidy_EP_DT_44 <- mutate(tidy_EP_DT_44, unique = as.integer(docs)*1000 + 44)
tidy_EP_DT_45 <- mutate(tidy_EP_DT_45, unique = as.integer(docs)*1000 + 45)
tidy_EP_DT_46 <- mutate(tidy_EP_DT_46, unique = as.integer(docs)*1000 + 46)
tidy_EP_DT_47 <- mutate(tidy_EP_DT_47, unique = as.integer(docs)*1000 + 47)
tidy_EP_DT_48 <- mutate(tidy_EP_DT_48, unique = as.integer(docs)*1000 + 48)
tidy_EP_DT_49 <- mutate(tidy_EP_DT_49, unique = as.integer(docs)*1000 + 49)

tidy_EP_DT_50 <- mutate(tidy_EP_DT_50, unique = as.integer(docs)*1000 + 50)

tidy_EP_DT_51 <- mutate(tidy_EP_DT_51, unique = as.integer(docs)*1000 + 51)
tidy_EP_DT_52 <- mutate(tidy_EP_DT_52, unique = as.integer(docs)*1000 + 52)
tidy_EP_DT_53 <- mutate(tidy_EP_DT_53, unique = as.integer(docs)*1000 + 53)
tidy_EP_DT_54 <- mutate(tidy_EP_DT_54, unique = as.integer(docs)*1000 + 54)
tidy_EP_DT_55 <- mutate(tidy_EP_DT_55, unique = as.integer(docs)*1000 + 55)
tidy_EP_DT_56 <- mutate(tidy_EP_DT_56, unique = as.integer(docs)*1000 + 56)
tidy_EP_DT_57 <- mutate(tidy_EP_DT_57, unique = as.integer(docs)*1000 + 57)
tidy_EP_DT_58 <- mutate(tidy_EP_DT_58, unique = as.integer(docs)*1000 + 58)
tidy_EP_DT_59 <- mutate(tidy_EP_DT_59, unique = as.integer(docs)*1000 + 59)

tidy_EP_DT_60 <- mutate(tidy_EP_DT_60, unique = as.integer(docs)*1000 + 60)

tidy_EP_DT_61 <- mutate(tidy_EP_DT_61, unique = as.integer(docs)*1000 + 61)
tidy_EP_DT_62 <- mutate(tidy_EP_DT_62, unique = as.integer(docs)*1000 + 62)
tidy_EP_DT_63 <- mutate(tidy_EP_DT_63, unique = as.integer(docs)*1000 + 63)
tidy_EP_DT_64 <- mutate(tidy_EP_DT_64, unique = as.integer(docs)*1000 + 64)
tidy_EP_DT_65 <- mutate(tidy_EP_DT_65, unique = as.integer(docs)*1000 + 65)
tidy_EP_DT_66 <- mutate(tidy_EP_DT_66, unique = as.integer(docs)*1000 + 66)
tidy_EP_DT_67 <- mutate(tidy_EP_DT_67, unique = as.integer(docs)*1000 + 67)
tidy_EP_DT_68 <- mutate(tidy_EP_DT_68, unique = as.integer(docs)*1000 + 68)
tidy_EP_DT_69 <- mutate(tidy_EP_DT_69, unique = as.integer(docs)*1000 + 69)

tidy_EP_DT_70 <- mutate(tidy_EP_DT_70, unique = as.integer(docs)*1000 + 70)

tidy_EP_DT_71 <- mutate(tidy_EP_DT_71, unique = as.integer(docs)*1000 + 71)
tidy_EP_DT_72 <- mutate(tidy_EP_DT_72, unique = as.integer(docs)*1000 + 72)
tidy_EP_DT_73 <- mutate(tidy_EP_DT_73, unique = as.integer(docs)*1000 + 73)
tidy_EP_DT_74 <- mutate(tidy_EP_DT_74, unique = as.integer(docs)*1000 + 74)
tidy_EP_DT_75 <- mutate(tidy_EP_DT_75, unique = as.integer(docs)*1000 + 75)
tidy_EP_DT_76 <- mutate(tidy_EP_DT_76, unique = as.integer(docs)*1000 + 76)
tidy_EP_DT_77 <- mutate(tidy_EP_DT_77, unique = as.integer(docs)*1000 + 77)
tidy_EP_DT_78 <- mutate(tidy_EP_DT_78, unique = as.integer(docs)*1000 + 78)
tidy_EP_DT_79 <- mutate(tidy_EP_DT_79, unique = as.integer(docs)*1000 + 79)

tidy_EP_DT_80 <- mutate(tidy_EP_DT_80, unique = as.integer(docs)*1000 + 80)

tidy_EP_DT_81 <- mutate(tidy_EP_DT_81, unique = as.integer(docs)*1000 + 81)
tidy_EP_DT_82 <- mutate(tidy_EP_DT_82, unique = as.integer(docs)*1000 + 82)
tidy_EP_DT_83 <- mutate(tidy_EP_DT_83, unique = as.integer(docs)*1000 + 83)
tidy_EP_DT_84 <- mutate(tidy_EP_DT_84, unique = as.integer(docs)*1000 + 84)
tidy_EP_DT_85 <- mutate(tidy_EP_DT_85, unique = as.integer(docs)*1000 + 85)
tidy_EP_DT_86 <- mutate(tidy_EP_DT_86, unique = as.integer(docs)*1000 + 86)
tidy_EP_DT_87 <- mutate(tidy_EP_DT_87, unique = as.integer(docs)*1000 + 87)
tidy_EP_DT_88 <- mutate(tidy_EP_DT_88, unique = as.integer(docs)*1000 + 88)
tidy_EP_DT_89 <- mutate(tidy_EP_DT_89, unique = as.integer(docs)*1000 + 89)

tidy_EP_DT_90 <- mutate(tidy_EP_DT_90, unique = as.integer(docs)*1000 + 90)

tidy_EP_DT_91 <- mutate(tidy_EP_DT_91, unique = as.integer(docs)*1000 + 91)
tidy_EP_DT_92 <- mutate(tidy_EP_DT_92, unique = as.integer(docs)*1000 + 92)
tidy_EP_DT_93 <- mutate(tidy_EP_DT_93, unique = as.integer(docs)*1000 + 93)
tidy_EP_DT_94 <- mutate(tidy_EP_DT_94, unique = as.integer(docs)*1000 + 94)
tidy_EP_DT_95 <- mutate(tidy_EP_DT_95, unique = as.integer(docs)*1000 + 95)
tidy_EP_DT_96 <- mutate(tidy_EP_DT_96, unique = as.integer(docs)*1000 + 96)
tidy_EP_DT_97 <- mutate(tidy_EP_DT_97, unique = as.integer(docs)*1000 + 97)
tidy_EP_DT_98 <- mutate(tidy_EP_DT_98, unique = as.integer(docs)*1000 + 98)
tidy_EP_DT_99 <- mutate(tidy_EP_DT_99, unique = as.integer(docs)*1000 + 99)

tidy_EP_DT_100 <- mutate(tidy_EP_DT_100, unique = as.integer(docs)*1000 + 100)



## merge all

tidy_EP_DT <- full_join(tidy_EP_DT_1, tidy_EP_DT_2)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_3)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_4)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_5)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_6)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_7)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_8)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_9)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_10)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_11)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_12)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_13)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_14)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_15)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_16)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_17)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_18)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_19)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_20)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_21)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_22)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_23)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_24)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_25)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_26)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_27)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_28)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_29)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_30)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_31)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_32)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_33)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_34)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_35)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_36)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_37)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_38)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_39)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_40)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_41)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_42)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_43)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_44)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_45)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_46)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_47)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_48)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_49)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_50)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_51)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_52)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_53)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_54)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_55)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_56)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_57)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_58)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_59)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_60)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_61)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_62)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_63)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_64)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_65)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_66)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_67)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_68)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_69)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_70)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_71)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_72)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_73)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_74)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_75)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_76)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_77)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_78)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_79)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_80)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_81)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_82)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_83)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_84)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_85)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_86)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_87)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_88)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_89)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_90)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_91)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_92)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_93)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_94)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_95)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_96)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_97)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_98)
tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_99)

tidy_EP_DT <- full_join(tidy_EP_DT, tidy_EP_DT_100)



##  export to CSV

write.table(tidy_EP_DT, "kline(2022_5_17)EP-MOC_chunks_ngram1908_33.csv", row.names = FALSE)







## 22 Document similarity score: TABLE 1



similarity <- source_EP %>% 
  mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = replace_white(text)) %>%
  mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
  mutate(text = replace_number(text))  %>%
  filter(!str_detect(text, "[0-9]+") ) 


similarity$text_clean = similarity$text







similarity <- mutate(similarity, filename =  
              ifelse(grepl("txt_2020_12_3/1922EditionFull.txt", filename), "1922",
              ifelse(grepl("txt_2020_12_3/1927EditionFull.txt", filename), "1927",
              ifelse(grepl("txt_2020_12_3/1931EditionFull.txt", filename), "1931",
              ifelse(grepl("txt_2020_12_3/1934EditionFull.txt", filename), "1934",
              ifelse(grepl("txt_2020_12_3/1937EditionFull.txt", filename), "1937",
              ifelse(grepl("txt_2020_12_3/1940EditionFull.txt", filename), "1940",
              ifelse(grepl("txt_2020_12_3/1942EditionFull.txt", filename), "1942",
              ifelse(grepl("txt_2020_12_3/1945EditionFull.txt", filename), "1945",
              ifelse(grepl("txt_2020_12_3/1950EditionFull.txt", filename), "1950",
              ifelse(grepl("txt_2020_12_3/1956EditionFull.txt", filename), "1956",
              ifelse(grepl("txt_2020_12_3/1960EditionFull.txt", filename), "1960",
              ifelse(grepl("txt_2020_12_3/1965EditionFull.txt", filename), "1965",
              ifelse(grepl("txt_2020_12_3/1969EditionFull.txt", filename), "1969",
              ifelse(grepl("txt_2020_12_3/1975EditionFull.txt", filename), "1975",
              ifelse(grepl("txt_2020_12_3/1984EditionFull.txt", filename), "1984",
              ifelse(grepl("txt_2020_12_3/1992EditionFull.txt", filename), "1992",
              ifelse(grepl("txt_2020_12_3/1997EditionFull.txt", filename), "1997",
              ifelse(grepl("txt_2020_12_3/2004EditionFull.txt", filename), "2004",
              ifelse(grepl("txt_2020_12_3/2011EditionFull.txt", filename), "2011",
              ifelse(grepl("txt_2020_12_3/2017EditionFull.txt", filename), "2017","x")))))))))))))))))))))

##  split


similarity_1922 <- filter(similarity, filename == "1922")
similarity_1927 <- filter(similarity, filename == "1927")
similarity_1931 <- filter(similarity, filename == "1931")
similarity_1934 <- filter(similarity, filename == "1934")
similarity_1937 <- filter(similarity, filename == "1937")
similarity_1940 <- filter(similarity, filename == "1940")
similarity_1942 <- filter(similarity, filename == "1942")
similarity_1945 <- filter(similarity, filename == "1945")
similarity_1950 <- filter(similarity, filename == "1950")
similarity_1956 <- filter(similarity, filename == "1956")
similarity_1960 <- filter(similarity, filename == "1960")
similarity_1965 <- filter(similarity, filename == "1965")
similarity_1969 <- filter(similarity, filename == "1969")
similarity_1975 <- filter(similarity, filename == "1975")
similarity_1984 <- filter(similarity, filename == "1984")
similarity_1992 <- filter(similarity, filename == "1992")
similarity_1997 <- filter(similarity, filename == "1997")
similarity_2004 <- filter(similarity, filename == "2004")
similarity_2011 <- filter(similarity, filename == "2011")
similarity_2017 <- filter(similarity, filename == "2017")

##  token
it1922 <- itoken(similarity_1922$text_clean, progressbar = FALSE)
it1927 <- itoken(similarity_1927$text_clean, progressbar = FALSE)
it1931 <- itoken(similarity_1931$text_clean, progressbar = FALSE)
it1934 <- itoken(similarity_1934$text_clean, progressbar = FALSE)
it1937 <- itoken(similarity_1937$text_clean, progressbar = FALSE)
it1940 <- itoken(similarity_1940$text_clean, progressbar = FALSE)
it1942 <- itoken(similarity_1942$text_clean, progressbar = FALSE)
it1945 <- itoken(similarity_1945$text_clean, progressbar = FALSE)
it1950 <- itoken(similarity_1950$text_clean, progressbar = FALSE)
it1956 <- itoken(similarity_1956$text_clean, progressbar = FALSE)
it1960 <- itoken(similarity_1960$text_clean, progressbar = FALSE)
it1965 <- itoken(similarity_1965$text_clean, progressbar = FALSE)
it1969 <- itoken(similarity_1969$text_clean, progressbar = FALSE)
it1975 <- itoken(similarity_1975$text_clean, progressbar = FALSE)
it1984 <- itoken(similarity_1984$text_clean, progressbar = FALSE)
it1992 <- itoken(similarity_1992$text_clean, progressbar = FALSE)
it1997 <- itoken(similarity_1997$text_clean, progressbar = FALSE)
it2004 <- itoken(similarity_2004$text_clean, progressbar = FALSE)
it2011 <- itoken(similarity_2011$text_clean, progressbar = FALSE)
it2017 <- itoken(similarity_2017$text_clean, progressbar = FALSE)

# total document cosine



it_similarity <- itoken(similarity$text_clean, progressbar = FALSE)
stop_words <- "stop_words"
v = create_vocabulary(it_similarity, stopwords = stop_words)
# trim less than 10 words and 99 percentile

v99 <- as.numeric(quantile(v$term_count, probs = c(.99)))

v = prune_vocabulary(v, term_count_min = 10, term_count_max = v99)
vectorizer = vocab_vectorizer(v)


### each year vs full corpus

dtmEP = create_dtm(it_similarity, vectorizer)
dim(dtmEP)    

# add weights

tfidf = TfIdf$new()
dtmEP_tfidf = fit_transform(dtmEP, tfidf)

tfidf_EP_cos_sim = sim2(x = dtmEP_tfidf, method = "cosine", norm = "l2")
dim(tfidf_EP_cos_sim)
tfidf_EP_cos_sim[1:5, 1:20]

# put in excel: USED TO CREATE TABLE
tfidf_EP_cos_sim_df <- tfidf_EP_cos_sim %>%
  as.matrix %>% as.data.frame
write.csv(tfidf_EP_cos_sim_df, "kline(2022_5_17)EP-MOC_table1_ngram1908_33.csv")







