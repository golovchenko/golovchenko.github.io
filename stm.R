####################################################################################################################
############################################# Preparing R ##########################################################
####################################################################################################################

### setting the working directory
setwd("C:/Users/htw606/Documents/2017 11 16 Universitet/Disinfo/Abroad/Japan/STM Talk and Workshop at Waseda/Data/Trump and Hillary 2016/stm")


### installing and loading packages
install.packages("devtools")
library("devtools")# will be used for loading older verison of "tm" package below
### click "yes" if R asks you to install additional build tools
install_version("tm", version = "0.7", repos = "http://cran.us.r-project.org") #text mining package
install.packages("dplyr")# for basic data manipulation
install.packages("SnowballC") # for stemming string text
install.packages("stm") # For running STM
library("stm")
library("dplyr")
library("tm")

### Configuring R system
Sys.setlocale("LC_CTYPE", "English")
Sys.setlocale("LC_TIME", "English")
Sys.setlocale("LC_COLLATE", "English")

####################################################################################################################
######################################### Preparing the data #######################################################
####################################################################################################################

### reading the data from github
df <- read.csv("http://golovchenko.github.io/hilary_trump_2016.txt", stringsAsFactors = F)

###removing urls
df$post_message <- gsub('https? ?[:;,]?/{,3}(www.)?[^ ]*', '', df$post_message) # remove URLs

### creating a varialbe for character length of the posts and keep only those that are at least 50 characters long
df <- df %>% mutate(charle = nchar(post_message)) %>%
  filter(charle >=50)

### Creating a numeric date variable,"days", where 01-01-2016 == 1
df$date <- as.Date(df$post_published)
start.date<- as.Date(c("2016-01-01")) 
df <- mutate(df, days = date - start.date)
df$days <- as.numeric(df$days) #converting to numeric
####################################################################################################################
############################## Processing and analyzing data with "stm" package ####################################
####################################################################################################################

#### Processing the text corpus 
processed <- textProcessor(df$post_message, metadata = df,language = "en", wordLengths = c(3, Inf), striphtml = T)

### associating text with metadata and setting threshold. Here we only keep words that appear in at least 5 documentts
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 5)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta


### Creating  models with different number of topics (k)
k5 <- stm(out$documents, out$vocab, K = 5, prevalence =~ source + s(days), max.em.its = 100, data = out$meta, init.type = "Spectral")
k10 <- stm(out$documents, out$vocab, K = 10, prevalence =~ source + s(days), max.em.its = 100, data = out$meta, init.type = "Spectral")
k15 <- stm(out$documents, out$vocab, K = 15, prevalence =~ source + s(days), max.em.its = 100, data = out$meta, init.type = "Spectral")

### plotting topic prevalence 
plot.STM(k10, type = "summary", xlim = c(0, .5))

### examining the topics more closely
labelTopics(k10, topics = 1, n = 10) # looking at top 10 most frequent words for top 1
findThoughts(k10, texts = meta$post_message, n = 3, topics = 1)# looking at top 3 exemplary posts for topic 1
cloud(k10, topic = 1, scale = c(4,0.5), max.words = 40)
?cloud
### ### exploratory analysis through stmBrowser
install.packages("stmBrowser") #installing the package
library("stmBrowser") # loading the package

#### Trying out stm browser
stmBrowser(k10, data=out$meta, c("source", "post_published", "reactions_count_fb","likes_count_fb", "comments_count_fb","shares_count_fb"),
           text= "post_message", n = 1115)



# estimating the effects
#out$meta$rating <- as.factor(out$meta$rating)
prep <- estimateEffect(1:10 ~ source, k10, meta = out$meta, uncertainty = "Global")
summary(prep, topics=5)

### Examining the difference in topic prevalence between Clinton and Trump
plot(prep, covariate ="source", topics = c(1:10), model = k10, method = "difference", cov.value2 = "Donald Trump", cov.value1 = "Hillary Clinton", xlab = "Donald Trump.... Hillary Clinton", main = "Effect of Tweet Source: Trump vs. Hillary", xlim=c(-.3,.3))

### Examining variation in topic proportion across time
#estimating effects
prep1 <- estimateEffect(1:10 ~ source + s(days), k10, meta = out$meta, uncertainty = "Global")

#plotting topical variation
plot.estimateEffect(prep1, "days", method = "continuous", topics = c(8,9), model = z, printlegend = FALSE, xaxt = "n", xlab = "2016")
monthseq <- seq(from = as.Date("2016-01-01"), to = as.Date("2016-12-31"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),labels = monthnames)

### Examining topic correlation
top.corr <- topicCorr(k10, cutoff = .1) #computing correlation. 
plot(top.corr)
?topicCorr

### For detailed instructions please read original package vignette: https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf
