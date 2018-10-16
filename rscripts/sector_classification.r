options(java.parameters = "-Xmx4g")

### import data
library(XLConnect) # reading excel files
title_job_description <- readWorksheetFromFile("Golden set 11168 vacancies plus UWV_standard occupations.xlsx", sheet= "title_job_description",header=TRUE)

# remove row 631 (Technical and Production) because job_description is too short
title_job_description<- title_job_description[-631,]

### remove one character
remove_one_char <- function(x, size) {
  word_vector<-unlist(str_split(x, " "))
  word_vector <- word_vector[!nchar(word_vector)<size]
  newstring <- paste0(word_vector, collapse=" ")
  return(newstring)
}

### Document preparation
library(stringr)
library(tm)
prep_docu <- function(doc){
  doc <- doc %>% 
    str_replace_all(pattern="\\\\n",replacement=' ') %>% 
    str_replace_all(pattern="\\\\r",replacement=' ') %>%
    str_replace_all(pattern="\\\\t",replacement=' ')
  vac_tokens <- doc %>% 
    tolower %>% 
    removeNumbers %>% 
    removeWords(words=c(tm::stopwords(kind="nl"), tm::stopwords(kind="en"))) %>% 
    remove_one_char(2) %>% 
    str_replace_all("[[:punct:]]", " ") %>%
    stripWhitespace %>% trimws
  return(vac_tokens)
}

# test (do not run)
prep_docu("yes we know that 1 + 1 = 3 u . g hello .net and\\n c++ ab .knowledge")
prep_docu(title_job_description$job_description[155])

### Select documents
selectDoc <- function(sizeeach){
  selected_rows <- integer()
  all_sectors <- unique(title_job_description$UWV.Occ.subsector.title)
  #all_sectors <- unique(title_job_description$UWV.Occ.sector.title)
  for(sector in all_sectors){
    select_rows <- sample(which(title_job_description$UWV.Occ.subsector.title==sector), size=sizeeach, replace=TRUE)
    #select_rows <- sample(which(title_job_description$UWV.Occ.sector.title==sector), size=sizeeach, replace=TRUE)
    selected_rows<- append(selected_rows,select_rows) 
  }
  return(selected_rows)
} 

### return softmax probabilities
softmax_prob <- function(predvalues){
  denom= 1+ sum(exp(predvalues))
  prob_rest = exp(predvalues)/denom
  prob_0 =1 - sum(exp(predvalues)/denom)
  return(c(prob_0, prob_rest))
}

### predict industry
pred_function<- function(predIndustry){
  industryScore <- apply(predIndustry, 1, softmax_prob)
  industryPred <- apply(industryScore, 2, which.max)-1
  return(industryPred)
}

### preprocess all documents
jobdescription <- lapply(title_job_description$job_description, prep_docu)

corpus_jobs <- lexicalize(jobdescription, lower=TRUE)
wordCount <- word.counts(corpus_jobs$documents,corpus_jobs$vocab)
maxFreq<- unname(round(quantile(wordCount, .999)))
to.keep <- corpus_jobs$vocab[wordCount < maxFreq & wordCount >= 3 ] 
corpus_jobs <- lexicalize(jobdescription, lower=TRUE, vocab=to.keep)

### job sector
job_sector<-factor(title_job_description$UWV.Occ.subsector.title)
industryNames<- levels(job_sector)
sector_jobs <- as.numeric(job_sector)-1
sector_jobs <- as.integer(sector_jobs)

### select vacancies
selectFirst <- selectDoc(500)
corpus_jobs_train <- corpus_jobs[selectFirst]
sector_jobs_train <- sector_jobs[selectFirst]
table(sector_jobs_train)


### train
library(lda)
num.topics= 200
params <- runif(num.topics*42, min=-1, max=1)
result <- slda.em(documents=corpus_jobs_train,
                  K=num.topics,
                  vocab=to.keep,
                  num.e.iterations=10,
                  num.m.iterations=4,
                  alpha=1, eta=.1,
                  annotations = sector_jobs_train,
                  params,
                  variance=0.25,
                  lambda=1.0,
                  logistic=TRUE,
                  method="sLDA", MaxNWts = 20000)

