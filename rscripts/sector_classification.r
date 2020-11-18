options(java.parameters = "-Xmx4g")
library(data.table)
library(ggrepel)

### import data
library(XLConnect) # reading excel files
title_job_description <- readWorksheetFromFile("data/Golden set 11168 vacancies plus UWV_standard occupations.xlsx", sheet= "title_job_description",header=TRUE)

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

predictions <- slda.predict(corpus_jobs, result$topics,result$model,alpha = 1,eta=0.1)
predictions.docsums <- slda.predict.docsums(corpus_jobs, result$topics, alpha = 1,eta=0.1)

industryNames[pred_function(predictions)+1]

conti_table <- table(pred_function(predictions), sector_jobs)
print(k)
print(sum(diag(conti_table))/sum(conti_table))

dist_conti=1-(conti_table/apply(conti_table,2,sum))

fit <- cmdscale(dist_conti,eig=TRUE, k=2)
#fit <- isoMDS(dist_conti, k=2)

df_fit= data.table(fit$points)
colnames(df_fit)

ggplot(df_fit, aes(x=V1,y=V2, 
                   #label=row.names(fit$points)
                   label=industryNames
                   )) +
  geom_point()+
  geom_text_repel()+
  scale_radius(range = c(3,6))


 theta <- t(apply(result$document_sums + 1, 2, function(x) x/sum(x)))
 phi <- t(apply(t(result$topics) + .1, 2, function(x) x/sum(x)))

 #topic.labels <- mallet.topic.labels(topic.model, topic.words, 3)
 topclust=mallet.topic.hclust(phi, theta, 0.3)
 #plot(mallet.topic.hclust(phi, theta, 0.3), labels=topic.labels)
 
top_doc=top.topic.documents(predictions.docsums, num.documents = 10, alpha=0.1)

#7971  7580 10652  7869  7217  6895  6545 10668  7755
#5723
which(sector_jobs==20)
#top_doc

topicdocs=function(x,y){
 match=sum(which( x %in% y))
 if(is.null(match)) 0
 else match
}
hr=unlist(apply(top_doc,2, topicdocs,y=which(sector_jobs==27)))
Topics[which(hr>0)]


range.data.input <- apply(predictions, 2, range)
train.indx <- sample(1:nrow(predictions), 6700)
tra.job <- cbind(predictions[train.indx, ], sector_jobs[train.indx])
tst.job <- predictions[-train.indx, ]
real.industry<- sector_jobs[-train.indx]
