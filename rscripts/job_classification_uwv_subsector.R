options(java.parameters = "-Xmx4g")

library(XLConnect)
library(tm)
library(tokenizers)
library(text2vec)
library(stringr)
library(lda)
library(TunePareto)
library(stats)
library(nnet)
library(beepr)
library(data.table)


title_job_description <- readWorksheetFromFile("data/Golden set 11168 vacancies plus UWV_standard occupations.xlsx", sheet= "title_job_description",header=TRUE)

title_job_description<- data.table(title_job_description[-631,])
title_jobdesc <- na.omit(title_job_description, cols="job_description")


remove_one_char <- function(x, size) {
  word_vector<-unlist(str_split(x, " "))
  word_vector <- word_vector[!nchar(word_vector)<size]
  newstring <- paste0(word_vector, collapse=" ")
  return(newstring)
}


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


prep_docu("yes we know that 1 + 1 = 3 u . g hello .net and\\n c++ ab .knowledge")
prep_docu(title_jobdesc$job_description[155])



selectDoc <- function(sizeeach){
  selected_rows <- integer()
  all_sectors <- unique(title_jobdesc$UWV.Occ.subsector.title)
  for(sector in all_sectors){
    #select_rows <- sample(which(title_job_description$UWV.Occ.subsector.title==sector), size=sizeeach, replace=TRUE)
    select_rows <- sample(which(title_jobdesc$UWV.Occ.subsector.title==sector), size=sizeeach, replace=TRUE)
    selected_rows<- append(selected_rows,select_rows) 
  }
  return(selected_rows)
} 

softmax_prob <- function(predvalues){
  denom= 1+ sum(exp(predvalues))
  prob_rest = exp(predvalues)/denom
  prob_0 =1 - sum(exp(predvalues)/denom)
  return(c(prob_0, prob_rest))
}

softmax_prob_v2 <- function(predvalues){
  denom= 1+ sum(exp(predvalues))
  prob = exp(c(0, predvalues))/denom
  return(prob)
}



pred_function<- function(predIndustry){
  #industryPred<-apply(predIndustry, 1, which.max)
  #industryScore <- apply(predIndustry, 1, max)
  industryScore <- apply(predIndustry, 1, softmax_prob)
  industryPred <- apply(industryScore, 2, which.max)-1
  #predFinalIndustry<- ifelse(industryScore < (-2), 0, industryPred)
  #return(unlist(predFinalIndustry))
  return(industryPred)
}

pred_function_v2<- function(predIndustry){
  industryPred<-apply(predIndustry, 1, which.max)
  industryScore <- apply(predIndustry, 1, max)
  #industryScore <- apply(predIndustry, 1, softmax_prob)
  #industryPred <- apply(industryScore, 2, which.max)-1
  predFinalIndustry<- ifelse(industryScore < (-2), 0, industryPred)
  return(unlist(predFinalIndustry))
  #return(industryPred)
}


jobdescription <- lapply(title_jobdesc$job_description, prep_docu)


corpus_jobs <- lexicalize(jobdescription, lower=TRUE)
wordCount <- word.counts(corpus_jobs$documents,corpus_jobs$vocab)
maxFreq<- unname(round(quantile(wordCount, .999)))
to.keep <- corpus_jobs$vocab[wordCount < maxFreq & wordCount >= 3 ] 
corpus_jobs <- lexicalize(jobdescription, lower=TRUE, vocab=to.keep)

#job_sector<-factor(title_jobdesc$UWV.Occ.subsector.title)
#job_sector<-factor(title_job_description$UWV.Occ.subsector.title)

job_sector = sort(unique(title_jobdesc$UWV.Occ.subsector.title))
industryNames<- 1:length(job_sector)-1
names(industryNames)= job_sector

industryNames_rev = names(industryNames)
names(industryNames_rev) = industryNames

# subsectors = 1:length(industryNames)-1
# names(subsectors)=industryNames

sector_jobs <- industryNames[title_jobdesc$UWV.Occ.subsector.title]
#sector_jobs <- as.numeric(job_sector)-1
#sector_jobs <- as.numeric(factor(title_job_description$UWV.Occ.sector.title[1:1000]))
#sector_jobs <- class.ind(factor(title_job_description$UWV.Occ.sector.title[1:1000]))
#sector_jobs <- as.integer(sector_jobs)

table(sector_jobs)

#doc_lengths<-lapply(corpus_jobs, length)
#corpus_jobs <- corpus_jobs[-which(doc_lengths==0)]
#sector_jobs <- sector_jobs[-which(doc_lengths==0)]

selectFirst <- selectDoc(500)
corpus_jobs_train <- corpus_jobs[selectFirst]
sector_jobs_train <- sector_jobs[selectFirst]
table(sector_jobs_train)


#for(k in seq(from=0.05, to=.30, length.out =10)){
for(k in c(1, 1/200)){
  #selectFirst <- selectDoc(500)
  
  num.topics= 200
  
  #params <- sample(c(-1,1), num.topics*10, replace=TRUE)
  params <- runif(num.topics*43, min=-1, max=1)
  #params <- rep(1,num.topics*10)
  
  #doc_lengths<-lapply(corpus_jobs, length)
  #corpus_jobs <- corpus_jobs[-which(doc_lengths==0)]
  #sector_jobs <- sector_jobs[-which(doc_lengths==0)]
  

  result <- slda.em(documents=corpus_jobs,
                    K=num.topics,
                    vocab=to.keep,
                    num.e.iterations=10,
                    num.m.iterations=4,
                    alpha=1, eta=.1,
                    annotations = sector_jobs,
                    params,
                    variance=0.25,
                    lambda=1.0,
                    logistic=TRUE,
                    method="sLDA", MaxNWts = 20000)
  
  #Topics <- apply(top.topic.words(result$topics, 10, by.score=TRUE),2, paste, collapse=" ")
  save(result,to.keep,industryNames,corpus_jobs, file="slda_jobsubsect.RData")
  
  
  coef(result$model)
  
  
  
  predictions <- slda.predict(corpus_jobs, result$topics,result$model,alpha = 1,eta=0.1)
  industryNames[pred_function(predictions)+1]
  
  top3pred= function(x) x[order(x)[1:3]]
  
  apply(predictions,1,which.max)
  sector_jobs
  conti_table=table(sector_jobs,apply(predictions,1,which.max))
  
  
  conti_table <- table(pred_function(predictions), sector_jobs)
  print(k)
  print(sum(diag(conti_table))/sum(conti_table))
  
  range.data.input <- apply(predictions, 2, range)
  train.indx <- sample(1:nrow(predictions), 6700)
  tra.job <- cbind(predictions[train.indx, ], sector_jobs[train.indx])
  tst.job <- predictions[-train.indx, ]
  real.industry<- sector_jobs[-train.indx]
  
  object.frbcs.w <- frbs.learn(tra.job, range.data.input, method.type="FRBCS.W", control=list(num.labels=9, type.mf="SIGMOID", type.implication.func="LUKASIEWICZ"))
  
  pred<- predict(object.frbcs.w, tst.job[1:10,])
  err <- 100* sum(pred !=real.industry) / length(pred)
  
  ggplot(dtf, aes(Industry, Score)) +
    geom_bar(stat="identity", aes(fill=ifelse(Score>=0,"blue","red"))) +
    geom_text(aes(label=paste(round(Score, digits=1)),
                  vjust=ifelse(Score>=0,0,1))) +
    theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none")
}
#conti_table <- table(pred_function(predictions), sector_jobs)


predictions
sector_jobs[1:6]

stem_tokenizer = function(x) {
  tokenize_word_stems(x, language="dutch") 
}

#stem_tokenizer(mydoc_prep)

#%>% lapply(SnowballC::wordStem, language="dutch") 

prep_fun = prep_docu
#tok_fun = stem_tokenizer
tok_fun = tokenize_words

#docpath<- "expert_input"
#docpath = "./uk_small"

#dir("H:\\CJKR\\DATA\\UWV", pattern="jobfeed")

#dir_path<- dir("Z:\\CJKR\\DATA\\UWV", pattern="jobfeed", full.names=TRUE)
#current_dir_files = list.files(path=docpath, full.names=TRUE, recursive=TRUE, pattern=".txt")
#files_iterator = idir(dir_path, reader = read_file_docu)


#num_obs <- nrow(title_job_description)
#select_rows_train<-sample(1:nrow(title_job_description), size=round(num_obs*.70))

#train <- title_job_description[select_rows_train,]

it_train = itoken(title_job_description$job_description,
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  progressbar = FALSE)
#,
#ids=current_dir_files

#prep_fun = tolower
#tok_fun = word_tokenizer

#it_train = itoken(content(cc), preprocessor = prep_fun,
#                  tokenizer = tok_fun
#                  )

vocab = create_vocabulary(it_train, stopwords=c(tm::stopwords("nl"), "wij"))
#vocab$vocab$terms
vocab

vocab = prune_vocabulary(vocab, doc_proportion_max = .40, term_count_min=3)

vectorizer = vocab_vectorizer(vocab )

tcm = create_tcm(it_train, vectorizer, skip_grams_window = 5)
dtm = create_dtm(it_train, vectorizer)

glove = GloVe$new(word_vectors_size = 50, vocabulary=vocab, x_max =5)
wv_main=glove$fit_transform(tcm, n_iter=50)

word_vectors_context = glove$components
word_vectors = wv_main + t(word_vectors_context)
#glove$fit(tcm, n_iter=20)

#word_vectors <- glove$get_word_vectors()


new_glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
new_glove$fit(tcm, n_iter = 20)


documents=corpus_jobs$documents
K=10
vocab=corpus_jobs$vocab
num.e.iterations=20
num.m.iterations=10
alpha=1
eta=.5
#annotations = sector_jobs
#params
params=sample(c(-1,1), 10*10, replace=TRUE)
variance=0.25
lambda=1.0
logistic=TRUE
method="sLDA"
annotations <- factor(title_job_description$UWV.Occ.sector.title[1:1000])
trace=FALSE
MaxNWts=3000


estimate.params <- function(document.sums, num.topics, logistic, 
                            MaxNWts = 3000) {
  z.bar. <- t(document.sums)/colSums(document.sums)
  model.fit <- nnet::multinom(annotations ~ z.bar. + 
                                0, family = multinom(), MaxNWts = MaxNWts, trace = trace)  
  list(model = model.fit, coefs = as.double(t(coef(model.fit))))
}


method <- rep(method, length.out = num.m.iterations)
variance <- rep(variance, length.out = num.m.iterations)
num.e.iterations <- rep(num.e.iterations, length.out = num.m.iterations)
result <- lda:::.slda.collapsed.gibbs.sampler(documents=corpus_jobs$documents, K=10, vocab=corpus_jobs$vocab, 
                                              num.e.iterations[1], alpha=alpha, eta=eta, annotations=annotations,params, 
                                              variance=variance[1], logistic=TRUE, "sLDA", lambda = lambda, trace = 0L)
fit <- estimate.params(result$document_sums, K, logistic, 
                       MaxNWts)
params <- fit$coefs

for (ii in seq(length.out = num.m.iterations - 1)) {
  result <- .slda.collapsed.gibbs.sampler(documents, K, 
                                          vocab, num.e.iterations[ii + 1], alpha, eta, annotations, 
                                          params, variance[ii + 1], logistic, method[ii + 1], 
                                          lambda = lambda, initial = list(assignments = result$assignments), 
                                          trace = trace)
  fit <- estimate.params(result$document_sums, K, logistic, 
                         MaxNWts)
  params <- fit$coefs
}