load("slda_jobsubsect.RData")

library(lda)
library(tm)
library(ggplot2)
library(nnet)
library(broom)
#corpus_jobs
#industryNames
#result
#to.keep

# data(poliblog.documents)
# 
# data(poliblog.vocab)
# 
# data(poliblog.ratings)
# 
# poliblog.ratings_01<- as.integer(ifelse(poliblog.ratings==-100, 0,1))
# 
#  num.topics <- 10

 ## Initialize the params
 #params <- sample(c(0, 1), num.topics, replace=TRUE)
 
 
  
 # result <- slda.em(documents=poliblog.documents,
 #                                        K=num.topics,
 #                                        vocab=poliblog.vocab,
 #                                        num.e.iterations=10,
 #                                        num.m.iterations=4,
 #                                        alpha=1.0, eta=0.1,
 #                                       #poliblog.ratings / 100,
 #                   poliblog.ratings_01,
 #                                        params,
 #                                        variance=0.25,
 #                                       lambda=1.0,
 #                                        logistic=TRUE,
 #                                        method="sLDA")
  
 ## Make a pretty picture.
load("res/sldajob_subsectors.RData")
  Topics <- apply(top.topic.words(result$topics, 8, by.score=TRUE),
                                     2, paste, collapse=" ")
  writeLines(Topics, "topics.txt")
   #coefs <- data.frame(coef(summary(result$model)))
  coefs <- coef(result$model)
  dim(coefs)
  head(coefs)
   theme_set(theme_bw())
   
   ## Healthcare vs Housekeeping
   lng_coefs18=melt(coefs[17,])
   lng_coefs20=melt(coefs[19,])
   coefscombine=cbind(lng_coefs18,lng_coefs20)
   names(coefscombine)= c("Healthcare","Housekeeping")
   coefscombine$Topics = Topics
   coefscombine$Topicnum=1:200
   
  head(coefscombine)
   
 coefsTopic18 <- cbind(lng_coefs18, Topics=factor(Topics, Topics[order(lng_coefs18$value)]))
 coefsTopic20 <- cbind(lng_coefs20, Topics=factor(Topics, Topics[order(lng_coefs20$value)])) 
 #coefscombine=cbind(coefsTopic18,coefsTopic18)
 head(coefscombine)
 #head(coefsTopic18)
 
 
 
 coefsTopic18sorted <- coefsTopic18[order(coefsTopic18$value, decreasing = TRUE),]
 coefsTopic20sorted <- coefsTopic20[order(coefsTopic20$value, decreasing = TRUE),] 
 # row.names(coefsTopic18sorted[1:10,])
 # row.names(coefsTopic20sorted[1:10,])
 # 
 rnames = union(row.names(coefsTopic18sorted[1:10,]),row.names(coefsTopic20sorted[1:10,]))
 coefscombine_ret=coefscombine[rnames,]
 df=melt(coefscombine_ret, id.vars=c("Topics","Topicnum"))
 
 ggplot(df, aes(value, Topics, fill = variable)) + geom_bar(position = position_dodge(), width = .75, stat="identity")  + 
   # geom_hline(y = -1, color = "deepskyblue4", size = .64, shape = 5, linetype="dashed") + 
   # geom_hline(y = 1, color = "firebrick1", size = .64, linetype="dashed") + geom_hline(y = 0, color = "black")
   geom_text(aes(x = value, ymax = Topics, label = Topicnum), position=position_dodge(width=.75)) + 
   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
   theme_bw() + 
   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
   scale_fill_manual(values = c("burlywood", "gray"), name = "") + 
   xlab("Coefficients") + 
   #ylab(expression("Fold-Enrichment" * " (log"[2] * ")" )) + 
   ylab("Topics") +
   theme(axis.text.x = element_text(size = 10,  colour=c("black")), axis.text.y = element_text(size = 9, colour=c("black")), axis.title.x = element_text(size=10, vjust = 0), axis.title.y = element_text(size=10, vjust = 0.5), legend.text = element_text(size=8)) 
         #legend.position=c(.9, .9)) 
 #+ 
   #coord_flip()
 
 
 ## Installation Technology (plumbing) vs Construction
 lng_coefs22=melt(coefs[21,])
 lng_coefs9=melt(coefs[8,])
 coefscombine=cbind(lng_coefs22,lng_coefs9)
 names(coefscombine)= c("Installation Technology (plumbing)","Construction")
 coefscombine$Topics = Topics
 coefscombine$Topicnum=1:200
 
 head(coefscombine)
 
 coefsTopic22 <- cbind(lng_coefs22, Topics=factor(Topics, Topics[order(lng_coefs22$value)]))
 coefsTopic9 <- cbind(lng_coefs9, Topics=factor(Topics, Topics[order(lng_coefs9$value)])) 
 #coefscombine=cbind(coefsTopic18,coefsTopic18)
 head(coefscombine)
 #head(coefsTopic18)
 
 
 
 coefsTopic22sorted <- coefsTopic22[order(coefsTopic22$value, decreasing = TRUE),]
 coefsTopic9sorted <- coefsTopic9[order(coefsTopic9$value, decreasing = TRUE),] 
 # row.names(coefsTopic18sorted[1:10,])
 # row.names(coefsTopic20sorted[1:10,])
 # 
 rnames = union(row.names(coefsTopic22sorted[81:90,]),row.names(coefsTopic9sorted[81:90,]))
 coefscombine_ret=coefscombine[rnames,]
 df=melt(coefscombine_ret, id.vars=c("Topics","Topicnum"))
 
 ggplot(df, aes(value, Topics, fill = variable)) + geom_bar(position = position_dodge(), width = .75, stat="identity")  + 
   # geom_hline(y = -1, color = "deepskyblue4", size = .64, shape = 5, linetype="dashed") + 
   # geom_hline(y = 1, color = "firebrick1", size = .64, linetype="dashed") + geom_hline(y = 0, color = "black")
   geom_text(aes(x = value, ymax = Topics, label = Topicnum), position=position_dodge(width=.75)) + 
   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
   theme_bw() + 
   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
   scale_fill_manual(values = c("burlywood", "gray"), name = "") + 
   xlab("Coefficients") + 
   #ylab(expression("Fold-Enrichment" * " (log"[2] * ")" )) + 
   ylab("Topics") +
   theme(axis.text.x = element_text(size = 10,  colour=c("black")), axis.text.y = element_text(size = 9, colour=c("black")), axis.title.x = element_text(size=10, vjust = 0), axis.title.y = element_text(size=10, vjust = 0.5), legend.text = element_text(size=8))
 
 
 qplot(Topics, value, data=coefs) +
       geom_(width=0.5, aes(ymin=Estimate-Std..Error,
                      ymax=Estimate+Std..Error)) + coord_flip()

    
 # predictions <- slda.predict(poliblog.documents,
 #                            result$topics, 
 #                            result$model,
 #                            alpha = 1.0,
 #                            eta=0.1)
 predictions <- slda.predict(corpus_jobs, 
                             result$topics,
                             result$model,
                             alpha = 1,
                             eta=0.1)
 
  
 # qplot(predictions,
 #                #fill=factor(poliblog.ratings),
 #                xlab = "predicted rating",
 #                 ylab = "density",
 #               alpha=I(0.5),
 #                 geom="density") +
 #       geom_vline(aes(xintercept=0)) +
 #       theme(legend.position = "none")
 
    
     # predicted.docsums <- slda.predict.docsums(poliblog.documents,
     #                                            +                                           result$topics, 
     #                                            +                                           alpha = 1.0,
     #                                            +                                           eta=0.1)
     
     predicted.docsums <- slda.predict.docsums(corpus_jobs,
                                               result$topics, 
                                               alpha = 1.0,
                                               eta=0.1)
  
 predicted.proportions <- t(predicted.docsums) / colSums(predicted.docsums)
  
  qplot(`Topic 1`, `Topic 2`, 
          data = structure(data.frame(predicted.proportions), 
                          names = paste("Topic", 1:10)), 
          size = `Topic 3`)

    
    
    
    # function (documents, K, vocab, num.e.iterations, num.m.iterations, 
    #           alpha, eta, annotations, params, variance, logistic = FALSE, 
    #           lambda = 10, regularise = FALSE, method = "sLDA", trace = 0L, 
    #           MaxNWts = 3000) 
    # {
    #   if (K > length(documents) && !regularise) {
    #     stop("K must be less than or equal to the length of documents.")
    #   }
    #   if (min(sapply(documents, length)) <= 0) {
    #     stop("All documents must have positive length.")
    #   }
    #   estimate.params <- function(document.sums, num.topics, logistic, 
    #                               MaxNWts = 3000) {
    #     z.bar. <- t(document.sums)/colSums(document.sums)
    #     if (logistic) {
    #       if (!(is.integer(annotations)) || sum(0:(length(unique(annotations)) - 
    #                                                1) %in% unique(annotations)) != length(unique(annotations))) 
    #         stop("Annotations must be consecutive integers starting from zero when sLDA and logistic.")
    #       model.fit <- nnet::multinom(annotations ~ z.bar. + 
    #                                     0, family = multinom(), MaxNWts = MaxNWts, trace = trace)
    #     }
    #     else if (regularise) {
    #       model.fit <- penalized::penalized(annotations ~ z.bar., 
    #                                         unpenalized = ~0, lambda2 = 1/lambda^2, trace = FALSE)
    #     }
    #     else {
    #       model.fit <- lm(annotations ~ z.bar. + 0)
    #     }
    #     list(model = model.fit, coefs = as.double(t(coef(model.fit))))
    #   }
    #   method <- rep(method, length.out = num.m.iterations)
    #   variance <- rep(variance, length.out = num.m.iterations)
    #   num.e.iterations <- rep(num.e.iterations, length.out = num.m.iterations)
    #   result <- .slda.collapsed.gibbs.sampler(documents, K, vocab, 
    #                                           num.e.iterations[1], alpha, eta, annotations, params, 
    #                                           variance[1], logistic, method[1], lambda = lambda, trace = trace)
    #   fit <- estimate.params(result$document_sums, K, logistic, 
    #                          MaxNWts)
    #   params <- fit$coefs
    #   for (ii in seq(length.out = num.m.iterations - 1)) {
    #     result <- .slda.collapsed.gibbs.sampler(documents, K, 
    #                                             vocab, num.e.iterations[ii + 1], alpha, eta, annotations, 
    #                                             params, variance[ii + 1], logistic, method[ii + 1], 
    #                                             lambda = lambda, initial = list(assignments = result$assignments), 
    #                                             trace = trace)
    #     fit <- estimate.params(result$document_sums, K, logistic, 
    #                            MaxNWts)
    #     params <- fit$coefs
    #   }
    #   c(result, fit)
    # }

  

