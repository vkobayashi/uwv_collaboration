load("res/sldajob_subsectors.RData")
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
    require("ggplot2")
  
  Topics <- apply(top.topic.words(result$topics, 10, by.score=TRUE),
                    +                 2, paste, collapse=" ")
  
   coefs <- data.frame(coef(summary(result$model)))
  #coefs <- data.frame(coef(result$model))
  
   theme_set(theme_bw())
  
 coefs <- cbind(coefs, Topics=factor(Topics, Topics[order(coefs$Estimate)]))
  
  coefs <- coefs[order(coefs$Estimate),]
  
 qplot(Topics, Estimate, colour=Estimate, size=abs(t.value), data=coefs)
       geom_errorbar(width=0.5, aes(ymin=Estimate-Std..Error,
                                     +                   ymax=Estimate+Std..Error)) + coord_flip()

    
 predictions <- slda.predict(poliblog.documents,
                                                              result$topics, 
                                                              result$model,
                                                               alpha = 1.0,
                                                              eta=0.1)
  
 qplot(predictions,
                fill=factor(poliblog.ratings),
                xlab = "predicted rating",
                 ylab = "density",
               alpha=I(0.5),
                 geom="density") +
       geom_vline(aes(xintercept=0)) +
       theme(legend.position = "none")
 
    
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

    
    
    
    function (documents, K, vocab, num.e.iterations, num.m.iterations, 
              alpha, eta, annotations, params, variance, logistic = FALSE, 
              lambda = 10, regularise = FALSE, method = "sLDA", trace = 0L, 
              MaxNWts = 3000) 
    {
      if (K > length(documents) && !regularise) {
        stop("K must be less than or equal to the length of documents.")
      }
      if (min(sapply(documents, length)) <= 0) {
        stop("All documents must have positive length.")
      }
      estimate.params <- function(document.sums, num.topics, logistic, 
                                  MaxNWts = 3000) {
        z.bar. <- t(document.sums)/colSums(document.sums)
        if (logistic) {
          if (!(is.integer(annotations)) || sum(0:(length(unique(annotations)) - 
                                                   1) %in% unique(annotations)) != length(unique(annotations))) 
            stop("Annotations must be consecutive integers starting from zero when sLDA and logistic.")
          model.fit <- nnet::multinom(annotations ~ z.bar. + 
                                        0, family = multinom(), MaxNWts = MaxNWts, trace = trace)
        }
        else if (regularise) {
          model.fit <- penalized::penalized(annotations ~ z.bar., 
                                            unpenalized = ~0, lambda2 = 1/lambda^2, trace = FALSE)
        }
        else {
          model.fit <- lm(annotations ~ z.bar. + 0)
        }
        list(model = model.fit, coefs = as.double(t(coef(model.fit))))
      }
      method <- rep(method, length.out = num.m.iterations)
      variance <- rep(variance, length.out = num.m.iterations)
      num.e.iterations <- rep(num.e.iterations, length.out = num.m.iterations)
      result <- .slda.collapsed.gibbs.sampler(documents, K, vocab, 
                                              num.e.iterations[1], alpha, eta, annotations, params, 
                                              variance[1], logistic, method[1], lambda = lambda, trace = trace)
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
      c(result, fit)
    }

  
myiris <- iris
myiris$Species <- as.integer(iris$Species)-1

irismodel <- multinom(Species~. , myiris)

predict(irismodel, myiris, type="probs")
