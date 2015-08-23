# Create a Sample Corpus for Model

## Create a Training Sample and Test set
ctwitter <- file.path("~", "Data Science Coursera", "capstone-JHU-swiftkey", 
                      "data", "final", "en_US", "en_US.twitter.txt")
cblogs <- file.path("~", "Data Science Coursera", "capstone-JHU-swiftkey", 
                    "data", "final", "en_US", "en_US.blogs.txt")
cnews <- file.path("~", "Data Science Coursera", "capstone-JHU-swiftkey", 
                   "data", "final", "en_US", "en_US.news.txt")
### Read in files
corpus_t <- readLines(ctwitter, encoding = "UTF-8", skipNul=TRUE)
corpus_b <- readLines(cblogs, encoding = "UTF-8", skipNul=TRUE)
corpus_n <- readLines(file(cnews, "rb"), encoding = "UTF-8", skipNul=TRUE)

### Function for creating samples
corpus_sample <- function(corpus, p) {
    mask <- rbinom(length(corpus), size=1, prob=p)
    mask <- mask == 1
    return(corpus[mask])
}

### Create a 4% sample
set.seed(13720)
sample_proportion = 0.04

sample_twitter <- corpus_sample(corpus_t, p=sample_proportion)
sample_blog <- corpus_sample(corpus_b, p=sample_proportion)
sample_news <- corpus_sample(corpus_n, p=sample_proportion)

### Aggregate Sample, and write out to file
scfile <- file.path("~", "Data Science Coursera", "capstone-JHU-swiftkey", 
                    "JHU-DSS-capstone", "data", "sample", "sample_corpus_4pct.txt")
lapply(list(sample_twitter, sample_blog, sample_news), 
       function(x){write(x, file=scfile, append=TRUE)})