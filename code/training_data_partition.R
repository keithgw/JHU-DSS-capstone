# Create a Training And Test Set

## Load 6% sample corpus
scfile <- file.path("~", "Data Science Coursera", "capstone-JHU-swiftkey", 
                    "JHU-DSS-capstone", "data", "sample", "sample_corpus_4pct.txt")
sample_corpus <- readLines(file(scfile, "rb"), encoding = "UTF-8", skipNul=TRUE)

## Split 6% sample into 4% training set, and 2% test set
train_file <- file.path("~", "Data Science Coursera", "capstone-JHU-swiftkey",
                        "JHU-DSS-capstone", "data", "sample", "train", 
                        "train_corpus.txt")
test_file <- file.path("~", "Data Science Coursera", "capstone-JHU-swiftkey",
                        "JHU-DSS-capstone", "data", "sample", "test", 
                        "test_corpus.txt")

set.seed(2012)
mask <- rbinom(length(sample_corpus), size=1, prob=2/3)
in_train <- mask == 1
training <- sample_corpus[in_train]
testing <- sample_corpus[!in_train]

write(training, file=train_file)
write(testing, file=test_file)