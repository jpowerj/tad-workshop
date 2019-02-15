#loaded_vars <- load("datasets/SCOTUS_Decisions.Rdata")
#scotus_data <- SCDB_2018_01_justiceCentered_Citation
#cur_data <- scotus_data %>% filter(term == 2017)

# 1. download manifesto numeric data (rile score)

```{r,download-data}
library(manifestoR)
mp_setapikey("datasets/manifesto_apikey.txt")
manifesto_data <- mp_maindataset()
all_countries <- unique(manifesto_data$countryname)
just_us <- manifesto_data %>% filter(countryname == "United States")
us_rile <- just_us$rile
just_us <- attach_year(just_us)
just_us$numyear <- as.numeric(just_us$year)
modern_us_data <- just_us %>% filter(numyear >= 1968)

# Step 2: Download the actual texts

the_docs <- mp_corpus(data.frame(party=modern_us_data$party,date=modern_us_data$date))

# Step 3: Make document-term matrix, then convert into format
# usable by the Machine Learning library
us_dtm <- DocumentTermMatrix(the_docs)
# Old, replaced by LiblineaR library instead
#alg <- naive_bayes(us_dtm_train,us_labels)
#rf <- randomForest(x=us_dtm_train, y=us_labels, xtest=us_dtm_test, ytest=us_labels_test)

require(LiblineaR)
require(SparseM)
source("W4_files/matrix_utils.R")

# Convert to format that works with LiblineaR
us_dtm_sm <- convertSlamToSparseM(us_dtm)

# Split this LiblineaR format matrix into training/test
us_dtm_sm_train <- us_dtm_sm[1:13,]
us_dtm_sm_test <- us_dtm_sm[13:26,]
# And split the labels up the same way
us_labels_train <- modern_us$rile[1:13]
us_labels_test <- modern_us$rile[13:26]

# Train the model
model <- LiblineaR(us_dtm_sm_train, us_labels_train)
summary(model)

# Totally separate tutorial stuff
#library(RCurl)
#pdf_file <- "W4_files/ChaneyWallachConnellyBlei2016.pdf"
#cite_url <- "http://citeseerextractor.ist.psu.edu:8080/extractor"
#postForm(
#  cite_url,
#  file = fileUpload(
#    filename = path.expand(pdf_file),
#    contentType = "application/pdf"
#  ),.encoding="utf-8")
#"curl -F myfile=@/path/to/file.pdf "http://citeseerextractor.ist.psu.edu:8080/extractor")

#library(httr)
#result <- POST(
#  url = cite_url,
#  body = upload_file(
#    path =  path.expand(pdf_file),
#    type = 'application/pdf'),
#  verbose()
#)
