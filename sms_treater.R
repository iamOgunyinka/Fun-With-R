library( tm )
library( wordcloud )

replacePunctuation <- function( x ){
	gsub( "[[:punct:]]+", " ", x )
}

stemer <- function()
{
	sms_raw <- read.csv( "sms_spam.csv", stringsAsFactors = FALSE )
	sms_raw$type <- factor( sms_raw$type, levels= c ( "Ham", "Spam" ) )
	spam_ham_percent <- round( prop.table( table( sms_raw$type ) ) * 100,
		digits = 1 )
	print( spam_ham_percent ) #percentage of ham to spam
	
	sms_corpus <- VCorpus( VectorSource( sms_raw$text ) )
	sms_corpus_clean <- tm_map( sms_corpus, 
		content_transformer( tolower ) )
	sms_corpus_clean <- tm_map( sms_corpus_clean, removeNumbers )
	sms_corpus_clean <- tm_map( sms_corpus_clean, removeWords, stopwords() )
	sms_corpus_clean <- tm_map( sms_corpus_clean, 
		content_transformer( replacePunctuation ) )
	sms_corpus_clean <- tm_map( sms_corpus_clean, stemDocument )
	sms_corpus_clean <- tm_map( sms_corpus_clean, stripWhitespace )

	dtm <- DocumentTermMatrix( sms_corpus_clean )
	print( dtm )

	dtm2 <- DocumentTermMatrix( VCorpus( VectorSource( sms_corpus_clean ) ),
			control = list( tolower = TRUE, 
				removeWords = TRUE,
				removeNumbers = TRUE,
				removePunctuation = replacePunctuation,
				stemDocument = TRUE )
		  )
	print( dtm2 )

	sms_dtm_train <- dtm[ 1:4169, ]
	sms_dtm_test <- dtm[ 4069:5574, ]

	sms_train_label <- sms_raw[1:4169, ]$type
	sms_test_label <- sms_raw[4170:5574, ]$type

	wordcloud( sms_corpus_clean, min.freq = 50, random.order = FALSE )
}