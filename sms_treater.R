library( tm )
library( wordcloud )
library( e1071 )
library( gmodels )

stemer <- function()
{
	sms_raw <- read.csv( "sms_spam.csv", stringsAsFactors = FALSE )
	sms_raw$type <- factor( sms_raw$type, levels= c ( "Ham", "Spam" ) )
	spam_ham_percent <- round( prop.table( table( sms_raw$type ) ) * 100,
		digits = 1 )

	replacePunctuation <- function( x ){
		gsub( "[[:punct:]]+", " ", x )
	}

	dtm <- DocumentTermMatrix( VCorpus( VectorSource( sms_raw$text ) ),
			control = list( tolower = TRUE, 
				removeWords = TRUE,
				removeNumbers = TRUE,
				removePunctuation = replacePunctuation,
				stemDocument = TRUE )
		  )
	
	sms_dtm_train <- dtm[ 1:4169, ]
	sms_dtm_test <- dtm[ 4170:5574, ]

	sms_train_label <- sms_raw[1:4169, ]$type
	sms_test_label <- sms_raw[4170:5574, ]$type

	sms_freq_words <- findFreqTerms( sms_dtm_train, 5 )
	#wordcloud( sms_corpus_clean, min.freq = 50, random.order = FALSE )
	
	sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words ]
	sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words ]

	convert_fun <- function( x ){
		x <- ifelse( x > 0, "YES", "NO" )
	}

	sms_train <- apply( sms_dtm_freq_train, MARGIN = 2, convert_fun )
	sms_test <- apply( sms_dtm_freq_test, MARGIN = 2, convert_fun )

	sms_model <- naiveBayes( sms_train, sms_train_label )
	sms_test_pred <- predict( sms_model, sms_test )

	CrossTable( sms_test_pred, sms_test_label, prop.chisq = FALSE,
		prop.t = FALSE, dnn = c( "Predicted", "Actual" )
	)

	sms_model_lap <- naiveBayes( sms_train, sms_train_label, laplace = 1 )
	sms_test_pred_lap <- predict( sms_model_lap, sms_test )

	CrossTable( sms_test_pred_lap, sms_test_label,
		prop.t = FALSE, prop.chisq = FALSE, 
		dnn =c ( "Predicted", "Actual" )
	)
}