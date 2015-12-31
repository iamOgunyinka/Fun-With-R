fullname <- c( "Joshua", "John Doe", "Jane Doe", "Stephen Hawkins" )
symptoms <- factor( c ( "MILD", "MODERATE", "SEVERE", "MILD" ),
		levels = c( "MILD", "MODERATE", "SEVERE" ), ordered=TRUE )
gender <- factor( c( "MALE", "MALE", "FEMALE", "MALE" ),
		levels = c( "MALE","FEMALE" ) )
temperature <- c( 98.2, 97.5, 99.12, 101.5 )
blood_type <- factor( c( "A", "AB", "A", "O"),
		levels = c( "A", "AB", "B", "O" ))
flu_status <- c( T, F, F, T )

all_patients <- data.frame( fullname, symptoms, gender, 
		temperature, blood_type, flu_status )

print( all_patients )