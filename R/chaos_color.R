chaos_color <-	function(
){
	c(
		"#E64B35",	## "Cinnabar"
		"#4DBBD5",	## "Shakespeare"
		"#00A087",	## "PersianGreen"
		"#3C5488",	## "Chambray"
		"#F39B7F",	## "Apricot"
		"#8491B4",	## "WildBlueYonder"
		"#91D1C2",	## "MonteCarlo"
		"#DC0000",	## "Monza"
		"#7E6148",	## "RomanCoffee"
		"#B09C85"	## "Sandrift"
	)
}

scale_fill_chao <-	function(
						n
){
	scale_fill_manual(
		values = colorRampPalette(chaos_color())(n)
	)
}

scale_color_chao <-	function(
						n
){
	scale_color_manual(
		values = colorRampPalette(chaos_color())(n)
	)
}