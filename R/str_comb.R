str_comb <- function(
				c1,
				c2,
				c3 = NA,
				c4 = NA,
				c5 = NA,
				c6 = NA,
				c7 = NA,
				c8 = NA,
				sep = "_"
){	
##========basic_combine================
    all_2 <- NULL
	for (i in c1) 
	{
		all_2 <- c(all_2,paste(i,c2,sep = sep))
	}
	all <- all_2
##========3_item_combine================
	if (unique(!is.na(c3)))
	{
		all_3 <- NULL
		for(j in all_2)
		{
			all_3 <- c(all_3,paste(j,c3,sep = sep))
		}
		all <- all_3
	}
##========4_item_combine================
	if (unique(!is.na(c4)))
	{
		all_4 <- NULL
		for(k in all_3)
		{
			all_4 <- c(all_4,paste(k,c4,sep = sep))
		}
		all <- all_4
	}
##========5_item_combine================
	if (unique(!is.na(c5)))
	{
		all_5 <- NULL
		for(l in all_4)
		{
			all_5 <- c(all_5,paste(l,c5,sep = sep))
		}
		all <- all_5
	}
##========6_item_combine================
	if (unique(!is.na(c6)))
	{
		all_6 <- NULL
		for(m in all_5)
		{
			all_6 <- c(all_6,paste(m,c6,sep = sep))
		}
		all <- all_6
	}
##========7_item_combine================
	if (unique(!is.na(c7)))
	{
		all_7 <- NULL
		for(n in all_6)
		{
			all_7 <- c(all_7,paste(n,c7,sep = sep))
		}
		all <- all_7
	}
##========8_item_combine================
	if (unique(!is.na(c8)))
	{
		all_8 <- NULL
		for(o in all_7)
		{
			all_8 <- c(all_8,paste(o,c8,sep = sep))
		}
		all <- all_8
	}
	all
}
