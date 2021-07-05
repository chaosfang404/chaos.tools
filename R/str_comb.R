str_comb <- function(
				.data,
				c2,
				...,
				sep = "_"
){
	all <- NULL
	for (i in .data)
	{
		all <- c(all,paste(i,c2,sep = sep))
	}

	list_rest <- list(...)
	list_rest_len <- length(list_rest)
    
    if (list_rest_len >= 1)
	{
		for (i in 1:list_rest_len)
		{
			all_tmp <- NULL
			for (j in all)
			{
				all_tmp <- c(all_tmp,paste(j,list_rest[[i]],sep = sep))
			}
			all <- all_tmp
		}
	}
	all
}
