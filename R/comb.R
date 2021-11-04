comb2 <- function(
			.data,
			...,
			sep = "_"
){
	all <- .data

	list_rest <- list(...)
	
	if (length(list_rest) >= 1)
	{
		for (i in 1:length(list_rest))
		{
			paste2 <- function(
						x
			){
				paste(x,list_rest[[i]],sep = sep)
			}
		
			all <- sapply(all,paste2)
		}
	}
	all <- t(all)
	rownames(all) <- NULL
	data.table(all)
}

str_comb <- function(
				.data,
				...,
				sep = "_"
){
	str <- comb2(.data,...,sep = sep) %>% unlist
	names(str) <- NULL
	str
}


dt_comb <- function(
				.data,
				...,
				sep = "_"
){
	str_comb(.data,...,sep = sep) %>%
	data.table() %>%
	separate_col(".") %>% 
	setnames(paste0("V",1:ncol(.))) %>% 
	.[]
}

