comb2 <- function(
			...,
			sep = "_"
){
	list_all <- list(...)

	all <- list_all[[1]]

	if (length(list_all) >= 2)
	{
		for (i in 2:length(list_all))
		{
			paste2 <- function(
						x
			){
				paste(x,list_all[[i]],sep = sep)
			}
		
			all <- sapply(all,paste2)
		}
	}
	all <- t(all)
	rownames(all) <- NULL
	data.table(all)
}

str_comb <- function(
				...,
				sep = "_"
){
	str <- comb2(...,sep = sep) %>% unlist
	names(str) <- NULL
	str
}


dt_comb <- function(
				...,
				sep = "_"
){
	str_comb(...,sep = sep) %>%
	data.table() %>%
	separate_col(".") %>% 
	setnames(paste0("V",1:ncol(.))) %>% 
	.[]
}

