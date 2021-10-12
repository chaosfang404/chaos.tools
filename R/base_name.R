base_name <- function(
				full_name,
				ext = NA
){
	final_list <- c(NULL)
	for(i in full_name)
	{
		shortname <- i %>% 
					stringr::str_split("/") %>%
					unlist() %>%
					rev() %>%
					.[1]

		if(is.na(ext))
		{
			ext2 <- shortname %>%
					stringr::str_split("[.]") %>%
					unlist() %>%
					rev() %>%
					.[1]
			final <- shortname %>%
						stringr::str_replace(paste0(".",ext2),"")
		}else
		{
			final <- shortname %>%
						stringr::str_replace(ext,"")
		}

		final_list <- c(final_list,final)
	}
	final_list
}

