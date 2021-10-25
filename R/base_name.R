base_name <- function(
				full_name,
				ext = NA
){
	short_name_func <- function(
							x
	){
		shortname <- x[1] %>% 
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

		final
	}

	apply(data.table(full_name), 1, short_name_func)
}

