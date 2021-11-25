base_name <- function(
				full_name,
				ext = NA
){
	short_name_func <- function(
							x
	){
		shortname <- x[1] %>% 
						strsplit("/") %>%
						unlist() %>%
						rev() %>%
						.[1]

		if(is.na(ext))
		{
			ext2 <- shortname %>%
					strsplit("[.]") %>%
					unlist() %>%
					rev() %>%
					.[1]

			final <- shortname %>%
						gsub(
							pattern = paste0(".",ext2),
							replacement = ""
						)
		}else
		{
			final <- shortname %>%
						gsub(
							pattern = ext,
							replacement = ""
						)
		}

		final
	}

	sapply(full_name, short_name_func) %>%
	as.character()
}
