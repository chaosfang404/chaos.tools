#' Fast separate column
#' @examples
#' data.frame(x=paste0(1:5,"_",letters[1:5],".",LETTERS[1:5])) %>% separate_col("x")
#' data.frame(x=paste0(1:5,"_",letters[1:5],".",LETTERS[1:5])) %>% separate_col("x",sep = "_")
#' data.frame(x=paste0(1:5,"_",letters[1:5],".",LETTERS[1:5])) %>% separate_col("x",select = 1)
#' data.frame(x=paste0(1:5,"_",letters[1:5],".",LETTERS[1:5])) %>% separate_col("x",select = 2,into = "y")
#' data.frame(x=paste0(1:5,"_",letters[1:5],".",LETTERS[1:5])) %>% separate_col("x",select = 3,into = "y",remove = F)
#' data.frame(x=paste0(1:5,"_",letters[1:5],".",LETTERS[1:5])) %>% separate_col("x",select = 1,remove = F)
#' data.frame(x=paste0(1:5,"_",letters[1:5],".",LETTERS[1:5])) %>% separate_col("x",select = c(1,3),remove = F)
#' data.frame(x=paste0(1:5,"_",letters[1:5],".",LETTERS[1:5])) %>% separate_col("x",select = c(1,3),into = c("y","z"),remove = F)
#' @export
separate_col <- function(
					.data,
					column,
					into = NA,
					select = NA,
					sep = "[^[:alnum:]]+",
					remove = TRUE
){
	split_columns <- as.data.table(.data)%>%
						.[[column]] %>%
						tstrsplit(split = sep) %>%
						setDT()
 
	default_name <- paste(
						"splited",
						column,
						1:ncol(split_columns),
						sep = "_"
					)

	if(length(select) == 1)
	{
		if(is.na(select))
		{
			select = 1:ncol(split_columns)
		}
	}

	selected_columns <- paste("splited",column,select,sep = "_")

	if(length(into) == 1)
	{
		if(is.na(into))
		{
			into <- default_name[select]

			##a simplified way when there is only 1 column were selected
			##and the original column was not need
			if(length(select) == 1)
			{
				if(!is.na(select))
				{
					if(isTRUE(remove))
					{
						into <- column
					}
				}
			}
		}
	}

	renamed_split_columns <-  split_columns %>%
								setnames(default_name) %>%
								.[,..selected_columns] %>%
								setnames(into)

	if(isTRUE(remove))
	{
		data.table(
			.data
		)[,(column) := NULL] %>%
		cbind(renamed_split_columns)
	} else
	{
		data.table(
			.data
		) %>% 
		cbind(renamed_split_columns)
	}
	
}
