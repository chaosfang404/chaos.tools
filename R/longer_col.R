library(data.table)
library(magrittr)
library(glue)

longer_col = function(
				.data,
				column,
				sep = "[^[:alnum:]]+"
){
	split_data <- as.data.table(.data)%>%
					.[[column]] %>%
					tstrsplit(split = sep) %>%
					setDT()
	split_data %>%
		setnames(
			paste0(
				"Tmp_splited_", 
				column,
				"_No.",
				1:ncol(.)
			)
		)

	data_with_na <- .data %>%
						as.data.table %>%
						.[,(column):=NULL] %>%
						.[,names(split_data):=split_data] %>%
						melt(
							measure = patterns(paste0("^Tmp_splited_",column,"_No.")),
							value.name = column
						) %>%
						.[,-"variable"]
    
	na_remove_cmd <- glue("!is.na({column})")
	eval(parse(text = glue("data_with_na[{na_remove_cmd}]")))
}
