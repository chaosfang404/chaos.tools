chaos_merge <-	function(
					.data,
					bedtools_path = NA,
					c = 4,
					o = "collapse"
){
	if(is.na(bedtools_path))
	{
		bedtools_path <- system("which bedtools",intern = T)
	}

	tmp_str <- chaos_tmp()
	dt_file <- paste0(tmp_str,".chaos_merge.tmp")
	fwrite(.data, dt_file, sep = "\t", col.names = F, scipen = 10)

	dt <-	paste0(
				"cat ",
				dt_file,
				" | sort -k1,1 -k2,2n | ",
				bedtools_path,
				" merge -c ",
				c,
				" -o ",
				o
			) %>%
			system(intern = T) %>%
			as.data.table() %>%
			separate_col(".",sep = "\t") %>%
			setnames(paste0("V",1:ncol(.))) %>%
			.[,V2 := as.numeric(V2)] %>%
			.[,V3 := as.numeric(V3)]

	unlink(dt_file)
	dt
}

## It would be faster when the output of bedtools were writen to files,
## however it would generate too much small files