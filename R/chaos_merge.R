chaos_merge <-	function(
					.data,
					bedtools_path = "/usr/sbin/bedtools",
					c = 4,
					o = "collapse"
){
	tmp_str <- chaos_tmp()
	dt_file <- paste0(tmp_str,".chaos_merge.tmp")
	fwrite(.data, dt_file, sep = "\t", col.names = F)

	paste0(
		"cat ",
		dt_file,
		" | sort -k1,1 -k2,2n | bedtools merge -c ",
		c,
		" -o ",
		o,
		" > ",
		tmp_str
	) %>%
	system(intern = F)

	dt <- fread(tmp_str)
	unlink(c(dt_file,tmp_str))
	dt
}