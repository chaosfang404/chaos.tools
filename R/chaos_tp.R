chaos_tp <- function(
				hic_file,
				chr_list = NA,
				resolution = 1e4,
				norm = "KR",
				window.size = 20L
){
	tp_func <- function(
						x
	){
		x[
			chr1_bin_end := chr1_bin + resolution
		][
			,.(chr1,chr1_bin,chr1_bin_end,chr2_bin,counts)
		] %>%
		dcast(
			chr1 + chr1_bin + chr1_bin_end ~ chr2_bin, 
			value.var = "counts"
		) %>%
		TopDom(window.size = window.size)
	}


	hic_interaction(
		hic_file = hic_file,
		chr_list = chr_list,
		resolution = resolution,
		norm = norm,
		inter = "intra",
		list = T
	) %>%
	sapply(tp_func) %>%
	rbindlist()
	
}