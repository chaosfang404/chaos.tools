wcpa_matrix = function(
				.data,
				name,
				res = "2500000",
				method = "KR",
				chr = c(1:22,"X","Y")
){
	chr_list <- as.character(chr)
	
	wcpa(.data)[
		  sample == name & 
		  resolution == res &
		  normalization == method &
		  chr1 %in% chr_list & 
		  chr2 %in% chr_list
	][
		, 
		.(
			chr1 = factor(chr1,levels = chr_list),
			chr2 = factor(chr2,levels = chr_list),
			WCPA
		)
	] %>%
   	tidyfst::wider_dt(
		 chr1,
		 name = "chr2",
		 value = "WCPA"
	 ) %>%
	tibble::column_to_rownames("chr1")
}
