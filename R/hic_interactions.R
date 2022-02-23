hic_interaction <- function(
						hic_file,
						chr_list = NA,
						norm = "KR",
						resolution = 1e6,
						inter = "half"
){
	tmp <- function(
				x
			){
				chr1 = x[1]
				chr2 = x[2]
				as.data.table(
					strawr::straw(toupper(norm), hic_file, chr1, chr2, "BP", resolution)
				)[
					,chr1 := chr1
				][
					,chr2 := chr2
				][
					,normalization := norm
				][
					is.na(counts),
					counts := 0
				] %>%
				setnames(
					old = c("x","y"),
					new = c("chr1_bin","chr2_bin")
				)
			}

	chr_list_dt(
		hic_file = hic_file,
		chr_list = chr_list,
		inter = inter
	) %>%
	apply(1,tmp) %>%
	rbindlist()

}
