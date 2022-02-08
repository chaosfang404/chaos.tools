hic_circlize <-	function(
					hic_file = "/mnt/d/work/Hi-C/hic_files_hg19/DMSO_DHT.hic",
					genome = "hg19",
					resolution = 1e4,
					chr_pair = c(12,16,9,22),
					chr_color = c("#4dbbd5", "#f39b7f", "#00a087", "#e64b35"),
					limit = 1000
){
	chr_dt <-	matrix(chr_pair, ncol = 2, byrow = T)

	dt <-	apply(
				chr_dt,
				1,
				function(
					x
				){
					hic_interaction(
						hic_file = hic_file,
						c(x[1],x[2]),
						inter = "inter",
						resolution = resolution
					) %>%
					.[order(-counts)] %>%
					head(limit)
				}
			) %>% 
			rbindlist() %>% 
			.[, No := paste0("interaction_",1:.N)]

	bed_1 <- dt[
				,.(chr = paste0("chr",chr1),start = chr1_bin,counts)
			][
				,end := start + resolution
			][
				,.(chr,start,end,counts)
			]

	bed_2 <- dt[
				,.(chr = paste0("chr",chr2),start = chr2_bin,counts)
			][
				,end := start + resolution
			][
				,.(chr,start,end,counts)
			]

	circlize::circos.initializeWithIdeogram(
		chromosome.index = paste0("chr",c(chr_dt[,1],chr_dt[,2])),
		species = genome
	)

	circlize::circos.track(
		ylim = c(0, 1), 
		bg.col = chr_color, 
		bg.border = NA, track.height = 0.05
	)

	circlize::circos.genomicLink(
		bed_1, 
		bed_2, 
		col = sample(1:4, nrow(bed_1),replace = T),
		border = NA
	)
}
