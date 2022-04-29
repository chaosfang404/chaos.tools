hic_circlize <-	function(
					hic_file = "/mnt/d/work/Hi-C/hic_files_hg19/DMSO_DHT.hic",
					genome = "hg19",
					resolution = 1e4,
					norm = "KR",
					chr_pair = c(12,16,19,22,15,22,13,17),
					chr_color = NA,
					limit = 1000,
					cex = NA,
){
	if(is.na(cex)){cex <- 1}

	cpl <- length(chr_pair)

	if(length(chr_color) != cpl)
	{
		chr_color <- colorRampPalette(chaos_color())(cpl)
	}


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
						resolution = resolution,
						norm = norm
					) %>%
					.[order(-counts)] %>%
					head(limit)
				}
			) %>% 
			rbindlist()

	bed <-	lapply(
				c(1,2),
				function(x)
				{
					col <- c(paste0("chr",x),paste0("chr",x,"_bin"),"counts")
					dt[,..col] %>%
					setnames(c("chr","start","counts")) %>%
					.[,chr := paste0("chr",chr)] %>%
					.[,end := start + resolution] %>%
					.[,.(chr,start,end,counts)]
				}
			)

	circlize::circos.initializeWithIdeogram(
		chromosome.index =	chr_dt %>%
							as.character() %>%
							paste0("chr",.) %>%
							unique(),
		species = genome,
		axis.labels.cex= 0.4*par("cex"),
		abels.cex = 1*par("cex"),
	)

	circlize::circos.track(
		ylim = c(0, 1), 
		bg.col = chr_color, 
		bg.border = NA, 
		track.height = 0.05
	)

	circlize::circos.genomicLink(
		bed[[1]], 
		bed[[2]], 
		col = sample(1:cpl, nrow(bed[[1]]),replace = T),
		border = NA
	)
}
