hic_circlize <-	function(
					hic_file = "/mnt/d/work/Hi-C/hic_files_hg19/DMSO_DHT.hic",
					genome = "hg19",
					resolution = 1e4,
					norm = "KR",
					chr_pair = c(19,22,17,19,16,19,19,20),
					chr_color = NA,
					no_link_color = NA,
					limit = 1000,
					axis.labels.cex = 0.5,
					labels.cex = 1 
){
	chr_dt <- chr_pair |> matrix(ncol = 2, byrow = T)

	uniq_chr <- chr_dt |> as.character() |> unique()

	cpl <- uniq_chr |> length()

	if(length(chr_color) != cpl)
	{

		chr_color <- colorRampPalette(chaos_color())(cpl)
	}
	
	color_dt <- 	data.table(
						chr = uniq_chr,
						color = chr_color
					)

	dt <-	chr_dt |>
			apply(
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
		chromosome.index = paste0("chr",uniq_chr),
		species = genome,
		axis.labels.cex= axis.labels.cex,
		labels.cex = labels.cex,
	)

	circlize::circos.track(
		ylim = c(0, 1),
		bg.col = color_dt$color, 
		bg.border = NA, 
		track.height = 0.05
	)

	circlize::circos.genomicLink(
		bed[[1]], 
		bed[[2]], 
		col = rep(color_dt[!chr %in% no_link_color,color],each = limit),
		border = NA
	)
}

## hic_circlize2 uses data directly from hic_interaction
hic_circlize2 <-	function(
						matrix,
						genome = "hg19",
						chr_color = NA,
						no_link_color = NA,
						axis.labels.cex = 0.5,
						labels.cex = 1
){
	chr_dt <- matrix[,.(chr1,chr2)] |> unique()

	uniq_chr <- chr_dt |> as.character() |> unique()

	cpl <- uniq_chr |> length()

	if(length(chr_color) != cpl)
	{

		chr_color <- colorRampPalette(chaos_color())(cpl)
	}
	
	color_dt <- 	data.table(
						chr = uniq_chr,
						color = chr_color
					)

	bed <-	lapply(
				c(1,2),
				function(x)
				{
					col <- c(paste0("chr",x),paste0("chr",x,"_bin"),"counts")
					matrix[,..col] %>%
					setnames(c("chr","start","counts")) %>%
					.[,chr := paste0("chr",chr)] %>%
					.[,end := start + resolution] %>%
					.[,.(chr,start,end,counts)]
				}
			)

	circlize::circos.initializeWithIdeogram(
		chromosome.index = paste0("chr",uniq_chr),
		species = genome,
		axis.labels.cex= axis.labels.cex,
		labels.cex = labels.cex,
	)

	circlize::circos.track(
		ylim = c(0, 1),
		bg.col = color_dt$color, 
		bg.border = NA, 
		track.height = 0.05
	)

	circlize::circos.genomicLink(
		bed[[1]], 
		bed[[2]], 
		col = rep(color_dt[!chr %in% no_link_color,color],each = nrow(bed[[1]])),
		border = NA
	)
}
