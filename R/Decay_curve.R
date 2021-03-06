cvd <-	function(
			hic_file = "/mnt/d/work/Hi-C/hic_files_hg19/DMSO_DHT.hic",
			resolution = 1e4,
			norm = "KR",
			chr = NA
){
	c_count <- 	hic_file |>
				lapply(
					function(x){
						hic_interaction(
							hic_file = x[1],
							chr_list = chr,
							resolution = resolution,
							norm = norm,
							inter = "intra"
						)[
							,distance := chr2_bin - chr1_bin
						][
							,sum(counts),.(chr1,distance)
						][
							,.(sample = base_name(x[1]),chr = chr1,distance,counts = V1)
						]
					}
				) |>
				rbindlist()

	s_count <- 	c_count[
					,sum(counts),
					.(sample,distance)
				][
					,.(sample,chr = "all",distance,counts = V1)
				]

	rbind(c_count,s_count)
}

plot_cvd <- function(
				.data,
				sample = NA,
				chr = "all",
				slop_position = c(4,6.8),
				slop_length = 2,
				min = 1e4,
				max = 1e7
){
	s <- sample

	if(length(s) == 1)
	{
		if(is.na(s))
		{
			s <- unique(.data$sample)
		}
	}

	c <- chr

#	x_breaks <- data.table(
#					breaks = 4:log(max),
#					labels = c("10Kb","100Kb","1Mb","10Mb")
#				)
#	#as the default resolution is 10k, the closest distance is 10k except 0
#	#the contact that longer than 100Mb is rare, and we don't really need the plot the contact longer than 10Mb

	x_breaks <- log10(min):log10(max) |>
				lapply(
					function(i)
					{
						if(i < 6)
						{
							data.table(breaks = i, labels = paste0(10^(i-3),"kb"))
						} else
						{
							data.table(breaks = i, labels = paste0(10^(i-6),"Mb"))
						}
					}
				) |>
				rbindlist()

	dt <-	.data[
				sample %in% s & 
				chr %in% c &
				distance %between% c(min,max)
			]

	p <-	ggplot(
				dt,
				aes(
					log10(distance),
					log10(counts),
					color = sample
				)
			) +
			geom_line(alpha = 0.8) +
			theme_prism() + 
			theme(legend.position = c(0.75,0.8)) +
			scale_x_continuous(
				breaks = x_breaks$breaks,
				minor_breaks = sapply(log10(min):(log10(max)-1),function(x){log10(1:9) + x}),
				#1:9 代表大格之间的9个小格，x_breaks$breaks代表所有的大刻度
				labels = x_breaks$labels,
				guide = "prism_offset_minor"
			) +
			scale_y_continuous(
				limits = c(
							floor(log10(min(dt$counts))),
							ceiling(log10(max(dt$counts)))
						),
				guide = "prism_offset"
			) +
			labs(
				x = "Contact Distance",
				y = "Contact frequency"
			) +
			scale_color_npg() + 
			annotate(
				geom = "text",
				x = slop_position[1] + sqrt(2)*slop_length/4,
				y = slop_position[2] - sqrt(2)*slop_length/4, 
				parse = T, 
				label = "italic(s)^-1"
			) + 
			annotate(
				geom = "segment",
				x = slop_position[1],
				xend = slop_position[1] + sqrt(2)*slop_length/2,
				y = slop_position[2],
				yend = slop_position[2] - sqrt(2)*slop_length/2,
				# 当 slope = -1时, yend = y + x - xend
				linetype = 2,
				color = "#3c5488",
				alpha = 0.8
			)

	if(length(chr) == 1)
	{
		p
	}else
	{
		p + facet_grid(~chr)
	}
}
