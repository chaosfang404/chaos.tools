plot_di_is <- function(
				.data = fread("/mnt/d/work/Hi-C/DI_IS_hg19/DI_IS.all.gz"), 
				sample = c("DMSO_EtOH","DMSO_DHT"),
				resolution = 10000,
				chr = "10",
				start = 33000000,
				end = 35000000,
				size = 1,
				which = "all"
){
	dt <- 	.data[
				V1 == chr & 
				V5 %in% sample & 
				between(V2,start,end) & 
				V7 == resolution
			]

	if(which != "all"){ dt <- dt[V6 == which]}

	ggplot(
		dt,
		aes(V2,V4,color = V5)
	) + 
	geom_line(size = size) +
	facet_grid(V6 ~ .,scales = "free") + 
	theme_prism() +
	theme(
		legend.position = "none",
		strip.text.y = element_text(angle = 0)
	) + 
	labs(x=NULL, y = NULL) + 
	scale_y_continuous(guide = "prism_offset") + 
	scale_x_continuous(
		guide = "prism_offset",
		breaks = seq(start,end,5e5),
		labels = paste0(seq(start,end,5e5)/1000000,"Mb")
	) +
	scale_color_npg()
}