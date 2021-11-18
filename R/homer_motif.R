homer_motif <- function(
					files
){
	tmp <- function(
				x
	){
		fread(
			x,
			skip = 1
		) %>% 
		setnames(
			c("A","C","G","T")
		) %>% 
		t()
	}

	lapply(files,tmp)
}

motif_plot <- function(
					files
){
	files %>%
	homer_motif() %>% 
	ggseqlogo(
		method = "prob",
		facet = "wrap",
		ncol = 1
	)
}