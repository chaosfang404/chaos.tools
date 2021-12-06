n_load <- function(
){
	pacman::p_load(
		data.table,
		tidyfst,
		data.table,
		chaos.tools,
		ggplot2,
		ggsci,
		ggthemes,
		stringr,
		edgeR,
		rtracklayer,
		ggrepel
	)

	setDTthreads(threads = parallel::detectCores())
}
