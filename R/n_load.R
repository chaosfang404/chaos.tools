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
		ggrepel,
		export,
		ggprism,
		strawr,
		shiny,
		DT
	)

	setDTthreads(threads = parallel::detectCores())
}


clin_load <- function(
){
	pacman::p_load(
		clinfun,
		missForest,
		vcdExtra,
		survminer,
		survival
	)

	setDTthreads(threads = parallel::detectCores())
}
