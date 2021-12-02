chaos_tp <- function(
				hic_file,
				chr_list = NA,
				resolution = 1e4,
				norm = "KR",
				window.size = 20L
){
	chr_info <- as.data.table(
					readHicChroms(hic_file)
				)[name != "ALL"]

	if(length(chr_list) == 1)
	{
		if(is.na(chr_list))
		{
			chr_list <- chr_info[,name]
		}
	}

	tad_dt <- expand.grid(
					hic_file,
					chr_list,
					stringAsFactors = F
				) %>%
				as.data.table()


	core_fun <- function(
					x
	){
		chr_length <- chr_info[
							name == x[2],
							length
						]

		bins <- seq(0,chr_length,resolution) %>%
				data.table(
					id = 1:length(.),
					chr = x[2],
					from.coord = ., 
					to.coord = c(.[-1],chr_length)
				)

		counts <- hic_interaction(
					hic_file = x[1],
					chr_list = x[2],
					resolution = resolution,
					norm = norm,
					inter = "intra"
				)[
					,.(chr1_bin,chr2_bin,counts)
				] %>%
				complete_dt(chr1_bin = bins$from.coord, chr2_bin = bins$from.coord,fill = 0) %>% 
				dcast(chr1_bin ~ chr2_bin, value.var= "counts",fill = 0) %>%
				.[,-1] %>%
				as.matrix()

		result <- structure(
						list(bins = bins, counts = counts),
						class = "TopDomData"
					) %>%
					TopDom(window.size = window.size)

		sample_name <- base_name(x[1],".hic")
		result$domain$sample <- sample_name
		result$binSignal$sample <- sample_name
		result$bed$sample <- sample_name
	}

	result <- chr_list %>%
			data.table() %>%
			apply(1,core_fun)

	length_dt <- data.table(1:length(result))

	domains <-  apply(
					length_dt,
					1,
					function(x){result[[x]]$domain}
				) %>% 
				rbindlist()

	binSignals <-  apply(
					length_dt,
					1,
					function(x){result[[x]]$binSignal}
				) %>% 
				rbindlist()

	beds <-  apply(
					length_dt,
					1,
					function(x){result[[x]]$bed}
				) %>% 
				rbindlist()

	list(
		domain = domains,
		binSignals = binSignals,
		bed = beds
	)
}
