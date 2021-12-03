chaos_tp <- function(
				hic_file,
				chr_list = NA,
				resolution = 1e4,
				norm = "KR",
				window.size = 20
){
	chr_info <- as.data.table(
					strawr::readHicChroms(hic_file[1])
				)[name != "ALL"]

	if(length(chr_list) == 1)
	{
		if(is.na(chr_list))
		{
			chr_list <- chr_info[,name]
		}
	}

	tad_dt <- expand.grid(
					a = hic_file,
					b = chr_list,
					c = resolution,
					d = window.size,
					stringsAsFactors = F
				) %>%
				as.data.table()

	bin_func <- function(x)
	{
		chr_length <- chr_info[
							name == x[1],
							length
						]

		seq(0,chr_length,as.numeric(x[2])) %>%
		data.table(
			id = 1:length(.),
			chr = as.character(x[1]),
			resolution = as.character(x[2]),
			from.coord = ., 
			to.coord = c(.[-1],chr_length)
		)
	}

	bin_all <- tad_dt[,.(b,c)] %>% 
				unique() %>%
				apply(1,bin_func) %>%
				rbindlist()

	core_fun <- function(
					x
	){
		hic_file_single <- x[1]
		chr_list_single <- x[2]
		resolution_single <- as.numeric(x[3])
		window.size_single <- as.numeric(x[4])

		bins <- bin_all[
					chr == x[1] & resolution == as.numeric(x[3]),
					-"resolution"
				]

		counts <- hic_interaction(
					hic_file = x[1],
					chr_list = x[2],
					resolution = as.numeric(x[3]),
					norm = norm,
					inter = "intra"
				)[
					,.(chr1_bin,chr2_bin,counts)
				] %>%
				complete_dt(
					chr1_bin = bins$from.coord, 
					chr2_bin = bins$from.coord,
					fill = 0
				) %>% 
				dcast(
					chr1_bin ~ chr2_bin, 
					value.var= "counts",
					fill = 0
				) %>%
				.[,-1] %>%
				as.matrix()

		single_result <- structure(
							list(bins = bins, counts = counts),
							class = "TopDomData"
						) %>%
						TopDom::TopDom(window.size = as.numeric(x[4]))

		single_result$domain$sample <- base_name(x[1],".hic")
		single_result$domain$resolution <- as.numeric(x[3])
		single_result$domain$window_size <- as.numeric(x[4])
		single_result$binSignal$sample <- base_name(x[1],".hic")
		single_result$binSignal$resolution <- as.numeric(x[3])
		single_result$binSignal$window_size <- as.numeric(x[4])
		single_result$bed$sample <- base_name(x[1],".hic")
		single_result$bed$resolution <- as.numeric(x[3])
		single_result$bed$window_size <- as.numeric(x[4])
		single_result
	}

	total_result <- apply(tad_dt,1,core_fun)

	length_dt <- data.table(1:length(total_result))

	all_domains <-  apply(
						length_dt,
						1,
						function(x){total_result[[x]]$domain}
					) %>% 
					rbindlist()

	all_binSignals <-  apply(
							length_dt,
							1,
							function(x){total_result[[x]]$binSignal}
						) %>% 
						rbindlist()

	all_beds <-  apply(
					length_dt,
					1,
					function(x){total_result[[x]]$bed}
				) %>% 
				rbindlist()

	list(
		domain = all_domains,
		binSignal = all_binSignals,
		bed = all_beds
	)
}
