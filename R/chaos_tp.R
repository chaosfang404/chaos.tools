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
		bins <- bin_all[chr == x[2]]

		counts <- hic_interaction(
					hic_file = x[1],
					chr_list = x[2],
					resolution = as.numeric(x[3]),
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
					TopDom::TopDom(window.size = as.numeric(x[4]))

		result$domain$sample <- base_name(x[1],".hic")
		result$domain$resolution <- as.numeric(x[3])
		result$domain$window_size <- as.numeric(x[4])
		result$binSignal$sample <- base_name(x[1],".hic")
		result$binSignal$resolution <- as.numeric(x[3])
		result$binSignal$window_size <- as.numeric(x[4])
		result$bed$sample <- base_name(x[1],".hic")
		result$bed$resolution <- as.numeric(x[3])
		result$bed$window_size <- as.numeric(x[4])
		result
	}

	result <- apply(tad_dt,1,core_fun)

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
