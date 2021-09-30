inter_data <- function(
				hic_files,
				samples = NA,
				norms = c("NONE","KR"),
				chr = c(1:22,"X","Y"),
				unit = "BP",
				resolutions = 2500000,
				file = NA,
				save = FALSE
){
	if(length(samples == 1))
	{
		if(is.na(samples))
		{
			samples <- gsub(".hic","",hic_files)
		}
	}

	if(length(hic_files) == length(samples))
	{
		dt <- data.table(
					file = hic_files, 
					name = samples
				)
		data <- data.table(NULL)

		for (hic_file in dt$file)
		{
			sample <- dt[file == hic_file, name]
			for (resolution in resolutions)
			{
				for (norm in norms)
				{
					for (chr1 in chr)
					{
						for (chr2 in chr)
						{
							if (chr1 != chr2)
							{
								interaction <- strawr::straw(
													norm = norm,
													fname = hic_file,
													chr1loc = as.character(chr1),
													chr2loc = as.character(chr2),
													unit = unit,
													binsize = resolution
												) %>%
												replace_na_dt(to  = 0)
												
								data <- rbind(
											data,
											data.table(
												sample = sample,
												resolution = resolution,
												normalization = norm,
												chr1 = as.character(chr1),
												chr2 = as.character(chr2),
												interaction = sum(interaction$counts)
											)
										)
							}
						}
					}
				}
			}
		}
	}

	if(save){
		if(is.na(file))
		{
			file <- paste0("interchromosomal.interactions.",as.Date(Sys.time()))
		}

		fwrite(data, file, sep = "\t", col.names = F)
	}

	data
}


intra_data <- function(
				hic_files,
				samples = NA,
				norms = c("NONE","KR"),
				chr = c(1:22,"X","Y"),
				unit = "BP",
				resolutions = 2500000,
				file = "intrachromosomal.inteactions",
				save = FALSE
){
	if(length(samples == 1))
	{
		if(is.na(samples))
		{
			samples <- gsub(".hic","",hic_files)
		}
	}

	if(length(hic_files) == length(samples))
	{
		dt <- data.table(
					file = hic_files, 
					name = samples
				)
		data <- data.table(NULL)

		for (hic_file in dt$file)
		{
			sample <- dt[file == hic_file, name]
			for (resolution in resolutions)
			{
				for (norm in norms)
				{
					for (chr1 in chr)
					{
						interaction <- strawr::straw(
											norm = norm,
											fname = hic_file,
											chr1loc = as.character(chr1),
											chr2loc = as.character(chr1),
											unit = unit,
											binsize = resolution
										)$counts %>%
										sum()
						data <- rbind(
									data,
									data.table(
										sample = sample,
										resolution = resolution,
										normalization = norm,
										chr1 = as.character(chr1),
										chr2 = as.character(chr1),
										interaction = interaction
									)
								) %>%
								tidyfst::replace_na_dt(to = 0)
					}
				}
			}
		}
	}

	if(save){
		if(is.na(file))
		{
			file <- paste0("intrachromosomal.interactions.",as.Date(Sys.time()))
		}

		fwrite(data, file, sep = "\t", col.names = F)
	}

	data
}
