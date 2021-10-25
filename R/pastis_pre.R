pastis_pre <- function(
				hic_file,
				name = NA,
				ref = "hg19",
				chr_list = NA,
				resolution = 1e6,
				iteration = 100,
				method = "pm2",
				verbose = TRUE
){
	chr_list_dt <- chr_list_dt(
						hic_file = hic_file,
						chr_list = chr_list,
						inter = "all"
					)

	chr_list <- chr_list_dt[,V1] %>% unique() %>% as.character()


	iter_label <- paste0("iter_",iteration)

	## create folder
	if(is.na(name))
	{
		if(resolution < 1e6)
		{
			res_label <- paste0(resolution/1e3,"Kb")
		}else
		{
			res_label <- paste0(resolution/1e6,"Mb")
		}

		name <- hic_file %>% 
					base_name() %>%
					paste0("_",res_label)
	}

	work_dir <- paste0(name,"_iter_",iteration)
	if(!dir.exists(work_dir)){dir.create(work_dir)}
	file_prefix <- 	paste0(work_dir,"/",name)

	## get chr info
	chr_size_info <- data.table(
						strawr::readHicChroms(hic_file)
					)[
						name %in% chr_list,
						.(chr = name, length)
					][
						,chr := factor(chr,levels = chr_list)
					][
						,bin_end := floor(length/resolution)
					]


	## create bed file
	bed_file <- paste0(file_prefix,".bed")
	if(!file.exists(bed_file))
	{
		bed_data_func <- function(
							x
		){
			chr <- x[1]
			length <- x[2]
			data.table(
				chr = chr,
				start = seq(0,length,resolution)
			)[
				,end := start + resolution - 1
			][
				chr == i & 
				end > length,
				end := length
			][
				bin_No := start/resolution +1
			]
		}

		bed_data <- apply(chr_size_info,1,bed_data_func) %>% rbindlist()

		bed_data %>%
		mutate_dt(start = format(start,scientific = F, trim = T)) %>%
		fwrite(bed_file, sep = "\t", col.names = F)
	}else
	{
		bed_data <- fread(
						bed_file,
						col.names = c("chr","start","end","bin_No")
					)[
						,chr := as.character(chr)
					]
	}

	## create chr pairs for all interaction
	count_file <- paste0(file_prefix,".matrix")
	if(!file.exists(count_file))
	{
		count_data_func <- function(
					x
		){
			chr1 <- x[1]
			chr2 <- x[2]
			data.table(
				strawr::straw("NONE", hic_file, chr1, chr2, "BP", resolution)
			)[
				,chr_x := chr1
			][
				,chr_y := chr2
			] %>%
			left_join_dt(
				bed_data,
				by = c(
					"chr_x" = "chr",
					"x" = "start"
				)
			) %>% 
			rename_dt(chr_x_bin = bin_No) %>%
			left_join_dt(
				bed_data,
				by = c(
					"chr_y" = "chr",
					"y" = "start"
				)
			) %>% 
			rename_dt(
				chr_y_bin = bin_No
			) %>%
			select_dt(
				chr_x_bin,
				chr_y_bin,
				counts
			)
		}
		count_data <- apply(chr_list_dt,1,count_data_func) %>% rbindlist()
		
		count_data %>%
		filter_dt(chr_x_bin != chr_y_bin) %>%
		arrange_dt(chr_x_bin,chr_y_bin) %>%
		fwrite(count_file,sep = "\t", col.names = F)
	}

	## generate config.ini

	if(isTRUE(verbose)){verbose_label <- 1}else{verbose_label <- 0}

	c("[all]",
		"output_name: structure",
		paste0("verbose: ", verbose_label),
		paste0("max_iter: ", iteration),
		paste0("counts: ", name, ".matrix"),
		paste0("lengths: ", name, ".bed"),
		"normalize: True"
	) %>%
	data.table() %>%
	fwrite(file.path(work_dir,"config.ini"),sep = "\t", col.names = F)

	if (method == "mds"){pastis_cmd <- paste0("pastis-", method, " ", getwd(), "/", work_dir)}
	if (method == "nmds"){pastis_cmd <- paste0("pastis-", method, " ", getwd(), "/", work_dir)}
	if (method == "pm1"){pastis_cmd <- paste0("pastis-", method, " ", getwd(), "/", work_dir)}
	if (method == "pm2"){pastis_cmd <- paste0("pastis-", method, " ", getwd(), "/", work_dir)}
	if (method == "poisson"){pastis_cmd <- paste0("pastis-", method, " ", getwd(), "/", work_dir)}

	## generate shell scripte
	c("#!/bin/sh",
		paste0("#BSUB -J ",name,"_iter_",iteration, "_pastis"),
		"#BSUB -q ser",
		"#BSUB -n 1",
		'#BSUB -R "span[ptile=40]"',
		"#BSUB -W 360:00",
		"",
		"source activate env_3d",
		pastis_cmd
	) %>%
	data.table() %>%
	write.table(paste0(name,"_iter_",iteration,".sh"),sep = "\t", col.names = F,row.names = F, quote = F)

	print(paste0("Preparation for ", name,".iter_",iteration, " has finished"))
}
