pastis_pre <- function(
				hic_file,
				name = NA,
				ref = "hg19",
				chr_list = c(1:22,"X","Y"),
				resolution = 1e6,
				iteration = 100,
				method = "pm2",
				verbose = TRUE
){

	chr_list <- as.character(chr_list)
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
	chr_size_info <- chr_size(
						ref = ref, 
						extra = T
					)[
						,chr := str_replace(chr,"chr","")
					][
						chr %in% chr_list
					][
						,chr := factor(chr,levels = chr_list)
					][
						,bin_end := floor(length/resolution)
					]

	## create bed file
	bed_file <- paste0(file_prefix,".bed")
	if(!file.exists(bed_file))
	{
		bed_data <- data.table(NULL)
		for(i in chr_list)
		{
			tmp <- data.table(
						chr = i,
						start = seq(0,chr_size_info[chr == i,length],resolution)
					)[
						,end := start + resolution - 1
					][
						chr == i & 
						end > chr_size_info[chr == i,length],
						end := chr_size_info[chr == i,length]
					]
			bed_data <- rbind(bed_data,tmp)
		}

		bed_data[,bin_No := 1:.N]

		bed_data %>%
		mutate_dt(start = format(start,scientific = F, trim = T)) %>%
		fwrite(bed_file, sep = "\t", col.names = F)
	}else
	{
		bed_data <- fread(bed_file) %>%
					setnames(c("chr","start","end","bin_No")) %>%
					mutate_dt(chr <- as.character(chr))
	}

	## create chr pairs for all interaction
	count_file <- paste0(file_prefix,".matrix")
	if(!file.exists(count_file))
	{
		pairs <- combn(chr_list,2) %>% 
					t() %>% 
					rbind(data.table(V1=chr_list,V2 = chr_list))

		count_data <- data.table(NULL)
		for (i in 1:nrow(pairs))
		{
			chr1 <- pairs[i,V1] %>% as.character()
			chr2 <- pairs[i,V2] %>% as.character()
			tmp <- data.table(
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
			count_data <- rbind(count_data,tmp)
		}
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
