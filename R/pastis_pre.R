pastis_pre <- function(
				hic_file,
				name = NA,
				ref = "hg19",
				chr_list = c(1:22,"X","Y"),
				resolution = 1e6,
				iteration = 100
){
	
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

		work_dir <- hic_file %>% 
					str_split("/") %>%
					unlist() %>%
					rev() %>%
					.[1] %>%
					str_replace(".hic","") %>%
					paste0("_",res_label)
	}else
	{
		work_dir <- name
	}

	dir.create(work_dir)
	setwd(work_dir)


	## get chr info
	chr_size_info <- chr_size(ref = ref, extra = T) %>%
						mutate_dt(chr = str_replace(chr,"chr","")) %>%
						filter_dt(chr %in% chr_list) %>%
						mutate_dt(
							chr = factor(chr,levels = chr_list),
							bin_end = floor(chr_length/resolution))
						)

	## create bed file
	bed_data <- data.table(NULL)
	for(i in chr_list)
	{
		tmp <- data.table(
					chr = i,
					start = seq(0,chr_size[chr == i,chr_length],resolution)
				) %>% 
				mutate_dt(end = start + resolution - 1) %>% 
				mutate_when(
					chr == i & 
					end > chr_size[chr == i,chr_length],
					end = chr_size[chr == i,chr_length]
				)
		bed_data <- rbind(bed_data,tmp)
	}

	bed_data[,bin_No := 1:.N]

	bed_data %>%
	mutate_dt(start = format(start,scientific = F, trim = T)) %>%
	fwrite(paste0(name,"bed"), sep = "\t", col.names = F)

	## create chr pairs for all interaction
	pairs <- combn(chr_list,2) %>% 
				t() %>% 
				rbind(data.table(V1=chr_list,V2 = chr_list))

	## create count data from .hic
	count_data <- data.table(NULL)
	for (i in 1:nrow(pairs))
	{
		chr1 <- pairs[i,V1]
		chr2 <- pairs[i,V2]
		tmp <- straw("NONE",hic_file,chr1,chr2,"BP",resolution) %>% 
				mutate_dt(
					chr_x = chr1, 
					chr_y = chr2
				) %>%
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
				rename_dt(chr_y_bin = bin_No) %>%
				.[, .(chr_x_bin, chr_y_bin, counts)]
		count_data <- rbind(count_data,tmp)
	}
	count_data %>% 
	arrange_dt(chr_x_bin,chr_y_bin) %>%
	fwrite(paste0(name,".count"),sep = "\t", col.names = F)

	## generate config.ini
	c("[all]",
		"output_name: structure",
		"verbose: 0",
		psate0("max_iter: ", iteration),
		paste0("counts: ", name, ".count"),
		paste0("lengths: ", name, ".bed"),
		"normalize: True"
	) %>%
	data.table() %>%
	fwrite("config.ini",sep = "\t", col.names = F)

	## generate shell scripte
	c("#!/bin/sh",
		paste0("#BSUB -J ",name,"_pastis"),
		"#BSUB -q ser",
		"#BSUB -n 1",
		'#BSUB -R "span[ptile=40]"',
		"#BSUB -W 360:00",
		"",
		"source activate env_3d",
		paste0("pastis-pm2 ",getwd(),"/",name)
	) %>%
	data.table() %>%
	fwrite(paste0(name,".sh"),sep = "\t", col.names = F)
}
