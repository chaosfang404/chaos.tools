online_genome_process <- function(
							ref
){
	fread(
		paste0("https://hgdownload.soe.ucsc.edu/goldenPath/",ref,"/bigZips/",ref,".chrom.sizes")
	)[
		,.(chr = V1, length = V2)
	][
		,name := ref
	][
		grepl(chr,pattern ="_"),
		group := "extra"
	][
		chr == "chrM",
		group := "mitochondrion"
	][
		is.na(group),
		group := "main",.(name,chr,length,group)
	][
		,group := factor(group,levels = c("main","mitochondrion","extra"))
	][
		order(group,chr)
	]
}

chr_size <- function(
				ref = "hg19",
				mit = FALSE,
				extra = FALSE,
				online = FALSE
){
	chaos.tools_data_dir <- "~/.config/chaos.tools"

	if(!dir.exists(chaos.tools_data_dir))
	{
		dir.create(chaos.tools_data_dir)
	}

	local_genome_file <- system.file(
								"extdata",
								"common.genome.info.txt",
								package = "chaos.tools"
							)

	downloaded_genome_file <- file.path(
									chaos.tools_data_dir,
									"downloaded.genome.info.txt"
								)

	if(!file.exists(downloaded_genome_file))
	{
		downloaded_genome <- data.table(NULL)
	}else
	{
		downloaded_genome <- fread(downloaded_genome_file)
	}


	all_genome <- rbind(
						fread(local_genome_file),
						downloaded_genome
					)

	genome_list <- all_genome[,name] %>% 
					unique()

	if (ref == "list")
	{
		genome_list
	}else
	{
		
		if(ref %in% genome_list)
		{
			if(isTRUE(online))
			{
				dt <- online_genome_process(ref = ref)
			}else
			{
				dt <- all_genome[
							name == ref
						][
							,group := factor(group,levels = c("main","mitochondrion","extra"))
						]
			}
		}else
		{
			dt <- online_genome_process(ref = ref)
			data.table::fwrite(
				rbind(dt,downloaded_genome),
				downloaded_genome_file,
				sep = "\t"
			)
		}

		if(isFALSE(mit))
		{
			dt <- dt[group != "mitochondrion"]
		}
	
		if(isFALSE(extra))
		{
			dt[group != "extra",.(chr,length)]
		}else
		{
			dt[,.(chr,length)]
		}
	}
}
