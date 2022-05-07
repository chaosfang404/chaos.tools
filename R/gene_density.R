gene_density_calc <- function(
							ref = "hg19",
							annotation_file = "~/Data/Reference/hg19/annotation/gencode.v38lift37.annotation.gff3.gz",
							resolution = 1e4,
							position = "tss"
){
	slice <- function(
				x
	){
		seq_dt(
			start = 0,
			end = as.numeric(x[2]),
			slice_size = resolution
		)[
			,chr := x[1]
		][
			,.(chr,start = V1 + 1,end = V2)
		]
	}

	chr_info <- chr_size(ref = ref, mit = T) %>%
				apply(1,slice) %>%
				rbindlist() %>%
				setkey()

	gene_info_raw <-	as.data.table(
							import(annotation_file)
						)[
							type == "gene",
							.(chr = as.character(seqnames), start, end, gene_name,strand)
						]

	if(position == "tss")
	{
		gene_info <-	gene_info_raw[
							strand == "+", `:=`(x = start, y = start + 1)
						][
							strand == "-", `:=`(x = end - 1, y = end)
						][
							,.(chr,start = x, end = y, gene_name, position = "tss")
						]
	} else if(position == "tes")
	{
		gene_info <-	gene_info_raw[
							strand == "+", `:=`(x = end - 1, y = end)
						][
							strand == "-", `:=`(x = start, y = start + 1)
						][
							,.(chr,start = x, end = y, gene_name, position = "tes")
						]
	} else if(position == "mid")
	{
		gene_info <-	gene_info_raw[
							,x := ceiling((start + end)/2)
						][
							,y := x + 1
						][
							,.(chr,start = x, end = y,gene_name, position = "mid")
						]
	} else if(position == "all")
	{
		gene_info <-	gene_info_raw[,.(chr,start, end,gene_name, position = "all")]
	}


	foverlaps(
		chr_info,
		setkey(gene_info,chr,start,end)
	)[
		!is.na(gene_name),
		gene_number := .N,
		.(chr,i.start,i.end)
	][
		is.na(gene_name),
		gene_number := 0
	][
		,.(chr,start = i.start,end = i.end,gene_number)
	][
		,`:=`(pos = position,res = resolution,genome = ref, annotation = base_name(annotation_file))
	] %>% unique()
}

gene_density <- function(
					ref = "hg19",
					annotation_file = "~/Data/Reference/hg19/annotation/gencode.v38lift37.annotation.gff3.gz",
					resolution = 1e4,
					position = "tss"
){
	po <- position

	chaos.tools_data_dir <- "~/.config/chaos.tools"

	gene_density_file <- file.path(chaos.tools_data_dir,"gene_density.txt.gz")

	if(!dir.exists(chaos.tools_data_dir))
	{
		dir.create(chaos.tools_data_dir)
	}

	if(!file.exists(gene_density_file))
	{
		gene_density <- gene_density_calc(
							ref = ref,
							annotation_file = annotation_file,
							resolution = resolution,
							position = position
						)
		fwrite(gene_density,gene_density_file,quote = F, sep = "\t", col.names = T, compress = "gzip")
	}else
	{
		gene_density <- fread(
							gene_density_file
						)[
							genome == ref &
							res == resolution &
							annotation == base_name(annotation_file) &
							pos == position
						]
		if(nrow(gene_density) == 0)
		{
			gene_density <- gene_density_calc(
								ref = ref,
								annotation_file = annotation_file,
								resolution = resolution,
								position = position
							)
			fwrite(gene_density,gene_density_file,quote = F, sep = "\t",append = T,compress = "gzip")
		}
	}

	gene_density
}
