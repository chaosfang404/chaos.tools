gene_density_calc <- function(
							ref = "hg19",
							annotation_file = "/mnt/d/Data/Reference/hg19/annotation/gencode.v38lift37.annotation.gff3.gz",
							resolution = 1e4
){
	slice <- function(
				x
	){
		chr_length <- as.numeric(x[2])
		seq_dt(
			start = 0,
			end = chr_length,
			slice_size = resolution
		)[
			,chr := x[1]
		][
			,.(chr,start = V1,end = V2)
		]
	}

	chr_info <- chr_size(ref = ref, mit = T) %>%
				apply(1,slice) %>%
				rbindlist() %>%
				setkey()

	gene_info <- as.data.table(
					import(annotation_file)
				)[
					type == "gene",
					.(chr = seqnames, start, end, gene_name)
				][
					,chr := as.character(chr)
				] %>%
				setkey(chr,start,end)

	foverlaps(
		chr_info,
		gene_info
	)[
		!is.na(gene_name),
		gene_number := .N,
		.(chr,i.start,i.end)
	][
		,.(chr,start = i.start,end = i.end,gene_number)
	][
		is.na(gene_number),
		gene_number := 0
	][
		,res := resolution
	][
		,genome := ref
	][
		,annotation := base_name(annotation_file)
	] %>%
	unique()
}

gene_density <- function(
					ref = "hg19",
					annotation_file = "/mnt/d/Data/Reference/hg19/annotation/gencode.v38lift37.annotation.gff3.gz",
					resolution = 1e4
){
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
							resolution = resolution
						)
		fwrite(gene_density,gene_density_file,quote = F, sep = "\t", col.names = T, compress = "gzip")
	}else
	{
		gene_density <- fread(
							gene_density_file
						)[
							genome == ref &
							res == resolution &
							annotation == base_name(annotation_file)
						]
		if(nrow(gene_density) == 0)
		{
			gene_density <- gene_density_calc(
							ref = ref,
							annotation_file = annotation_file,
							resolution = resolution
						)
			fwrite(gene_density,gene_density_file,quote = F, sep = "\t",append = T,compress = "gzip")
		}
	}

	gene_density
}
