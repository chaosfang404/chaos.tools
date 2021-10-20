hic_inter <- function(
				hic_file,
				chr_list = c(1:22,"X","Y"),
				norm = "NONE",
				resolution = 1e6
){
	chr_list <- as.character(chr_list)

	if(length(chr_list == 1))
	{
		chr_list <- rep(chr_list,2)
	}

	chr_list_dt <- combn(chr_list,2) %>% 
					t() %>% 
					data.table()

	inter_data <- data.table(NULL)
	for (i in 1:nrow(chr_list_dt))
	{
		chr1 <- chr_list_dt[i,V1]
		chr2 <- chr_list_dt[i,V2]
		tmp <- strawr::straw(norm, hic_file, chr1, chr2, "BP", resolution) %>%
				data.table() %>%
				mutate_dt(chr1 = chr1,chr2 = chr2) %>%
				replace_na_dt(to = 0)
		inter_data <- rbind(inter_data,tmp)
	}
	inter_data
}

hic_intra <- function(
				hic_file,
				chr_list = c(1:22,"X","Y"),
				norm = "NONE",
				resolution = 1e6
){
	chr_list <- as.character(chr_list)

	intra_data <- data.table(NULL)
	for (i in 1:length(chr_list))
	{
		tmp <- strawr::straw(norm, hic_file, i, i, "BP", resolution) %>%
				data.table() %>%
				mutate_dt(chr1 = i,chr2 = i) %>%
				replace_na_dt(to = 0)
		intra_data <- rbind(intra_data,tmp)
	}
	intra_data
}
