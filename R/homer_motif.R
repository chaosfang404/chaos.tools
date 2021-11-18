homer_motif <- function(
					files,
					name = NA
){
	tmp <- function(
				x
	){
		fread(
			x,
			skip = 1
		) %>% 
		setnames(
			c("A","C","G","T")
		) %>% 
		t()
	}

	list <- lapply(files,tmp)
	
	if(length(name) == 1)
	{
		if(is.na(name))
		{
			name <- base_name(files)
		}
	}
	names(list) <- name
	list
}

motif_plot <- function(
					files,
					name = NA,
					remove_text = TRUE
){
	p_base <- homer_motif(
					files,
					name = name
				) %>% 
				ggseqlogo(
					method = "prob",
					facet = "wrap",
					ncol = 1
				) %>%
				suppressWarnings()
	if(isTRUE(remove_text))
	{
		p <- p_base + 
				theme_void() + 
				theme(strip.text = element_blank())
	}else
	{
		p <- p_base
	}

	p

}

homer_motif_info <- function(
						files,
						table = FALSE
){
	tmp <- function(
				x
	){
		as.data.table(
			read.table(
				text = readLines(x,1),
				sep = "\t"
			)
		)[
			,motif := str_replace(V1,"[>]","")
		][
			,name := base_name(x)
		][
			,logP := V4
		][
			,q_value := V5
		] %>%
		separate_col("V2", select = 1:2, into = c("factor","data_source"), sep = "/") %>%
		separate_col("V6", sep = ",", into = c("T","B","p_value")) %>%
		separate_col("T",select = 2,sep = ":") %>%
		separate_col("B",select = 2,sep = ":") %>%
		separate_col("p_value",select = 2,sep = ":") %>%
		.[,.(name,motif,factor,data_source,p_value,q_value,T,B)]
	}

	dt <- files %>%
			data.table() %>%
			apply(1,tmp) %>%
			rbindlist()

	if(isFALSE(table))
	{
		dt
	}else
	{
		image_dir <- file.path(getwd(),"motif_image")

		if(!dir.exists(image_dir)){dir.create(image_dir)}

		generate_png <- function(
							x
		){
			ggsave(
				paste0(image_dir,"/",base_name(x),".png"),
				height = 6,
				width = 20
			)
			print(motif_plot(x))
			dev.off()
		}

		sapply(files,generate_png)

		png_files <- paste0(image_dir,"/",base_name(files),".png")

		dt[,motif := ""] %>%
		kbl(
#			"latex",
			align ="c"
		) %>% 
		kable_classic(
			full_width = F
		) %>%
		column_spec(
			2,
			image = spec_image(png_files,40,12,res = 30)
		) %>%
		save_kable(
			"motif_table.pdf",
			self_contained = T
		)
	}
}
