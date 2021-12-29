juicer_tool <- function(
					cmd = "eigenvector",
					hic_file,
					chr,
					resolution = 1e6,
					norm = "KR",
					juicer_tool_path = "~/local/juicer/common/juicer_tools.jar",
					matrix_type = 1
){
	res <- format(resolution,scientific = F,trim = T)

	result <- paste(
					"java -jar",juicer_tool_path, cmd, "-p",norm, hic_file, chr,"BP",res,sep = " "
				) %>%
				system(inter = T)

	if(cmd == "eigenvector")
	{
		as.data.table(
			result
		)[
			,.(eigen = result)
		][
			eigen == "NaN",
			eigen := 0
		][
			,eigen := as.numeric(eigen)
		][]

	}else if(cmd == "pearsons")
	{
		matrix <- as.data.table(result[-1]) %>% 
					separate_col("V1",sep = " ") %>%
					apply(1,as.numeric) %>% 
					as.data.table() %>%
					setnames(paste0("bin_",1:ncol(.)))

		matrix[matrix == "NaN"] <- 0

		if(matrix_type == 1)
		{
			matrix
		}else if(matrix_type == 2)
		{
			matrix[
				,x := paste0("bin_",1:.N)
			] %>%
			melt("x", paste0("bin_",1:nrow(.)),variable.name = "y", value.name = "pearsons")
		}
	}
}


pearsons_plot <- function(
					hic_file,
					resolution,
					chr,
					min = -0.5,
					max = 0.5,
					min_color = "#00004a",
					max_color = "#e64b35",
					smooth = TRUE
){
	p_base <-	juicer_tool(
					"pearsons",
					hic_file,
					resolution = resolution,
					chr = chr
				)[
					,row := paste0("bin_",1:.N)
				] %>%
				melt(
					"row",
					variable.name = "col", 
					value.name = "pearsons",
					fill = NA
				) %>%
				.[
					,row := factor(.$row, levels = str_sort(unique(.$row), numeric = T, decreasing = T))
				] %>%
				.[
					,col := factor(.$col, levels = str_sort(unique(.$col), numeric = T))
				] %>%
				.[
					pearsons >= max,
					pearsons := max
				] %>%
				.[
					pearsons <= min,
					pearsons := min
				] %>%
				ggplot(
					aes(col,row,fill = pearsons)
				) + 
				scale_fill_gradient2(
					limits = c(min,max),
					low = min_color,
					high = max_color
				) + 
				theme(
					axis.text = element_blank(),
					axis.title = element_blank(),
					axis.ticks = element_blank(),
					legend.position = "none"
				)
	if(isFALSE(smooth))
	{
		p <- p_base + geom_tile()
	}else
	{
		p <- p_base + geom_raster()
	}	

	p
}