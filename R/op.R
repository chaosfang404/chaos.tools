op <- function(
		file
){
	
	ext <- str_split(file,"[.]") %>% 
			unlist() %>% 
			rev() %>% 
			head(1)

	if(ext == "pdf")
	{
		program <- "/mnt/c/Users/Chao/AppData/Local/SumatraPDF/SumatraPDF.exe"
	}else if (ext == "png" | ext == "jpeg" | ext == "bmp")
	{
		program <- "/mnt/c/Program Files/Honeyview/Honeyview.exe"
	}else
	{
		program <- "/mnt/c/Program Files/Sublime Text/subl.exe"
	}

	cmd <- paste(shQuote(program),file,sep = " ")
	system(cmd,inter = F)
}