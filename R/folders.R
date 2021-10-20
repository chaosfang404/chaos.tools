scratch <- function()
{
	for (i in dir("/scratch"))
	{
		tmp_path <- file.path("/scratch",i,"chaos")
		if (dir.exists(tmp_path))
		{
			return(tmp_path)
		}
	}
}

desktop <- function()
{
	platform <- Sys.info()[1]
	user_name <- Sys.info()[7]

	if(platform == "Windwos")
	{
		desk_dir <- paste0("C:/Users/",user_name,"/Desktop")
	}else if(platform == "Linux")
	{
		desk_dir <- paste0("~/",user_name, "/Desktop")
	}

	return(desk_dir)
}
