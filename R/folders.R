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

desktop <- function(
			user_name = NA
){
	if(is.na(username))
	{
		user_name <- Sys.info()[7]
	}

	platform <- Sys.info()[1]
	release_name <- Sys.info()[2]

	if(platform == "Windows")
	{
		desk_dir <- paste0("C:/Users/",user_name,"/Desktop")
	}else if(platform == "Linux")
	{
		if(!stringr::str_detect(Sys.info()[2],"WSL"))
		{
			desk_dir <- paste0("~/",user_name, "/Desktop")
		}else
		{
			desk_dir <- paste0("/mnt/c/",user_name,"/Desktop")
		}
	}

	desk_dir
}
