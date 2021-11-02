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
			username = NA
){
	if(is.na(username))
	{
		username <- Sys.info()[7]
	}

	platform <- Sys.info()[1]

	if(platform == "Windows")
	{
		desk_dir <- file.path("C:/Users",username,"Desktop")
	}else if(platform == "Linux")
	{
		if(!stringr::str_detect(Sys.info()[2],"WSL"))
		{
			desk_dir <- file.path("~",username, "Desktop")
		}else
		{
			desk_dir <- file.path("/mnt/c/Users",username,"Desktop")
		}
	}

	desk_dir
}
