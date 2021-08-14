scratch = function()
{
	for (i in dir("/scratch"))
	{
		tmp_path=file.path("/scratch",i,"chaos")
		if (dir.exists(tmp_path))
		{
			return(tmp_path)
		}
	}
}
