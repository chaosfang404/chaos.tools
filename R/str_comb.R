str_comb <- function(
				c1,
				c2,
				sep = "_"
){
    all <- NULL
    for (i in c1) 
	{
        all <- c(all,paste(i,c2,sep = "_"))
	}
    all
}
