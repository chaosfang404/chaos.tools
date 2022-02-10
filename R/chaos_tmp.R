chaos_tmp <-	function(
					n = 15,
					type = "default",
					replace = TRUE,
					date = TRUE
){
	if(isTRUE(date))
	{
		d <- paste0("_",Sys.Date())
	}else
	{
		d <- NULL
	}
	
	if(type == "default")
	{
		source <- c(LETTERS,letters,0:9)
	}
	else if(type == "number" | type == "n")
	{
		source <- 0:9
	}else if(type == "character" | type == "c")
	{
		source <- c(LETTERS,letters)
	}else
	{
		greak <- c("α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ","ν","ξ","ο","π","ρ","σ","ς","τ","υ","φ","χ","ψ","ω")
		symbol <- c("·","~","！","@","#","$","%","^","&","*","(",")","_","-","+","=","|",",",".","/","<",">","?",";","'",":","[","]","{","}")

		if(type == "greak_mixed" | type == "g")
		{
			source <- c(LETTERS,letters,0:9,greak)
		}else if(type == "symbol_mixed" | type == "s")
		{
			source <- c(LETTERS,letters,0:9,symbol)
		}else if(type == "mixed" | type == "m")
		{
			source <- c(LETTERS,letters,0:9,greak,symbol)
		}
		
	}

	sample(x = source, size = n, replace = replace) %>%
	paste(collapse = "") %>%
	paste0(d)
}