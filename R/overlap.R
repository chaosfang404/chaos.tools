overlap <- function(
				x,
				y
			)
{
	overlap_raw <-foverlaps(
						x,
						y,
						nomatch = NULL
					)
	col_length_1 <- ncol(x)
	col_length_2 <- ncol(y)

	overlap_ratio <- function(
					x
	){
		a1 <- as.numeric(x[2])
		a2 <- as.numeric(x[3])
		b1 <- as.numeric(x[col_length_1 + 1])
		b2 <- as.numeric(x[col_length_1 + 2])
	
	
		c <- c(a1,a2,b1,b2) %>%
				.[order(.)]
	
		overlap_length <- c[3] - c[2]

		data.table(
			r1 = overlap_length/(a2 - a1),
			r2 = overlap_length/(b2 - b1),
			overlap_length,
			overlap_start = c[2],
			overlap_end = c[3],
			full_start = c[1],
			full_end = c[4]
		)
	}

	ratio <- apply(overlap_raw,1,overlap_ratio) %>% 
				rbindlist()

	cbind(overlap_raw,ratio)
}


