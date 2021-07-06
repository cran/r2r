## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(hash)
library(r2r)
packageVersion("hash")
packageVersion("r2r")

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
	data.frame(
		Feature = c(
			"Basic data structure",
			"Arbitrary type keys", 
			"Arbitrary type values",
			"Arbitrary hash function",
			"Arbitrary key comparison function",
			"Throw or return default on missing keys",
			"Hash table inversion"
		),
		r2r = c("R environment", "X", "X", "X", "X", "X", ""),
		hash = c("R environment", "", "X", "", "", "", "X")
		),
	align = "c",
	caption = "Features supported by {r2r} and {hash}"
	)

## -----------------------------------------------------------------------------
library(microbenchmark)

## -----------------------------------------------------------------------------
N <- 1e4

## -----------------------------------------------------------------------------
chars <- c(letters, LETTERS, 0:9)
random_keys <- function(n) paste0(
	sample(chars, n, replace = TRUE),
	sample(chars, n, replace = TRUE),
	sample(chars, n, replace = TRUE),
	sample(chars, n, replace = TRUE),
	sample(chars, n, replace = TRUE)
	)

set.seed(840)
keys <- random_keys(N)
values <- rnorm(N)


## -----------------------------------------------------------------------------
microbenchmark(
	`r2r_[[<-` = {
		for (i in seq_along(keys))
			m_r2r[[ keys[[i]] ]] <- values[[i]]
	},
	`r2r_[<-` = { m_r2r[keys] <- values },
	`hash_[[<-` = { 
		for (i in seq_along(keys))
			m_hash[[ keys[[i]] ]] <- values[[i]]
	},
	`hash_[<-` = m_hash[keys] <- values,
	
	times = 30, 
	setup = { m_r2r <- hashmap(); m_hash <- hash() }
)

## -----------------------------------------------------------------------------
microbenchmark(
	`r2r_[[` = { for (key in keys) m_r2r[[ key ]] },
	`r2r_[` = { m_r2r[ keys ] },
	`hash_[[` = { for (key in keys) m_hash[[ key ]] },
	`hash_[` = { m_hash[ keys ] },
	
	times = 30,
	setup = { 
		m_r2r <- hashmap(); m_r2r[keys] <- values
		m_hash <- hash(); m_hash[keys] <- values
	}
)

## -----------------------------------------------------------------------------
set.seed(841)
new_keys <- random_keys(N)
microbenchmark(
	`r2r_[[_bis` = { for (key in new_keys) m_r2r[[ key ]] },
	`hash_[[_bis` = { for (key in new_keys) m_hash[[ key ]] },
	
	times = 30,
	setup = { 
		m_r2r <- hashmap(); m_r2r[keys] <- values
		m_hash <- hash(); m_hash[keys] <- values
	}
)

## -----------------------------------------------------------------------------
set.seed(842)
mixed_keys <- sample(c(keys, new_keys), N)
microbenchmark(
	r2r_has_key = { for (key in mixed_keys) has_key(m_r2r, key) },
	hash_has_key = { for (key in new_keys) has.key(key, m_hash) },
	
	times = 30,
	setup = { 
		m_r2r <- hashmap(); m_r2r[keys] <- values
		m_hash <- hash(); m_hash[keys] <- values
	}
)

## -----------------------------------------------------------------------------
microbenchmark(
	r2r_delete = { for (key in keys) delete(m_r2r, key) },
	hash_delete = { for (key in keys) del(key, m_hash) },
	hash_vectorized_delete = { del(keys, m_hash) },
	
	times = 30,
	setup = { 
		m_r2r <- hashmap(); m_r2r[keys] <- values
		m_hash <- hash(); m_hash[keys] <- values
	}
)

