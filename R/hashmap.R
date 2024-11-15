# r2r -- R-Object to R-Object Hash Maps
# Copyright (C) 2021  Valerio Gherardi
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.



#------------------------------ Internal constructor --------------------------#

new_hashmap <- function(
	hash_fn, compare_fn, key_preproc_fn, on_missing_key, default
	)
{
	res <- new_hashtable(hash_fn, compare_fn, key_preproc_fn)
	attr(res, "values") <- new.env(parent = emptyenv(), size = 0L)
	attr(res, "throw") <- (on_missing_key == "throw")
	attr(res, "default") <- default
	class(res) <- c("r2r_hashmap", class(res))
	return(res)
}



#--------------------------------- Constructor --------------------------------#

#' @rdname hashtable
#' @export
hashmap <- function(...,
		hash_fn = default_hash_fn,
		compare_fn = identical,
		key_preproc_fn = identity,
		on_missing_key = "default",
		default = NULL
		)
{
	validate_hashmap_args(...,
			      hash_fn = hash_fn,
			      compare_fn = compare_fn,
			      key_preproc_fn = key_preproc_fn,
			      on_missing_key = on_missing_key
			      )
	m <- new_hashmap(
		hash_fn, compare_fn, key_preproc_fn, on_missing_key, default
		)
	for (pair in list(...))
		insert(m, pair[[1]], pair[[2]])
	return(m)
}

validate_hashmap_args <- function(
	..., hash_fn, compare_fn, key_preproc_fn, on_missing_key
	)
{
	validate_hashset_args(hash_fn, compare_fn, key_preproc_fn)
	for (pair in list(...))
		if (!is.list(pair) || length(pair) != 2L) {
			m <- "'...' arguments must be length two lists."
			rlang::abort(m, class = "r2r_domain_error")
		}
	if (!identical(on_missing_key, "throw") &&
	    !identical(on_missing_key, "default")
	) {
		m <- "'on_missing_key' must be either \"throw\" or \"default\"."
		rlang::abort(m, class = "r2r_domain_error")
	}
}

#----------------------------- Basic R/W operations ---------------------------#

#' @rdname insert
#' @param value an arbitrary R object. Value associated to \code{key}.
#' @export
insert.r2r_hashmap <- function(x, key, value, ...)
{
	keys <- attr(x, "keys")
	values <- attr(x, "values")
	h <- get_env_key(keys, key, attr(x, "hash_fn"), attr(x, "compare_fn"))
	keys[[h]] <- key
	values[[h]] <- value
}

#' @rdname delete
#' @export
delete.r2r_hashmap <- function(x, key)
{
	keys <- attr(x, "keys")
	values <- attr(x, "values")
	h <- get_env_key(keys, key, attr(x, "hash_fn"), attr(x, "compare_fn"))
	if (exists(h, envir = keys, inherits = FALSE)) {
		rm(list = h, envir = keys)
		rm(list = h, envir = values)
	}
	return(invisible(NULL))
}

#' @rdname query
#' @export
query.r2r_hashmap <- function(x, key)
{
	keys <- attr(x, "keys")
	values <- attr(x, "values")
	h <- get_env_key(keys, key, attr(x, "hash_fn"), attr(x, "compare_fn"))
	if (exists(h, envir = keys, inherits = FALSE))
		return(values[[h]])
	else if (attr(x, "throw"))
		rlang::abort("Key not found", class = "r2r_missing_key")
	else
		return(attr(x, "default"))
}



#------------------------------ Subsetting methods ----------------------------#

#' @rdname subsetting_hashtables
#' @export
"[[.r2r_hashmap" <- function(x, i)
	query.r2r_hashmap(x, i)

#' @rdname subsetting_hashtables
#' @export
"[.r2r_hashmap" <- function(x, i)
{
	`validate_[_arg`(i)
	lapply(i, function(key) query.r2r_hashmap(x, key))
}

#' @rdname subsetting_hashtables
#' @export
"[[<-.r2r_hashmap" <- function(x, i, value) {
	insert.r2r_hashmap(x, i, value)
	x
}

#' @rdname subsetting_hashtables
#' @export
"[<-.r2r_hashmap" <- function(x, i, value) {
	`validate_[<-_args`(i, value)
	lapply(seq_along(i),
	       function(n) `[[<-.r2r_hashmap`(x, i[[n]], value[[n]])
	       )
	x
}

#------------------------ Extra key/value access operations -------------------#

#' @rdname values
#' @export
values.r2r_hashmap <- function(x)
	mget_all(attr(x, "values"))

#' @rdname has_key
#' @export
has_key.r2r_hashmap <- function(x, key)
{
	keys <- attr(x, "keys")
	h <- get_env_key(keys, key, attr(x, "hash_fn"), attr(x, "compare_fn"))
	!is.null(keys[[h]])
}



#----------------------------- Property getters/setters -----------------------#

#' @rdname on_missing_key
#' @export
on_missing_key.r2r_hashmap <- function(x)
	if (attr(x, "throw")) "throw" else "default"

#' @rdname on_missing_key
#' @export
`on_missing_key<-.r2r_hashmap` <- function(x, value)
{
	if (identical(value, "throw"))
		attr(x, "throw") <- TRUE
	else if (identical(value, "default"))
		attr(x, "throw") <- FALSE
	else {
		msg <- "'value' must be either \"throw\" or \"default\""
		rlang::abort(msg, class = "r2r_domain_error")
	}
	return(x)
}

#' @rdname default
#' @export
default.r2r_hashmap <- function(x)
	attr(x, "default")

#' @rdname default
#' @export
`default<-.r2r_hashmap` <- function(x, value) {
	attr(x, "default") <- value
	return(x)
}




#---------------------------------- Print methods -----------------------------#

#' @export
print.r2r_hashmap <- function(x, ...)
{
	cat("An r2r hashmap.")
	return(invisible(x))
}

#' @export
summary.r2r_hashmap <- function(object, ...)
{
	cat("An r2r hashmap.")
	return(invisible(object))
}

#' @export
str.r2r_hashmap <- summary.r2r_hashmap
