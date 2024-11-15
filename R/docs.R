#------------------------------- Hash table documentation ---------------------#
#'
#' @title Hash maps and sets
#'
#' @author Valerio Gherardi
#'
#' @description Objects of class \code{hashmap} and \code{hashset} store
#' collections of key/value pairs (\code{hashmap}), or just keys
#' (\code{hashset}), providing constant time read and write operations. Both
#' the keys and the optional values can be arbitrary R objects. \code{hashmap}s
#' and \code{hashset}s provide an R implementation of
#' \href{https://en.wikipedia.org/wiki/Hash_table}{hash tables}.
#'
#' See \link[r2r]{hashtable_methods} for an overview of the available methods
#' for \code{hashmap} and \code{hashset} class objects. Note that both these
#' classes have a common parent class \code{hashtable}, from which they can also
#' inherit S3 methods.
#'
#' @param ... these arguments can be used to specify a set of initial elements
#' to be inserted in the \code{hashmap} or \code{hashset}. For \code{hashmap()},
#' each of these should be a \code{list} of two elements (a key-value pair).
#' @param hash_fn the (string valued) hash function applied to keys.
#' Required for advanced use only; see Details.
#' @param compare_fn the (boolean valued) comparison function used for
#' testing key equality. Required for advanced use only; see Details.
#' @param key_preproc_fn key pre-processing function applied to keys before
#' hashing and comparison. Required for advanced use only; see Details.
#' @param on_missing_key either \code{"throw"} or \code{"default"}.
#' In the second case, an exception is thrown upon query of a missing key; otherwise, a default value
#' (specified through the \code{default} argument) is returned.
#' @param default default value associated with missing keys. This will be
#' returned only if \code{on_missing_key} is equal to \code{"default"}.
#'
#' @return a \code{hashmap} and a \code{hashset} class object for
#' \code{hashmap()} and \code{hashset()}, respectively.
#'
#' @details \code{hashmap}s and \code{hashset}s implement hash tables,
#' building on top of base R built-in \code{\link[base]{environment}}s,
#' which by themselves are, essentially, string -> R object hash maps.
#' In order to handle keys of non-string type, a string valued hash function
#' \code{\link[r2r]{default_hash_fn}()} is provided, which leverages on
#' \code{\link[digest]{digest}()} for handling arbitrary R object keys.
#'
#' By default, key equality is tested through \code{\link[base]{identical}()}.
#' For some use cases, it may be sensible to employ a different comparison
#' function, which can be assigned through the \code{compare_fn} argument. In this
#' case, one must also make sure that equal (in the sense of
#' \code{compare_fn()})
#' keys get also assigned the same hashes by \code{hash_fn()}. A simple way to
#' ensure this is to use to use a key pre-processing function, to be applied
#' before both key hashing \emph{and} comparison. The \code{key_preproc_fn}
#' argument provides a short-cut to this, by automatically composing both the
#' provided \code{hash_fn()} and \code{compare_fn()} functions with
#' \code{key_preproc_fn()} function. This is illustrated in an example below.
#'
#' One might also want to set set specific hash and/or key comparison functions
#' for efficiency reasons, e.g. if the \code{default_hash_fn()} function produces
#' many collisions between inequivalent keys.
#'
#' When \code{on_missing_key} is equal to \code{"throw"}, querying a missing
#' key will cause an error. In this case, an rlang \link[rlang]{abort}
#' condition  of class \code{"r2r_missing_key"} is returned, which can be useful
#' for testing purposes.
#'
#' @examples
#' m <- hashmap(
#'         list("foo", 1),
#'         list("bar", 1:5),
#'         list(data.frame(x = letters, y = LETTERS), "baz")
#'         )
#' m[[ data.frame(x = letters, y = LETTERS) ]]
#'
#' # Set of character keys, case insensitive.
#' s <- hashset("A", "B", "C", key_preproc = tolower)
#' s[["a"]]
#'
#' @seealso \link[r2r]{hashtable_methods}
#' @name hashtable
NULL



#----------------------- Hash table methods documentation ---------------------#
#'
#' @title Methods for S3 classes \code{hashmap} and \code{hashset}
#'
#' @author Valerio Gherardi
#' @md
#' @description This page provides an overview of the available methods for
#' \code{hashmap} and \code{hashset} objects (and for their common parent class
#' \code{hashtable}). We list methods based on the general type of task
#' addressed.
#'
#' ### Basic read/write operations
#' - \code{\link[r2r]{insert}()}
#' - \code{\link[r2r]{delete}()}
#' - \code{\link[r2r]{query}()}
#' - \link[r2r]{subsetting_hashtables}: \code{`[[`}, \code{`[[<-`}, \code{`[`}
#' and \code{`[<-`}
#' ### Size of hash table
#' - \code{\link[r2r]{length.r2r_hashtable}()}
#' ### Other key or value access operations
#' - \code{\link{keys}()}
#' - \code{\link{values}()}
#' - \code{\link{has_key}}, \code{\link[r2r:has_key]{%has_key%}}
#' ### Get/set hashtable properties
#' - \code{\link{hash_fn}()}
#' - \code{\link{compare_fn}()}
#' - \code{\link{on_missing_key}()}
#' - \code{\link{default}()}
#' @name hashtable_methods
NULL



#------------------------- Subsetting ops. documentation ----------------------#
#'
#' @title Subsetting \code{hashset}s and \code{hashmap}s
#'
#' @author Valerio Gherardi
#' @md
#' @description Subsetting operators \code{`[[`} and \code{`[`} for
#' \code{hashset}s and \code{hashmap}s provide an equivalent synthax for the
#' basic read/write operations performed by \code{\link{insert}()},
#' \code{\link{delete}()} and \code{\link{query}()}.
#'
#' @param x an \code{hashset} or \code{hashmap}.
#' @param i for \code{`[[`}-subsetting, an arbitrary R object, the key to be
#' queried or inserted/deleted from the hash tables. For \code{`[`}-subsetting,
#' a list or an atomic vector whose individual elements correspond to the keys.
#' @param value for \code{`[[`}-subsetting: \code{TRUE} or \code{FALSE} if
#' \code{x} is an \code{hashset}, an arbitrary R object if \code{x} is an
#' \code{hashmap}. In the case of \code{hashset}s, setting a key's value to
#' \code{TRUE} and \code{FALSE} is equivalent to inserting and deleting,
#' respectively, such key from the set. For \code{`[`}-subsetting, \code{value}
#' must be a list or an atomic vector of the same length of \code{i},
#' whose individual elements are the values associated to the corresponding
#' keys in the hash table.
#' @return the replacement forms (`[[<-` and `[<-`) always return `value`.
#' \code{`[[`} returns \code{TRUE} or \code{FALSE} if
#' \code{x} is an \code{hashset}, an arbitrary R object if \code{x} is an
#' \code{hashmap} and \code{i} is a valid key; when \code{i} is not a key, the
#' behaviour for \code{hashmap}s depends on the value of
#' \code{\link{on_missing_key}(x)}.
#' The \code{`[`} operator returns a list of the same length of \code{i}, whose
#' k-th element is given by \code{x[[ i[[k]] ]]} (the remark on missing keys for
#' hashmaps applies also here).
#' @name subsetting_hashtables
NULL
