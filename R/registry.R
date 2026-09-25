# registry.R -- Name a tool so a plan can reference it instead of carrying a
# closure.

.OAP_TOOL_REGISTRY <- new.env(parent = emptyenv())

#' Register a tool by name
#'
#' A registered tool can be referenced from a node by its name, which is what
#' lets a plan be written to a file and performed again in another session.
#' The registry is per session; a plan loaded from a file needs its tools
#' registered again before it is performed.
#'
#' @param name A single string.
#' @param fn A closure `function(inputs, data, node)` returning an
#'   [orchestraManifest::orchestra_manifest].
#'
#' @returns `name`, invisibly.
#'
#' @seealso [resolve_tool()], [plan_to_yaml()].
#'
#' @author Max Moldovan
#'
#' @examples
#' register_tool("always_effect", function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "example", "example:constant")
#' })
#' resolve_tool("always_effect")
#'
#' @export
register_tool <- function(name, fn) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name),
            is.function(fn))
  assign(name, fn, envir = .OAP_TOOL_REGISTRY)
  invisible(name)
}

#' Resolve a tool reference
#'
#' Three reference forms are accepted. `"<name>"` is a tool registered with
#' [register_tool()]. `"pkg::fn"` is an exported function of an installed
#' package. `"script:<path>::<fn>"` sources an R script into a fresh
#' environment and returns the function it defines under that name; the
#' script runs with the privileges of the session, so only reference scripts
#' you would source yourself.
#'
#' @param ref A single string in one of the three forms.
#'
#' @returns The function the reference names. An unknown reference is an
#'   error.
#'
#' @seealso [register_tool()].
#'
#' @author Max Moldovan
#'
#' @examples
#' resolve_tool("stats::median")
#'
#' @export
resolve_tool <- function(ref) {
  stopifnot(is.character(ref), length(ref) == 1L)
  if (startsWith(ref, "script:")) {
    spec  <- sub("^script:", "", ref)
    parts <- strsplit(spec, "::", fixed = TRUE)[[1]]
    if (length(parts) != 2L) {
      stop("script ref must be 'script:<path>::<fn>'", call. = FALSE)
    }
    path <- path.expand(parts[1])
    fn   <- parts[2]
    if (!file.exists(path)) {
      stop(sprintf("script not found: %s", path), call. = FALSE)
    }
    e <- new.env(parent = globalenv())
    sys.source(path, e)
    if (!exists(fn, envir = e, inherits = FALSE) ||
        !is.function(get(fn, envir = e))) {
      stop(sprintf("script '%s' has no function '%s'", path, fn),
           call. = FALSE)
    }
    return(get(fn, envir = e))
  }
  if (grepl("::", ref, fixed = TRUE)) {
    parts <- strsplit(ref, "::", fixed = TRUE)[[1]]
    return(getExportedValue(parts[1], parts[2]))
  }
  if (exists(ref, envir = .OAP_TOOL_REGISTRY, inherits = FALSE)) {
    return(get(ref, envir = .OAP_TOOL_REGISTRY))
  }
  stop(sprintf(paste0("unknown tool ref '%s'; register it with ",
                      "register_tool(), or use a 'pkg::fn' or ",
                      "'script:<path>::<fn>' ref"), ref), call. = FALSE)
}
