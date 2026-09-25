# zzz.R -- Load-time binding of the contract vocabulary.
#
# The set of inferential targets an edge may be typed with is owned by
# orchestraManifest. It is read at load time rather than copied into this
# package so that a new target added there is accepted here without a
# release.

.OAP_OPERATORS <- c("compute", "abstain_gate", "triangulate")
.OAP_CONTRACTS <- "any"

.onLoad <- function(libname, pkgname) {
  ns <- topenv()
  assign(".OAP_CONTRACTS",
         c("any", orchestraManifest::inferential_targets()),
         envir = ns)
  invisible(NULL)
}
