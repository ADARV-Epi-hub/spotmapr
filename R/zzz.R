# Null-coalesce operator (not exported)
`%||%` <- function(a, b) if (is.null(a)) b else a

# Friendly welcome shown when the package is attached, to help first-time
# (non-coder) users know how to launch the tool.
.onAttach <- function(libname, pkgname) {
  v <- tryCatch(as.character(utils::packageVersion(pkgname)), error = function(e) "")
  packageStartupMessage(
    "SpotMap ", if (nzchar(v)) paste0("v", v, " ") else "",
    "- interactive spot maps for India (created by ADARV)\n",
    "\n",
    "  To build your map, run:\n",
    "      spotmap()                                  # interactive wizard\n",
    "      spotmap(\"data.csv\", output = \"map.html\")   # or a direct call"
  )
}
