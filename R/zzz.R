.onLoad <- function(libname, pkgname){
  register_all_s3_methods()
}
register_s3_method <- function(pkg, generic, class, fun = NULL){
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()){
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

register_all_s3_methods <- function(){
  register_s3_method("base", "as.character", "percent")
  register_s3_method("base", "format", "percent")
  register_s3_method("base", "print", "percent")
  register_s3_method("base", "[", "percent")
  register_s3_method("base", "unique", "percent")
  register_s3_method("base", "rep", "percent")
  register_s3_method("base", "rep_len", "percent")
  register_s3_method("base", "Ops", "percent")
  register_s3_method("base", "Math", "percent")
}

on_package_load <- function(pkg, expr){
  if (isNamespaceLoaded(pkg)){
    expr
  } else {
    thunk <- function(...) expr
    setHook(packageEvent(pkg, "onLoad"), thunk)
  }
}
