# When starting a new R session, a specific directory is added to the libPath.
# It's called lib. As it is on the first libPath position,
# packages are installed into this directory by default. This enables working in
# a sandbox.

.First <- function() {
  .libPaths(new = c(paste(getwd(), "lib/", sep = "/"), .libPaths()))
}

.First()

### Limit memory if interactive user & not working on VM ###
local({
  r_memory_limit <- 13e9
  
  # TRUE if not working on VM:
  using_local_machine <- grepl(pattern = "inwt-l",
                               x = tolower(Sys.info()["nodename"]))
  
  if (interactive() & using_local_machine) {
    message("Limiting memory for R to ",  R.utils::hsize(r_memory_limit))
    unix::rlimit_as(r_memory_limit, r_memory_limit)
  }
})
