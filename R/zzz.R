

# ipkgs <- installed.packages()
# ipkgs[grep("fusion", ipkgs[,1]), c(1,3)]
# ipkgs[grep("nmr.parser", ipkgs[,1]), c(1,3)]
# ipkgs[grep("rldx", ipkgs[,1]), c(1,3)]
# utils::packageVersion("fusion")
# utils::packageVersion("rldx")
message(cat(crayon::yellow("'fusion' versions > 0.0.1 introduced breaking changes.")))
message(cat(crayon::yellow("Use release 0.0.1 if not sure.")))
message(cat(crayon::yellow("remotes::install_github('phenological/fusion@0.0.1')")))
