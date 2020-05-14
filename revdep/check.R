# library(devtools)
# install_github("r-lib/revdepcheck")

# using tools: tried, but seemed to stall, not used
# save mice_x.y.z.tar.gz in dir specified below
#dir <- path.expand("~/Package/mice/checkdep")
#check_packages_in_dir(dir,
#                      check_args = c("--as-cran", ""),
#                      reverse = list(repos = getOption("repos")["CRAN"]))

library(revdepcheck)
revdep_reset()

# the following takes two hours
revdep_check(num_workers = 3)

# print out results
revdep_summary()
revdep_details(revdep = "smartdata")

# commit 
system("git add revdep/*.md")
system("git commit -m 'Update revdep results'")
system("git push -u origin HEAD")

# signal package authors


# after successfully submitted to CRAN, clean up 
revdep_reset()
