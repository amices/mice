source("https://install-github.me/r-lib/revdepcheck")
library("revdepcheck")

# the following takes one hour
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
