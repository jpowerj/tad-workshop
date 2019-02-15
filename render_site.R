# Used to be cool. Now it just runs render_site()
library(rmarkdown)

# Delete tad_old
#file.remove("../../git_io/tad_old/")
# Rename the old version tad_old
#file.rename("../../git_io/tad/","../../git_io/tad_old/")

render_site()
