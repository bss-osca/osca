library(usethis)

## Use existing github repo
use_git_config(user.name = "relund", user.email = "junk@relund.dk")
use_git()
create_github_token()
gitcreds::gitcreds_set()
use_git_remote("origin", url = NULL, overwrite = TRUE)  # remove remote
create_from_github