library(usethis)

## Use existing github repo
use_git_config(user.name = "relund", user.email = "junk@relund.dk")
use_git()
create_github_token()
gitcreds::gitcreds_set()
git_remotes()
use_git_remote("origin", url = "https://github.com/bss-osca/osca.git", overwrite = TRUE)  # set remote