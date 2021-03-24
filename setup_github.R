## Script for setup access to GitHub 
## Use what you find best

library(usethis)
library(tidyverse)

#### Create a new git and github repo ####

## copy of usethis::use_github function except can define repo_name
use_github_project <- 
  function (organisation = NULL,
            private = FALSE,
            protocol = git_protocol(),
            host = NULL,
            repo_name) {
    usethis:::check_uses_git()
    usethis:::check_default_branch()
    usethis:::challenge_uncommitted_changes(msg = "\n    There are uncommitted changes and we're about to create and push to a new \\\n    GitHub repo")
    usethis:::check_no_origin()
    whoami <- suppressMessages(gh::gh_whoami(.api_url = host))
    if (is.null(whoami)) {
      ui_stop(
        "\n      Unable to discover a GitHub personal access token\n      A token is required in order to create and push to a new repo\n\n      Call {ui_code('gh_token_help()')} for help configuring a token"
      )
    }
    empirical_host <-
      usethis:::parse_github_remotes(glue::glue("{whoami$html_url}/REPO"))$host
    if (empirical_host != "github.com") {
      ui_info("Targeting the GitHub host {ui_value(empirical_host)}")
    }
    owner <- organisation %||% whoami$login
    usethis:::check_no_github_repo(owner, repo_name, host)
    repo_desc <- ""
    repo_desc <- gsub("\n", " ", repo_desc)
    repo_spec <- glue::glue("{owner}/{repo_name}")
    private_string <- if (private)
      "private "
    else
      ""
    ui_done("Creating {private_string}GitHub repository {ui_value(repo_spec)}")
    if (is.null(organisation)) {
      create <- gh::gh(
        "POST /user/repos",
        name = repo_name,
        description = repo_desc,
        private = private,
        .api_url = host
      )
    }
    else {
      create <- gh::gh(
        "POST /orgs/{org}/repos",
        org = organisation,
        name = repo_name,
        description = repo_desc,
        private = private,
        .api_url = host
      )
    }
    origin_url <- switch(protocol,
                         https = create$clone_url,
                         ssh = create$ssh_url)
    withr::defer(usethis:::view_url(create$html_url))
    ui_done("Setting remote {ui_value('origin')} to {ui_value(origin_url)}")
    use_git_remote("origin", origin_url)
    default_branch <- git_branch_default()
    repo <- usethis:::git_repo()
    remref <- glue::glue("origin/{default_branch}")
    ui_done(
      "\n    Pushing {ui_value(default_branch)} branch to GitHub and setting \\\n    {ui_value(remref)} as upstream branch"
    )
    gert::git_push(
      remote = "origin",
      set_upstream = TRUE,
      repo = repo,
      verbose = FALSE
    )
    gbl <- gert::git_branch_list(local = TRUE, repo = repo)
    if (nrow(gbl) > 1) {
      ui_done("\n      Setting {ui_value(default_branch)} as default branch on GitHub")
      gh::gh(
        "PATCH /repos/{owner}/{repo}",
        owner = owner,
        repo = repo_name,
        default_branch = default_branch,
        .api_url = host
      )
    }
    invisible()
  }

use_git_config(user.name = "relund", user.email = "junk@relund.dk")
#git config --global credential.helper 'cache --timeout 3600'. # 3600 sec = 1 hour


use_git()
create_github_token()   # use e.g. repo name
gitcreds::gitcreds_set()
use_git_remote("origin", url = NULL, overwrite = TRUE)  # remove remote if already one
use_github_project(repo_name = "test3")



#### Use existing github repo ####
github_url <- "https://github.com/bss-osca/osca.git"   # git@github.com:bss-osca/osca.git
use_git_config(user.name = "relund", user.email = "junk@relund.dk")
use_git()
create_github_token()
gitcreds::gitcreds_set()
git_remotes()
use_git_remote("origin", url = github_url, overwrite = TRUE)  # set remote

#### Set remote ####
github_url <- "git@github.com:bss-osca/osca.git"
git_remotes()  # current
use_git_remote("origin", url = github_url, overwrite = TRUE)  # set remote
git_remotes()  # check
