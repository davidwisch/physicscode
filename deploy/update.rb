#!/usr/bin/env ruby

REPO_LOC = "git://location/of/git/repo"
TARGET_DIR = "/path/to/checkout/directory"
DEPLOY_DIR = "/path/to/deploy/directory"

#clone the repository
if File.exists? TARGET_DIR
        Dir.chdir(TARGET_DIR)
        `git pull`
else
        `git clone #{REPO_LOC} #{TARGET_DIR}`
end

#cd into site/ directory so jekyll can build it.
#Jekyll can't easily find out plugins if we don't do this.
Dir.chdir(File.join(TARGET_DIR, "site"))

#build the site
`jekyll --no-auto --rdiscount --pygments #{DEPLOY_DIR}`

