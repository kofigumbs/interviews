source "https://rubygems.org"

ruby "~> 2.6.5"

group :client do
  gem "jekyll"
end

# <https://jekyllrb.com/docs/plugins/installation/#the-jekyll_plugins-group>
group :client, :jekyll_plugins do
  gem "jekyll-environment-variables"
end

group :server do
  gem "httparty"
  gem "pg"
  gem "sequel"
  gem "sinatra"
end
