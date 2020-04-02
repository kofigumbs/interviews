require "net/http"
require "sinatra"
require_relative "daily"
require_relative "repository"

DAILY = Daily.new(ENV.fetch("DAILY_API_KEY"))
STATS = Repository.new(:stats) # opaque "database" reference

# Allow access by any method, from any origin, for any route.
# Since we're not authenticating yet, there's no reason to be too restrictive
# on access controls.
#
before do
  headers({
    "Access-Control-Allow-Origin" => "*",
    "Access-Control-Allow-Methods" => "*",
  })
end

# ROUTES
#
#   Using Sinatra's "Classic Style" since we are only runing one application
#   per Ruby process. See "Modular vs. Classic Style" section of intro for
#   more info: <http://sinatrarb.com/intro.html>

post("/room") do
  room = DAILY.create_room!
  STATS[room.url] = []
  room.to_json
end

post("/stats") do
  url, new_stat = JSON.parse(request.read).fetch_values(:url, :stat)
  STATS.transaction do
    old_stats = STATS[url]
    halt 403 if old_stats.nil?
    old_stats << new_stat
    STATS[url] = stats
  end
  {}.to_json
end

run Sinatra::Application
