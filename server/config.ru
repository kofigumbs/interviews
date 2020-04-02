require "net/http"
require "sinatra"
require_relative "daily"
require_relative "repository"

DAILY = Daily.new(ENV.fetch("DAILY_API_KEY"))
STATS = Repository.new(:stats) # opaque "database" reference

# Allow access by any method, from any origin, for any route.
# Since we're not authenticating yet, there's no reason to be too restrictive
# on access controls.
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
  room_id = DAILY.create_room!.fetch(:id)
  STATS[room_id] = []
  { id: room_id }.to_json
end

post("/stats/:room_id") do
  room_id = params.fetch(:room_id)
  STATS.transaction do
    stats = STATS[room_id]
    halt 401 if stats.nil?
    stats << JSON.parse(request.read)
    STATS[room_id] = stats
  end
  {}.to_json
end

run Sinatra::Application
