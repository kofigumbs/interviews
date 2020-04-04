require "sequel"
require "sinatra"
require_relative "daily"

DAILY = Daily.new ENV.fetch("DAILY_API_KEY")

DB = Sequel.connect ENV.fetch("DATABASE_URL")
DB[File.read File.join(__dir__, "schema.sql")]

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

get("/rooms") do
  DB[:rooms].reverse(:created_at).to_a.to_json
end

post("/rooms") do
  room = DAILY.create_room!.slice("url", "name")
  DB[:rooms].returning.insert(room).first.to_json
end

get("/rooms/:room_id/stats") do
  DB[:stats].where(room_id: params.fetch(:room_id)).to_a.to_json
end

post("/rooms/:room_id/stat") do
  room_id = params.fetch(:room_id)
  stat = JSON.parse(request.body.read)
  DB[:stats].insert stat.merge(room_id: room_id)
  {}.to_json
end

run Sinatra::Application
