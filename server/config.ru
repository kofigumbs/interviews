require "sequel"
require "sinatra"
require_relative "daily"

# Initialize Daily.co API wrapper
DAILY = Daily.new ENV.fetch("DAILY_API_KEY")

# Initialize Postgres database with our schema
DB = Sequel.connect ENV.fetch("DATABASE_URL")
DB.run File.read(File.join(__dir__, "schema.sql"))
DB.extension :pg_json

# Allow access by any method, from any origin, for any route.
# Since we're not authenticating yet, there's no reason to be too restrictive
# on access controls. This is true for errors as well.
ACCESS_CONTROL =  {
  "Access-Control-Allow-Origin" => "*",
  "Access-Control-Allow-Methods" => "*",
}

before do
  headers ACCESS_CONTROL
end

error do
  headers ACCESS_CONTROL
  puts env["rack.errors"] # make-shift logging
  {}.to_json
end

# ROUTES
#
#   Using Sinatra's "Classic Style" since we are only runing one application
#   per Ruby process. See "Modular vs. Classic Style" section of intro for
#   more info: <http://sinatrarb.com/intro.html>

# Returns Room objects, ordered from youngest to oldest
#
#   [{
#     id: Int,
#     name: String,
#     url: String,
#     created_at: String, # TIMESTAMP: YYYY-MM-DD hh:mm:ss ZZZZ
#   }]
get("/rooms") do
  DB[:rooms].reverse(:created_at).to_a.to_json
end

# Create a new Room - takes no parameters - and returns it
#
#   {
#     id: Int,
#     name: String,
#     url: String,
#     created_at: String, # TIMESTAMP: YYYY-MM-DD hh:mm:ss ZZZZ
#   ]
post("/rooms") do
  room = DAILY.create_room!.slice("url", "name")
  DB[:rooms].returning.insert(room).first.to_json
end

# Returns a Room object and its Stats ordered by time recorded
#
# Path param:
#   room_id: Int
#
#   {
#     room: {
#       id: Int,
#       name: String,
#       url: String,
#       created_at: String, # TIMESTAMP: YYYY-MM-DD hh:mm:ss ZZZZ
#     },
#     users: [{
#       id: String,
#       stats: [{
#         room_id: Int,
#         user_id: String,
#         video_recv_bits_per_second: Int,
#         video_recv_packet_loss: Int,
#         video_send_bits_per_second: Int,
#         video_send_packet_loss: Int,
#         recorded_at: String, # TIMESTAMP: YYYY-MM-DD hh:mm:ss ZZZZ
#       }]
#     }]
#   }
get("/rooms/:room_id") do
  {
    room: DB[:rooms][id: params[:room_id]],
    users: DB[:stats]
      .select(
        Sequel.lit("user_id AS id"),
        Sequel.lit("COALESCE(JSON_AGG(stats.* ORDER BY recorded_at), '[]') AS stats"))
      .where(room_id: params[:room_id])
      .group(:user_id)
      .to_a
  }.to_json
end

# Record a new Stat
#
# Path param:
#   room_id: Int
#
# Body param:
#   {
#     video_recv_bits_per_second: Int,
#     video_recv_packet_loss: Int,
#     video_send_bits_per_second: Int,
#     video_send_packet_loss: Int,
#     recorded_at: String, # TIMESTAMP: YYYY-MM-DD hh:mm:ss ZZZZ
#   ]
post("/rooms/:room_id/stat") do
  room_id = params.fetch(:room_id)
  stat = JSON.parse(request.body.read)
  DB[:stats].insert stat.merge(room_id: room_id)
  {}.to_json
end

run Sinatra::Application
