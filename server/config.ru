require "net/http"
require "sinatra"

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
#
get("/")  { { hello: "world" }.to_json }

run Sinatra::Application
