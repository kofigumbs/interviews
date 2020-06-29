# DAILY API

require "httparty"

class Daily
  API_URL = "https://api.daily.co/v1"

  # Create an API client from an API key <https://dashboard.daily.co/>
  def initialize(key)
    @key = key
  end

  # See <https://docs.daily.co/reference#create-room>
  def create_room!
    post! "/rooms"
  end

  private

  def post!(path)
    HTTParty.post(API_URL + path, headers: {
      "Content-Type" => "application/json",
      "Authorization" => "Bearer " + @key,
    })
  end
end
