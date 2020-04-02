# DAILY API
#
require "httparty"

API_URL = "https://api.daily.co/v1"

class Daily
  def initialize(token)
    @token = token
  end

  def create_room!
    { id: rand(264) } # TODO
  end
end
