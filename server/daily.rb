# DAILY API
#
require "httparty"

API_URL = "https://api.daily.co/v1"

class Daily
  def initialize(token)
    @token = token
  end

  def create_room!
    # TODO
    {
      "name" => "call-#{rand(256)}",
      "url" => "https://hkgumbs.daily.co/hello",
    }
  end
end
