# DAILY API
#
require "httparty"

API_URL = "https://api.daily.co/v1"

class Daily
  def initialize(token)
    @token = token
  end

  def create_room!
    { url: "https://hkgumbs.daily.co/hello" } # TODO
  end
end
