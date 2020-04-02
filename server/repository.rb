# HASH-BACKED, IN-MEMORY DATA STORE
#
#   We only need key-value operations, but a good rule of thumb for API changes:
#   Is it implementable with the Sequel gem? <http://sequel.jeremyevans.net/>
#   The only thing we really implement is getting, setting, and iterating,
#   all of which just delegate to the underlying hash.
#
class Repository
  include Enumerable
  extend Forwardable
  def_delegators :@data, :each, :[], :[]=

  def initialize(name)
    @name = name
    @data = {}
  end

  # This is generally a useful interface and guarantee,
  # but not necessary for an in-memory implementation.
  def transaction
    yield
  end
end
