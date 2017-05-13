###
# This module provides a way for keyword parameters (to #initialize or similar)
# to be processed.  It is a mix-in: the target class must define a class method
# "defaults" which returns a hash, mapping keyword parameters to the default
# value.
#
# If any keyword parameters are given that do not have a default value, they 
# are rejected with an error.
#
# If a default value is marked MANDATORY (a constant in this module), then its
# value must be given by the user, or an error is raised.
#
# by Gavin Sinclair, taken from RubyGarden.org wiki
#

module KeywordProcessor

  MANDATORY = :MANDATORY

  def process_params(hash)
    defaults = self.class.defaults()

    # Guard user against invalid keys.
    hash.keys.each do |key|
      unless defaults.has_key? key
        raise ArgumentError, "Key not supported: #{key}"
      end
    end

    result = defaults.update(hash)

    # Ensure mandatory fields are given.
    unfilled = result.select { |k,v| v == MANDATORY }.map { |k,v| k.inspect }
    unless unfilled.empty?
      msg = "Following mandatory fields not given: #{unfilled.join(', ')}"
      raise ArgumentError, msg
    end

    result
  end

end


# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

###
# An example class to demonstrate the KeywordProcessor module.
#
class Recorder

  include KeywordProcessor

  def Recorder.defaults
    {
      :DATABASE => "...",
      :LOG_FILE => MANDATORY,
      :PASSWORD => "...",
      :USERNAME => "...",
    }
  end

  def initialize(init_params={})
    init = process_params(init_params)    # <-- Key line
    @database = init[:DATABASE]
    @log_file = init[:LOG_FILE]
    @password = init[:PASSWORD]
    @username = init[:USERNAME]
  end

end

end
