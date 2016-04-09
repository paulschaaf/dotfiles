#!/usr/bin/env ruby

# //////////////////////////////////////////////////////////////
class File
	def File.extension(aFilename)
		/^([^.]*)/.match(aFilename.reverse)[1].reverse
	end
end
