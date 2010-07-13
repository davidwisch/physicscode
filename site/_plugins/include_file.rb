module Jekyll
	class FileIncludeTag < Liquid::Tag
		def initialize(tag_name, file, tokens)
			super
			@file = file.strip
		end

		def render(context)
			contents = ""
			path = File.join("..", "src", @file)
			if File.exists?(path)
				File.open(path, "r") { |dat|
					contents = dat.read
				}
			else
				contents = "Error reading #{@file}"
			end

			return contents
		end
	end
end

Liquid::Template.register_tag('file', Jekyll::FileIncludeTag)
