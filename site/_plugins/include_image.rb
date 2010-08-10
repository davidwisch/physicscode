module Jekyll
	class ImageIncludeTag < Liquid::Tag
		def initialize(tag_name, src, tokens)
			super
			@src = src.strip
		end

		def render(context)
			"<div class=\"center\"><img src=\"/images/#{@src}.png\" /></div>"
		end
	end
end

Liquid::Template.register_tag('image', Jekyll::ImageIncludeTag)
