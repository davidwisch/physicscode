module Jekyll
	class ImageIncludeTag < Liquid::Tag
		def initialize(tag_name, src, tokens)
			super
			@src = src.strip!
		end

		def render(context)
			"<img src=\"/images/#{@src}.png\" />"
		end
	end
end

Liquid::Template.register_tag('image', Jekyll::ImageIncludeTag)
