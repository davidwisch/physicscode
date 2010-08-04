module Jekyll
	class CommitHashTag < Liquid::Tag
		def initialize(tag_name, arg, tokens)
			super
		end

		def render(context)
			commit = `git rev-parse HEAD`
			return commit.strip
		end
	end
end

Liquid::Template.register_tag('commit_hash', Jekyll::CommitHashTag)
