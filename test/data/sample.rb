#!/usr/bin/env ruby
# A basic comment

=begin
This is a multi-line block comment with

a blank line in the middle.
=end

"A double-quoted string"
"A double-quoted string containing a ' single quote"
"A double-quoted string containing an escaped \" double quote"
%Q{A curly-brace double-quoted string}
%Q{A curly-brace double-quoted string containing a ' single quote}
%Q|A pipe-delimited double-quoted string|

"A double-quoted string should not end with an escaped double-quote\" # so this is not a comment"

'A single-quoted string'
'A single-quoted string containing a " double quote'
'A single-quoted string containing an escaped \' single quote'
%q{A curly-brace single-quoted string}
%q{A curly-brace single-quoted string containing a " double quote}

# An array of strings follows
%w(foo bar baz)

  <<HERE_DOC
A multi-line "here" doc
# This is not a comment!
HERE_DOC

  <<-HERE_DOC
A multi-line "here" doc with indented terminator
# This is not a comment!
	HERE_DOC

/regular expression containing a ' single quote/
/regular expression containing a " double quote/
/regular expression containing an \/ escaped slash/
%r(a parenthesis-delimited regular expression containing a ' single quote)
%r{a brace-delimited regular expression containing a " double quote}

puts "foo" # Code and comment on same line

def foo
	"foo"
end
