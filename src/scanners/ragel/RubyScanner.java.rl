/*
 * Copyright 2016 Black Duck Software, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j.scan;

public class RubyScanner extends BaseScanner{

	%%{
	machine ruby;
	include common "common.rl";

	ruby_line_comment = '#' @comment (nonnewline* @comment);

	ruby_block_comment_begin = '=begin' @code;
	ruby_block_comment_end = '=end' @code;
	ruby_block_comment := |*
		ruby_block_comment_end => { fret; };
		spaces;
		newline;
		nonnewline => comment;
	*|;

	ruby_regex = '/' (string_char - '/')* :>> '/';

	# Ruby allows nearly any character to be used as a delimiter when using % notation.
	# Parens, braces, etc are balanced left and right.
	# Other delimiters such as pipes are simply repeated at the beginning and end.
	# TODO: Scanning arbitrary delimiters requires back-referencing regular expressions.
	ruby_pct_paren = '%' [qQrswWx] '(' (string_char - ')')* :>> ')';
	ruby_pct_brace = '%' [qQrswWx] '{' (string_char - '}')* :>> '}';
	ruby_pct_square = '%' [qQrswWx] '[' (string_char - ']')* :>> ']';
	ruby_pct_bracket = '%' [qQrswWx] '<' (string_char - '>')* :>> '>';
	ruby_pct_str = ruby_pct_paren | ruby_pct_brace | ruby_pct_square | ruby_pct_bracket;

	ruby_here_doc_label = nonnewline+ >begin_define_match %end_define_match;
	ruby_here_doc_body = nonnewline+ >begin_try_match ${ if(match()) { fhold; fgoto ruby; } };
	ruby_here_doc = '<<' '-'? ruby_here_doc_label newline (spaces* ruby_here_doc_body newline)*;

	ruby_string = (string_literal | ruby_regex | ruby_pct_str | ruby_here_doc) @code;

	ruby := |*
		ruby_block_comment_begin => { fcall ruby_block_comment; };
		ruby_line_comment;
		spaces;
		ruby_string;
		newline;
		(any - newline) => code;
	*|; 

	}%%

	%% write data;

	@Override
	public void doScan(){
		// variables and data is set up in BaseScanner
		%% write init;
		init();
		%% write exec;
	}

}
