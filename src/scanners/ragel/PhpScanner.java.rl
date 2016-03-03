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

public class PhpScanner extends BaseScanner{

	%%{
	machine php;
	include common "common.rl";
	include c "c.rl";

	php_line_comment = c_line_comment | ('#' @comment nonnewline* @comment);

	php_heredoc_begin = '<<<' nonnewline+ >begin_define_match %end_define_match @code;
	php_heredoc_end = spaces* (nonnewline+ >begin_try_match) :>> ';' @{ fhold; } @code;
	php_heredoc := |*
		#php_heredoc_end => { p -=1; boolean match = match(); p+=1; if(match) fret; }; 
		php_heredoc_end => { if(match()) fret; }; 
		spaces;
		newline;
		(any - newline) => code;
	*|;

	php := |*
		c_block_comment_begin => { fcall c_block_comment; };
		php_heredoc_begin => { fcall php_heredoc; };
		php_line_comment;
		spaces;
		string_literal;
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
