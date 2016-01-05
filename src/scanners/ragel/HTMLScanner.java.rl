/**
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

import com.blackducksoftware.ohcount4j.Language;

public class HTMLScanner extends BaseScanner{

	%%{

	machine html;
	include common "common.rl";
	include c "c.rl";

	html_css_begin = '<style' [^>]* '>' @code;
	html_css_end = '</style' [^>]* '>';
	html_css_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		html_css_end => { setLanguage(Language.HTML); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|; 

	html_js_begin = '<script' [^>]* '>' @code;
	html_js_end = '</script' [^>]* '>';
	html_js_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		html_js_end => { setLanguage(Language.HTML); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|; 

	html_comment_begin = '<!--' @comment;
	html_comment_end = '-->' @comment;
	html_comment := |*
		html_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;

	html_line := |*
		html_css_begin => { setLanguage(Language.CSS); fcall html_css_line; };
		html_js_begin => { setLanguage(Language.JAVASCRIPT); fcall html_js_line; };
		html_comment_begin => { fcall html_comment; };
		spaces;
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
