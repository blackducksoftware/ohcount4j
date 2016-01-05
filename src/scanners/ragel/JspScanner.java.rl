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

public class JspScanner extends BaseScanner{

	%%{

	machine jsp;
	include common "common.rl";
	include c "c.rl";

	jsp_java_begin = ('<%'| '<%!' | '<%=') @code;
	jsp_java_end = '%>';
	jsp_java_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		jsp_java_end => { setLanguage(Language.JSP); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|; 

	jsp_css_begin = '<style' [^>]* '>' @code;
	jsp_css_end = '</style' [^>]* '>';
	jsp_css_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		jsp_css_end => { setLanguage(Language.JSP); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|; 

	jsp_js_begin = '<script' [^>]* '>' @code;
	jsp_js_end = '</script' [^>]* '>';
	jsp_js_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		jsp_js_end => { setLanguage(Language.JSP); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|; 

	jsp_comment_begin = '<!--' @comment;
	jsp_comment_end = '-->' @comment;
	jsp_comment := |*
		jsp_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;

	jsp_line := |*
		jsp_java_begin => { setLanguage(Language.JAVA); fcall jsp_java_line; };
		jsp_css_begin => { setLanguage(Language.CSS); fcall jsp_css_line; };
		jsp_js_begin => { setLanguage(Language.JAVASCRIPT); fcall jsp_js_line; };
		jsp_comment_begin => { fcall jsp_comment; };
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
