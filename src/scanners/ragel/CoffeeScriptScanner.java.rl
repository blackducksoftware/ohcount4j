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

import com.blackducksoftware.ohcount4j.Language;

public class CoffeeScriptScanner extends BaseScanner {

	%%{

	machine coffeescript;
	include common "common.rl";
	include c "c.rl";

	coffeescript_js_begin = '`' @code;
	coffeescript_js_end = '`';
	coffeescript_js_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		coffeescript_js_end => { setLanguage(Language.COFFEESCRIPT); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|; 

	coffeescript_line_comment = '#' @comment nonnewline* @comment;
	coffeescript_comment_begin = '###' @comment;
	coffeescript_comment_end = '###' @comment;
	coffeescript_comment := |*
		coffeescript_comment_end => { fret; };
		coffeescript_line_comment;
		spaces;
		newline;
		(any - newline) => comment;
	*|;

	coffeescript_line := |*
		coffeescript_js_begin => { setLanguage(Language.JAVASCRIPT); fcall coffeescript_js_line; };
		coffeescript_comment_begin => { fcall coffeescript_comment; };
		coffeescript_line_comment;
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