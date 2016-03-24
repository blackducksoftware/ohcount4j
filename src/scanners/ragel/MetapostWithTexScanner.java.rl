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

public class MetapostWithTexScanner extends BaseScanner{

	%%{
		machine mptex;
		include common "common.rl";
		
		mptex_comment = '%' @{ fhold; } @comment nonnewline*;
		
		mptex_tex_comment = '%' @comment nonnewline* @comment;
		mptex_tex_entry = ('verbatimtex' | 'btex') @code;
  		mptex_tex_outry = 'etex' @code;
  		mptex_tex_line := |*
    		mptex_tex_outry @{ setLanguage(Language.METAPOST); p = ts; fret; };
    		mptex_tex_comment;
    		spaces;
    		newline;
    		(any - newline) => code;
  		*|;
	
		mptex_line := |*
    		mptex_tex_entry => { setLanguage(Language.TEX); fcall mptex_tex_line; };
    		spaces;
    		mptex_comment;
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
