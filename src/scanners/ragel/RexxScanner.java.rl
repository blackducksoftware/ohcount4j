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

public class RexxScanner extends BaseScanner{

	protected int nested_level = 0;

  	%%{
	    machine rexx;
	    include common "common.rl";
	    
	    action inc_nested_level { nested_level++; }
	 	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	  	action is_nested { nested_level > 0 }
	    
	    # For handeling nested nature of comments
	    rexx_cb_string_begin = '/*' @comment @inc_nested_level;
	    rexx_cb_string_end = '*/' @comment when is_nested @dec_nested_level;
	    rexx_cb_string := |*
	   		newline;
	   		spaces;
	   		(any - newline) => comment;
	    	rexx_cb_string_begin;
	    	rexx_cb_string_end;
	    *|;
	    
	  	rexx_line := |*
	  		rexx_cb_string_begin => { fhold; fcall rexx_cb_string; };
	  		string_literal;
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
