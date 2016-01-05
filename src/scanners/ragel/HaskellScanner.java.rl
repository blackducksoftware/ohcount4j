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

public class HaskellScanner extends BaseScanner{

	protected int nested_level = 0;

  	%%{
	    machine haskell;
	    include common "common.rl";
	    
	    action inc_nested_level { nested_level++; }
	 	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	  	action is_nested { nested_level > 0 }

		# Catches instances of '|--' as not a comment properly
		# If it finds '--' as the first thing in the file, cannot have leading '|'
		# Otherwise every time a comment is found, checks the char before it for '|'
		haskell_line_comment = ('--' 
				when { p == 0 || data[p-1] != '|' } 
						([^>] @comment nonnewline* | [^>])?) @comment;


	    # For handeling nested nature block comments {- -}
	    haskell_block_comment_begin = '{-' @comment @inc_nested_level;
	    haskell_block_comment_end = '-}' @comment when is_nested @dec_nested_level;
	    haskell_block_comment := |*
	   		newline;
	   		spaces;
	   		(any - newline) => comment;
	    	haskell_block_comment_begin;
	    	haskell_block_comment_end;
	    *|;
	    
	    haskell_char = '\'' @code string_char '\'' @code;
	    haskell_string_literal = string_literal_dq | haskell_char;
	    
	  	haskell_line := |*
	  		haskell_block_comment_begin => { fhold; fcall haskell_block_comment; };
	  		haskell_string_literal;
	    	haskell_line_comment;
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
