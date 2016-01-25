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

public class ForthScanner extends BaseScanner {

  %%{
    machine forth;
    include common "common.rl";
    	
    forth_line_comment = '\\' @comment nonnewline* @comment;
    
    forth_block_comment_begin = '(' @comment;
    forth_block_comment_end = ')' @comment;
    
    forth_block_comment := |*
	    forth_block_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
    *|;
    
  	forth_line := |*
		forth_block_comment_begin => { fcall forth_block_comment; };
    	forth_line_comment;
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
