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

public class PascalScanner extends BaseScanner{

  %%{
    machine pascal;
    include common "common.rl";
    include c "c.rl";
 
 	pascal_bracket_comment_begin = '{' @comment;
 	pascal_bracket_comment_end = '}' @comment;
 	
 	pascal_bracket_comment := |*
		pascal_bracket_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;
 	
 	pascal_starparen_comment_begin = '(*' @comment;
 	pascal_starparen_comment_end = '*)' @comment;

	pascal_starparen_comment := |*
		pascal_starparen_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;

  	pascal_line := |*
		pascal_bracket_comment_begin => { fcall pascal_bracket_comment; };
		pascal_starparen_comment_begin => { fcall pascal_starparen_comment; };
		c_line_comment;
	   	spaces;
		string_literal_sq; # no dq strings in standard pascal
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
