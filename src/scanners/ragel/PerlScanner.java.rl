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

public class PerlScanner extends BaseScanner{

  %%{
    machine perl;
    include common "common.rl";
 
 	perl_line_comment = '#' @comment nonnewline* @comment;
 
 	action starts_line {
 		p == 0 || data[p-1] == '\n' || data[p-1] == '\r' || data[p-1] == '\f'
 	}
 
 	perl_block_comment_begin = '=' when starts_line nonnewline+ @comment;
 	perl_block_comment_end = '=' when starts_line /cut/i @comment;
 	
 	perl_block_comment := |*
		perl_block_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;
 	
  	perl_line := |*
		perl_block_comment_begin => { fcall perl_block_comment; };
		perl_line_comment;
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
