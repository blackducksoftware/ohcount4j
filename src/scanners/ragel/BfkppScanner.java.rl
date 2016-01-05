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

public class BfkppScanner extends BaseScanner{

  	%%{
  		machine bfpp;
		include common "common.rl";
		
  		bfpp_operator = [+\-<>.,\[\]\%\!\#\^\:\;] @code;

  		bfpp_line_comment = ('=') @comment nonnewline*;

  		bfpp_include = '@include(' @code (
      		newline
      		|
      		spaces
      		|
      		(any - newline) @code
   		)* :>> ')';

  		bfpp_line := |*
    		spaces;
    		newline;
    		bfpp_line_comment;
    		bfpp_include;
    		bfpp_operator;
    		^space => comment;
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
