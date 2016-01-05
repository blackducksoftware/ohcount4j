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

public class BlitzMaxScanner extends BaseScanner{

	%%{
		machine blitzmax;
		include common "common.rl";
		
		blitzmax_line_comment = '\'' @comment nonnewline*;
  		blitzmax_rem_block_comment = /rem/i @comment (
      		newline
      		|
      		spaces
      		|
      		(any - newline) @comment
    	)* :>> (/end rem/i | /endrem/i);

  		blitzmax_comment = blitzmax_line_comment | blitzmax_rem_block_comment;
		
		blitzmax_line := |*
    		spaces;
    		newline;
    		blitzmax_comment;
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
