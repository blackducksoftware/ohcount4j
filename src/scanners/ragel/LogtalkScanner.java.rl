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

public class LogtalkScanner extends BaseScanner {

  %%{
    machine logtalk;
    include common "common.rl";

    logtalk_line_comment = '%' @comment nonnewline* @comment;
    
    logtalk_block_comment_begin = '/*' @comment;
	logtalk_block_comment_end = '*/' @comment;
	
	logtalk_block_comment := |*
		logtalk_block_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;
	
  	logtalk_line := |*
		logtalk_block_comment_begin => { fcall logtalk_block_comment; };
    	logtalk_line_comment;
    	spaces;
    	newline;
    	string_literal;
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
