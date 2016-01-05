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

public class PythonScanner extends BaseScanner{

  %%{
    machine python;
    include common "common.rl";
    include c "c.rl";
    
    python_line_comment = c_line_comment | ('#' @comment nonnewline* @comment);
    
    python_sq_doc_str_begin = '\'\'\'' @comment;
    python_sq_doc_str_end = '\'\'\'' @comment;
    python_sq_doc_str := |*
    	python_sq_doc_str_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
    *|;
    
    python_dq_doc_str_begin = '"""' @comment;
    python_dq_doc_str_end = '"""' @comment;
    python_dq_doc_str := |*
    	python_dq_doc_str_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
    *|;
    
    python_string_literal_sq = '\'' ([^'] | '\'' [^'] @{ fhold; }) @{ fhold; } nonnewline* '\'' @code;
    python_string_literal_dq = '"' ([^"] | '"' [^"] @{ fhold; }) @{ fhold; } nonnewline* '"' @code;
    python_string_literal = python_string_literal_sq | python_string_literal_dq;
    
  	python_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
    	python_sq_doc_str_begin => { fcall python_sq_doc_str; };
    	python_dq_doc_str_begin => { fcall python_dq_doc_str; };
    	python_line_comment;
    	spaces;
    	python_string_literal;
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
