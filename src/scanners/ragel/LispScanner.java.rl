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

public class LispScanner extends BaseScanner{

  %%{
    machine lisp;
    include common "common.rl";
    
    lisp_line_comment = ';' @comment nonnewline* @comment;
    
    lisp_block_comment_begin = '#|' @comment;
	lisp_block_comment_end = '|#' @comment;
	
	lisp_block_comment := |*
		lisp_block_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;
	
	# A messy solution but lisp_defun is entered to catch the possibility
	# of a docstring (seen as a comment in original ohcount). In lisp, a
	# docstring is the first string after a defun declaration (does not
	# always exist). Docstrings can be multi-line so a scanner machine is
	# used. Docstrings on same line as defun declaration are seen as code
	# because comment is overridden by code.
	
	lisp_docstring_begin = '"' @comment;
	lisp_docstring_end = '"' @comment;
	lisp_docstring := |*
		lisp_docstring_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;
	
	lisp_defun_begin = '(' /defun /i nonnewline* @code newline;
	lisp_defun := |*
		lisp_docstring_begin => { fcall lisp_docstring; };
		spaces;
		newline => { fret; };
	*|;
    
    lisp_string_begin = '"' @code;
    lisp_string_end = '"' @code;
    
    lisp_string := |*
    	lisp_string_end => { fret; };
    	spaces;
    	newline;
    	(any - newline) => code;
    *|;
    
  	lisp_line := |*
		lisp_block_comment_begin => { fcall lisp_block_comment; };
    	lisp_line_comment;
    	lisp_defun_begin => { fcall lisp_defun; };
    	spaces;
    	lisp_string_begin => { fcall lisp_string; };
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
