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

public class HamlScanner extends BaseScanner{

  int prior_indent_level = 0;
  int current_indent_level = 0;
  int bracket_level = 0;

  %%{
    machine haml;

    include common "common.rl";
    include c "c.rl";
    
    action haml_indent_level_inc { current_indent_level++; }
	action haml_indent_level_res { current_indent_level = 0; }
    action haml_indent_level_set { prior_indent_level = current_indent_level; }
    action bracket_inc { bracket_level++; }
    action bracket_dec { bracket_level--; }
    action bracket_level_res { bracket_level = 0; }
    
    haml_indent = ([ ]{2}) @haml_indent_level_inc;
    haml_indent_init = ([ ]{2} >haml_indent_level_res @haml_indent_level_inc)? haml_indent*;
    haml_eol = newline >haml_indent_level_res;
    haml_special_char = [\.%#];
    haml_ruby_evaluator = "==" | ([&!]? "=") | "-"  | "~";
    haml_comment_delimiter = ("-#" | "/");
    
    haml_ruby_line_comment = '#' @comment (nonnewline* @comment);

	haml_ruby_block_comment_begin = '=begin' @code;
	haml_ruby_block_comment_end = '=end' @code;
	haml_ruby_block_comment := |*
		haml_ruby_block_comment_end => { fret; };
		spaces;
		newline;
		nonnewline => comment;
	*|;
    
    haml_xhtml_tag_modifier =
    ('{' >bracket_level_res @code (
      newline
      |
      spaces
      |
      (nonnewline - spaces - [{}]) @code
      |
      '{' @bracket_inc
      |
      '}' @bracket_dec
    )* :>> ('}' when { bracket_level == 0 }) @code)
    |
    ('[' >bracket_level_res @code (
      newline
      |
      spaces
      |
      '[' @bracket_inc
      |
      ']' @bracket_dec
      |
      (nonnewline - spaces - '[' - ']') @code
    )* :>> (']' when { bracket_level == 0 }) @code);
    
    haml_xhtml_tag = "%" ((nonnewline-spaces-'['-'{')+ - haml_ruby_evaluator) haml_xhtml_tag_modifier? '//'?;

    haml_block_line_transition =
      haml_eol
      ( newline )*
      ( [ ]{2} when {current_indent_level < prior_indent_level} @haml_indent_level_inc )*
      ( [ ]{2} when {current_indent_level >= prior_indent_level} @haml_indent_level_inc)+;
    
	haml_block_comment =
    haml_indent_init haml_comment_delimiter >haml_indent_level_set @comment (
      haml_block_line_transition
      |
      spaces
      |
      (nonnewline-spaces) @comment
    )*;

  	haml_comment = haml_block_comment;

  	haml_string =
    haml_indent*
    (nonnewline
      - haml_comment_delimiter
      - haml_ruby_evaluator
      - "."
      - "#"
      - "%"
      - [ ]) @code
    (nonnewline @code | ("|" @code newline))*;

    haml_ruby_entry =
      ([ ]{2})*
      haml_xhtml_tag{,1}
      haml_ruby_evaluator spaces @code;

    haml_ruby_outry = 
      newline
      any @{fhold;};

    haml_ruby_line := |*
      haml_ruby_outry @{ setLanguage(Language.HAML); p = ts; fret; };
      haml_ruby_block_comment_begin => { fcall haml_ruby_block_comment; };
	  haml_ruby_line_comment;
      spaces;
	  newline;
	  (any - newline) => code;
    *|;

    haml_line := |*
      haml_ruby_entry => { setLanguage(Language.RUBY);  fcall haml_ruby_line; };
      spaces;
      haml_comment;
      haml_string;
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
