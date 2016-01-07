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

import com.blackducksoftware.ohcount4j.Language;

public class ClearSilverTemplateScanner extends BaseScanner{

  %%{
    machine clearsilver_template;

    include common "common.rl";
    include c "c.rl";
    
    cst_css_begin = '<style' [^>]* '>' @code;
	cst_css_end = '</style' [^>]* '>';
	cst_css_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		cst_css_end => { setLanguage(Language.HTML); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|;
	
	cst_js_begin = '<script' [^>]* '>' @code;
	cst_js_end = '</script' [^>]* '>';
	cst_js_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		cst_js_end => { setLanguage(Language.HTML); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|;
	
	cst_cs_comment = '#' @comment nonnewline*;
	cst_cs_string = '"' @code ([^\r\n\f"\\] | '\\' nonnewline)* '"';
  	cst_cs_begin = '<?cs' @code;
  	cst_cs_end = '?>' @code;
  	cst_cs_line := |*
  		cst_cs_end @{ setLanguage(Language.HTML); p = ts; fret; };
  	    cst_cs_comment;
  	    cst_cs_string;
    	spaces;
		newline;
		(any - newline) => code;
   *|;
   
   	cst_html_comment := (
    	newline
    	|
    	spaces
    	|
    	^(space | [\-<]) @comment
    	|
    	'<' '?cs' @{ setLanguage(Language.CLEARSILVER); fcall cst_cs_line; }
    	|
    	'<' !'?cs'
  	)* :>> '-->' @comment @{ fgoto clearsilver_template_line; };

  	cst_html_sq_str := (
    	newline
    	|
    	spaces
    	|
    	[^\r\n\f\t '\\<] @code
    	|
    	'\\' nonnewline @code
    	|
    	'<' '?cs' @{ setLanguage(Language.CLEARSILVER); fcall cst_cs_line; }
    	|
    	'<' !'?cs'
  	)* '\'' @{ fgoto clearsilver_template_line; };
  	
  	cst_html_dq_str := (
    	newline
    	|
    	spaces
    	|
    	[^\r\n\f\t "\\<] @code
    	|
    	'\\' nonnewline @code
    	|
    	'<' '?cs' @{ setLanguage(Language.CLEARSILVER); fcall cst_cs_line; }
    	|
    	'<' !'?cs'
  	)* '"' @{ fgoto clearsilver_template_line; };

	clearsilver_template_line := |*
		cst_css_begin    => { setLanguage(Language.CSS); fcall cst_css_line; };
		cst_js_begin     => { setLanguage(Language.JAVASCRIPT); fcall cst_js_line; };
		cst_cs_begin        => { setLanguage(Language.CLEARSILVER); fcall cst_cs_line; };
		
    	# standard HTML patterns
    	spaces;
    	'<!--'       => { fgoto cst_html_comment; };
    	'\''         => { fgoto cst_html_sq_str;  };
    	'"'          => { fgoto cst_html_dq_str;  };
    	newline;
    	(any - newline) => code;
	*|; 
    

  }%%

  %% write data;
  
  @Override
  public void doScan(){
  	setDefaultLanguage(Language.HTML);
  	setLanguage(Language.HTML);
  	
	// variables and data is set up in BaseScanner
    %% write init;
    init();
    %% write exec;
  }
}
