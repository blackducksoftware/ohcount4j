package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class CSSScanner extends BaseScanner{

  %%{
    machine css;
    include common "common.rl";
    
    text_with_newlines = (newline %got_newline) | nonnewline;
    
  	css_comment = ('/*' ( text_with_newlines )* :>> '*/') >start_comment %end_comment;
  	css_sq_str = ('\'' ( text_with_newlines )* :>> '\'') >start_str %end_str;
  	css_dq_str = ('"' ( text_with_newlines )* :>> '"') >start_str %end_str;
  	css_string = css_sq_str | css_dq_str;
  	css_newline = newline %got_newline;

  	html_line := |*
    	spaces => got_spaces;
    	css_comment;
    	css_string;
    	css_newline;
    	(any - newline) => got_code_character;
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
  
  @Override
  public Language getLanguage(){
  	return Language.LANG_CSS;
  }
  
  
  
}
