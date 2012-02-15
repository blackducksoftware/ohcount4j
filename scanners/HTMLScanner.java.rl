package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class HTMLScanner extends BaseScanner{

  %%{
    machine html;
    include common "common.rl";
    
    text_with_newlines = (newline %got_newline) | nonnewline;
    
    html_comment =  ('<!--' ( text_with_newlines )* :>> '-->') >start_comment %end_comment;
  	html_sq_str = ('\'' ( text_with_newlines )* :>> '\'') >start_str %end_str;
  	html_dq_str = ('"' ( text_with_newlines )* :>> '"') >start_str %end_str;
  	html_string = html_sq_str | html_dq_str;
  	html_newline = newline %got_newline;

  	html_line := |*
    	spaces => got_spaces;
    	html_comment;
    	html_string;
    	html_newline;
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
  	return Language.LANG_HTML;
  }
  
  
  
}
