package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class HTMLScanner extends BaseScanner{

  %%{
    machine html;
    include common "common.rl";
    
    text_with_newlines = (newline %nl) | nonnewline;
    
    html_comment =  ('<!--' @comment ( text_with_newlines @comment )* :>> '-->') @comment;
  	html_sq_str = ('\'' @code ( text_with_newlines )* :>> '\'') @code;
  	html_dq_str = ('"' @code ( text_with_newlines @code )* :>> '"') @code;
  	html_string = html_sq_str | html_dq_str;
  	html_newline = newline %nl;

  	html_line := |*
    	spaces;
    	html_comment;
    	html_string;
    	html_newline;
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
  
  @Override
  public Language getLanguage(){
  	return Language.LANG_HTML;
  }
  
  
  
}
