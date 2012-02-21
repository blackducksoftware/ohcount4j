package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class CSSScanner extends BaseScanner{

  %%{
    machine css;
    include common "common.rl";
    
    text_with_newlines = (newline %nl) | nonnewline;
    
  	css_comment = '/*' @comment ( text_with_newlines @comment )* :>> '*/' @comment;
  	css_sq_str = '\'' @code ( text_with_newlines @code )* :>> '\'' @code;
  	css_dq_str = '"' @code ( text_with_newlines @code )* :>> '"' @code;
  	css_string = css_sq_str | css_dq_str;
  	css_newline = newline %nl;

  	html_line := |*
    	spaces;
    	css_comment;
    	css_string;
    	css_newline;
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
  	return Language.LANG_CSS;
  }
  
  
  
}
