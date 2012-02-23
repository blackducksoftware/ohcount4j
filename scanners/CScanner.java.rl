package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class CScanner extends BaseScanner{

  %%{
    machine c;
    include common "common.rl";
    
    text_with_newlines = (newline %nl) | nonnewline;
    
  	c_line_comment = '//' @comment (nonnewline @comment)*;
  	c_block_comment = '/*' @comment (text_with_newlines @comment)* :>> '*/' @comment;
  	c_comment = c_line_comment | c_block_comment;

  	c_sq_str = '\'' @code (text_with_newlines @code)* :>> '\'' @code;
  	c_dq_str = '"' @code (text_with_newlines @code)* :>> '"' @code;
  	c_string = c_sq_str | c_dq_str;
  	c_newline = newline %nl;

  	c_line := |*
    	spaces;
    	c_comment;
    	c_string;
    	c_newline;
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
  	return Language.LANG_C;
  }
  
  
  
}
