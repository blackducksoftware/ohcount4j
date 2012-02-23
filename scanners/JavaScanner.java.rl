package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class JavaScanner extends BaseScanner{

  %%{
    machine java;
    include common "common.rl";
    
  	java_line_comment = '//' @comment (nonnewline* @comment);
  	java_block_comment = ('/*' @comment ( text_with_newlines @comment)* :>> '*/') @comment;
  	java_comment = java_line_comment | java_block_comment;

  	java_sq_str = ('\'' @code ( text_with_newlines @code )* :>> '\'') @code;
  	java_dq_str = ('"' @code ( text_with_newlines @code )* :>> '"') @code;
  	java_string = java_sq_str | java_dq_str;
  	java_newline = newline %nl;

  	java_line := |*
    	spaces;
    	java_comment;
    	java_string;
    	java_newline;
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
  	return Language.LANG_JAVA;
  }
  
  
  
}
