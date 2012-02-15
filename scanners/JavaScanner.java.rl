package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class JavaScanner extends BaseScanner{

  %%{
    machine java;
    include common "common.rl";
    
    text_with_newlines = (newline %got_newline) | nonnewline;
    
  	java_line_comment = (('//' [^\n]* ) >start_comment %end_comment) (newline %got_newline);
  	java_block_comment = ('/*' ( text_with_newlines )* :>> '*/') >start_comment %end_comment;
  	java_comment = java_line_comment | java_block_comment;

  	java_sq_str = ('\'' ( text_with_newlines )* :>> '\'') >start_str %end_str;
  	java_dq_str = ('"' ( text_with_newlines )* :>> '"') >start_str %end_str;
  	java_string = java_sq_str | java_dq_str;
  	java_newline = newline %got_newline;

  	java_line := |*
    	spaces => got_spaces;
    	java_comment;
    	java_string;
    	java_newline;
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
  	return Language.LANG_JAVA;
  }
  
  
  
}