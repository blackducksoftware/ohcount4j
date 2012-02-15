package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class CScanner extends BaseScanner{

  %%{
    machine c;
    include common "common.rl";
    
    text_with_newlines = (newline %got_newline) | nonnewline;
    
  	c_line_comment = (('//' [^\n]* ) >start_comment %end_comment) (newline %got_newline);
  	c_block_comment = ('/*' ( text_with_newlines )* :>> '*/') >start_comment %end_comment;
  	c_comment = c_line_comment | c_block_comment;

  	c_sq_str = ('\'' ( text_with_newlines )* :>> '\'') >start_str %end_str;
  	c_dq_str = ('"' ( text_with_newlines )* :>> '"') >start_str %end_str;
  	c_string = c_sq_str | c_dq_str;
  	c_newline = newline %got_newline;
  	# c_code = (any* - [spaces| c_string | c_comment | newline]) >start_code %end_code (newline %got_newline);

  	c_line := |*
    	spaces => got_spaces;
    	c_comment;
    	c_string;
    	c_newline;
    	(any - newline) => got_code_character;
    	#c_code;
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
