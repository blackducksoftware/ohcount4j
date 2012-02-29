package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class JavaScanner extends BaseScanner{

  %%{
    machine java;
    include common "common.rl";
    include c "c.rl";
    
  	java_line := |*
			c_block_comment_begin => { fcall c_block_comment; };
    	c_line_comment;
    	spaces;
    	string_literal;
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
  
  @Override
  public Language getLanguage(){
  	return Language.LANG_JAVA;
  }
  
  
  
}
