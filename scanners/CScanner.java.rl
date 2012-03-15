package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class CScanner extends BaseScanner{

  %%{
    machine c;
    include common "common.rl";
		include c "c.rl";
    
  	c_line := |*
			c_block_comment_begin => { fcall c_block_comment; };
    	c_line_comment;
    	spaces;
    	newline;
    	string_literal;
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
  	return Language.C;
  }
}
