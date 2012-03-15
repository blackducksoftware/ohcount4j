package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class ActionScriptScanner extends BaseScanner{

  %%{
    machine as;
    include common "common.rl";
    include c "c.rl";
    
    as_cdata_begin = '<![CDATA[' @code;
	as_cdata_end = ']]>' @code;
	as_cdata := |*
		as_cdata_end => { fret; };
		spaces;
		newline;
		(any - newline) => code;
	*|;
    
  	as_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
    	c_line_comment;
    	as_cdata_begin => { fcall as_cdata; };
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
  	return Language.ACTIONSCRIPT;
  }
  
  
  
}
