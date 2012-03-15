package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class EiffelScanner extends BaseScanner{

  %%{
    machine eiffel;
    include common "common.rl";
    
    eiffel_line_comment = '--' @comment nonnewline* @comment;
    
  	eiffel_line := |*
    	eiffel_line_comment;
    	spaces;
    	string_literal_dq;
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
  	return Language.EIFFEL;
  }
  
  
  
}
