package net.ohloh.ohcount4j.scan;

public class AutomakeScanner extends BaseScanner {

  %%{
    machine automake;
    include common "common.rl";

    automake_line_comment = '#' @comment nonnewline* @comment;
    
  	automake_line := |*
    	automake_line_comment;
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

}
