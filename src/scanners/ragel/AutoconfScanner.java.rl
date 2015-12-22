package net.ohloh.ohcount4j.scan;

public class AutoconfScanner extends BaseScanner {

  %%{
    machine autoconf;
    include common "common.rl";

    autoconf_line_comment = 'dnl' @comment nonnewline* @comment;
    
  	autoconf_line := |*
    	autoconf_line_comment;
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
