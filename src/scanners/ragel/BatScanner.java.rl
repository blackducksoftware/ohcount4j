package net.ohloh.ohcount4j.scan;

public class BatScanner extends BaseScanner {

  %%{
    machine bat;
    include common "common.rl";

    bat_line_comment = ( /rem/i | /@rem/i | '::' ) @comment nonnewline*;

  	bat_line := |*
    	bat_line_comment;
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
