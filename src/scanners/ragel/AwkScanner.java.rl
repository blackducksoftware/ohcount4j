package net.ohloh.ohcount4j.scan;

public class AwkScanner extends BaseScanner {

  %%{
    machine awk;
    include common "common.rl";

    awk_line_comment = '#' @comment nonnewline* @comment;
    
  	awk_line := |*
    	awk_line_comment;
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
