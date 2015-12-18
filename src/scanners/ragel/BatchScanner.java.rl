package net.ohloh.ohcount4j.scan;

public class BatchScanner extends BaseScanner {

  %%{
    machine batch;
    include common "common.rl";

    batch_line_comment = ( /rem/i | /@rem/i | '::' ) @comment nonnewline*;

  	batch_line := |*
    	batch_line_comment;
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
