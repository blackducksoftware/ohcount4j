package com.blackducksoftware.ohcount4j.scan;

public class ErlangScanner extends BaseScanner{

  %%{
    machine erlang;
    include common "common.rl";
    include c "c.rl";
    
    erlang_line_comment = '%' @comment nonnewline* @comment;
    
  	erlang_line := |*
    	erlang_line_comment;
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
