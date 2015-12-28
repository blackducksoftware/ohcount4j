package com.blackducksoftware.ohcount4j.scan;

public class AdaScanner extends BaseScanner{

  %%{
    machine ada;
    include common "common.rl";

    ada_comment = '--' @comment nonnewline* @comment;
    
  	ada_line := |*
		ada_comment;
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

}
