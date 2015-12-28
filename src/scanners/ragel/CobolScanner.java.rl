package com.blackducksoftware.ohcount4j.scan;

public class CobolScanner extends BaseScanner{

  %%{
    machine cobol;
    include common "common.rl";

    cobol_comment = '*' @comment nonnewline* @comment;
    
  	cobol_line := |*
    	cobol_comment;
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
