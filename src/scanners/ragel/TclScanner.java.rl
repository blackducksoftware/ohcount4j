package com.blackducksoftware.ohcount4j.scan;

public class TclScanner extends BaseScanner{

  %%{
    machine tcl;
    include common "common.rl";
    
    tcl_comment = '#' @comment nonnewline* @comment;
    
  	tcl_line := |*
    	tcl_comment;
    	spaces;
    	string_literal_dq; # no single quote strings in tcl
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
