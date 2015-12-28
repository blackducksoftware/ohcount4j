package com.blackducksoftware.ohcount4j.scan;

public class VisualBasicScanner extends BaseScanner{

  %%{
    machine vb;
    include common "common.rl";
    
    vb_line_comment = ('\'' | /rem/i) @comment nonnewline* @comment;
    
  	vb_line := |*
    	vb_line_comment;
    	spaces;
    	string_literal_dq; # Strings cannot be single quoted
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
