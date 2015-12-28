package com.blackducksoftware.ohcount4j.scan;

public class CMakeScanner extends BaseScanner {

  %%{
    machine CMake;
    include common "common.rl";

    cmake_line_comment = '#' @comment nonnewline* @comment;
    
  	cmake_line := |*
    	cmake_line_comment;
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
