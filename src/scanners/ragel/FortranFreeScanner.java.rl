package com.blackducksoftware.ohcount4j.scan;

public class FortranFreeScanner extends BaseScanner {

  	%%{
	    machine fortran_free;
	    include common "common.rl";

		fortran_free_comment = '!' @comment nonnewline* @comment;

	  	fortran_free_line := |*
	  		fortran_free_comment;
	  		string_literal;
	    	spaces;
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
