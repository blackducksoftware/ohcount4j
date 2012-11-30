package net.ohloh.ohcount4j.scan;

public class FortranFixedScanner extends BaseScanner {

  	%%{
	    machine fortran_fixed;
	    include common "common.rl";
	    
	    action starts_line {
 			p == 0 || data[p-1] == '\n' || data[p-1] == '\r' || data[p-1] == '\f'
 		}
	    
		fortran_fixed_comment = 'C' when starts_line @comment nonnewline* @comment;

	  	fortran_fixed_line := |*
			fortran_fixed_comment;
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
