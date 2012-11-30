package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class RexxScanner extends BaseScanner{

	protected int nested_level = 0;

  	%%{
	    machine rexx;
	    include common "common.rl";
	    
	    action inc_nested_level { nested_level++; }
	 	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	  	action is_nested { nested_level > 0 }
	    
	    # For handeling nested nature of comments
	    rexx_cb_string_begin = '/*' @comment @inc_nested_level;
	    rexx_cb_string_end = '*/' @comment when is_nested @dec_nested_level;
	    rexx_cb_string := |*
	   		newline;
	   		spaces;
	   		(any - newline) => comment;
	    	rexx_cb_string_begin;
	    	rexx_cb_string_end;
	    *|;
	    
	  	rexx_line := |*
	  		rexx_cb_string_begin => { fhold; fcall rexx_cb_string; };
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
