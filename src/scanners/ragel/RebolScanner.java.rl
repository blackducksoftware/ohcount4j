package net.ohloh.ohcount4j.scan;

public class RebolScanner extends BaseScanner{

	protected int nested_level = 0;

  	%%{
	    machine rebol;
	    include common "common.rl";
	    
	    action inc_nested_level { nested_level++; }
	 	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	  	action is_nested { nested_level > 0 }

	    rebol_line_comment = ';' @comment nonnewline* @comment;
	    
	    # For handeling nested nature of bracket strings { }
	    rebol_cb_string_begin = '{' @code @inc_nested_level;
	    rebol_cb_string_end = '}' @code when is_nested @dec_nested_level;
	    rebol_cb_string := |*
	   		newline;
	   		spaces;
	   		(any - newline) => code;
	    	rebol_cb_string_begin;
	    	rebol_cb_string_end;
	    *|;
	    
	  	rebol_line := |*
	  		rebol_cb_string_begin => { fcall rebol_cb_string; };
	  		string_literal_dq;
	    	rebol_line_comment;
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
