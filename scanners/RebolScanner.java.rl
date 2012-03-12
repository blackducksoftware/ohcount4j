package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class RebolScanner extends BaseScanner{

	protected int str_nested_level = 0;

  	%%{
	    machine rebol;
	    include common "common.rl";
	    
	    action inc_str_nested_level { str_nested_level++; }
	 	action dec_str_nested_level { str_nested_level--; if(str_nested_level == 0) fret; }
	  	action is_nested { str_nested_level > 0 }

	    rebol_line_comment = ';' @comment nonnewline* @comment;
	    
	    # Anything between curly brackets { } is considered a string
	    # Brackets may be nested so string does not end until matching bracket is found
	    # If not enough terminating brackets, rest of file is considered part of the string
	    rebol_cb_string_begin = '{' @code;
	    rebol_cb_string_end = '}' @code;
	    rebol_cb_string := |*
	   		newline;
	   		spaces;
	   		(any - newline) => code;
	    	rebol_cb_string_begin @inc_str_nested_level;
	    	rebol_cb_string_end when is_nested @dec_str_nested_level;
	    *|;
	    
	  	rebol_line := |*
	  		rebol_cb_string_begin => { fhold; fcall rebol_cb_string; };
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
	  
	@Override
	public Language getLanguage() {
		return Language.LANG_REBOL;
	}
  
}
