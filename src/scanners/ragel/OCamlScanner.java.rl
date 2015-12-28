package com.blackducksoftware.ohcount4j.scan;

import com.blackducksoftware.ohcount4j.Language;

public class OCamlScanner extends BaseScanner{

	protected int nested_level = 0;

  	%%{
	    machine ocaml;
	    include common "common.rl";
	    
	    action inc_nested_level { nested_level++; }
	 	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	  	action is_nested { nested_level > 0 }
	    
	    # For handeling nested nature of comments
	    ocaml_cb_string_begin = '(*' @comment @inc_nested_level;
	    ocaml_cb_string_end = '*)' @comment when is_nested @dec_nested_level;
	    ocaml_cb_string := |*
	   		newline;
	   		spaces;
	   		(any - newline) => comment;
	    	ocaml_cb_string_begin;
	    	ocaml_cb_string_end;
	    *|;
	    
	  	ocaml_line := |*
	  		ocaml_cb_string_begin => { fhold; fcall ocaml_cb_string; };
	  		string_literal_dq;
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
