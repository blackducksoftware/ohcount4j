package com.blackducksoftware.ohcount4j.scan;

public class CoqScanner extends BaseScanner {

	protected int nested_level = 0;

  	%%{
	    machine coq;
	    include common "common.rl";
	    
	    action inc_nested_level { nested_level++; }
	 	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	  	action is_nested { nested_level > 0 }

	    # For handling nested nature block comments (* *)
	    coq_block_comment_begin = '(*' @comment @inc_nested_level;
	    coq_block_comment_end = '*)' @comment when is_nested @dec_nested_level;

	    coq_block_comment := |*
	   		newline;
	   		spaces;
	   		(any - newline) => comment;
	    	coq_block_comment_begin;
	    	coq_block_comment_end;
	    *|;

	  	coq_line := |*
	  		coq_block_comment_begin => { fcall coq_block_comment; };
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