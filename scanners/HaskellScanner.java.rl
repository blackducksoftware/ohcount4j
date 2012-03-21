package net.ohloh.ohcount4j.scan;

public class HaskellScanner extends BaseScanner{

	protected int nested_level = 0;

  	%%{
	    machine haskell;
	    include common "common.rl";
	    
	    action inc_nested_level { nested_level++; }
	 	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	  	action is_nested { nested_level > 0 }

		# TODO verify |--
		
		haskell_line_comment = ([^|]? '--' ([^>] @comment nonnewline* | [^>])?) @comment;

	    # For handeling nested nature block comments { }
	    haskell_block_comment_begin = '{-' @comment @inc_nested_level;
	    haskell_block_comment_end = '-}' @comment when is_nested @dec_nested_level;
	    haskell_block_comment := |*
	   		newline;
	   		spaces;
	   		(any - newline) => comment;
	    	haskell_block_comment_begin;
	    	haskell_block_comment_end;
	    *|;
	    
	    haskell_string_literal = string_literal_dq | ('\'' @code string_char '\'' @code);
	    
	  	haskell_line := |*
	  		haskell_block_comment_begin => { fhold; fcall haskell_block_comment; };
	  		haskell_string_literal;
	    	haskell_line_comment;
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
