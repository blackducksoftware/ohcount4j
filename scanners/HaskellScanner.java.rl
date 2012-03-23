package net.ohloh.ohcount4j.scan;

public class HaskellScanner extends BaseScanner{

	protected int nested_level = 0;

  	%%{
	    machine haskell;
	    include common "common.rl";
	    
	    action inc_nested_level { nested_level++; }
	 	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	  	action is_nested { nested_level > 0 }

		# Catches instances of '|--' as not a comment properly
		# If it finds '--' as the first thing in the file, cannot have leading '|'
		# Otherwise every time a comment is found, checks the char before it for '|'
		haskell_line_comment = ('--' 
				when { p == 0 || data[p-1] != '|' } 
						([^>] @comment nonnewline* | [^>])?) @comment;


	    # For handeling nested nature block comments {- -}
	    haskell_block_comment_begin = '{-' @comment @inc_nested_level;
	    haskell_block_comment_end = '-}' @comment when is_nested @dec_nested_level;
	    haskell_block_comment := |*
	   		newline;
	   		spaces;
	   		(any - newline) => comment;
	    	haskell_block_comment_begin;
	    	haskell_block_comment_end;
	    *|;
	    
	    haskell_char = '\'' @code string_char '\'' @code;
	    haskell_string_literal = string_literal_dq | haskell_char;
	    
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
