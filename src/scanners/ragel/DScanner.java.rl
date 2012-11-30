package net.ohloh.ohcount4j.scan;

public class DScanner extends BaseScanner{

	protected int nested_level = 0;

  	%%{
	    machine d;
	    include common "common.rl";
	    include c "c.rl";
	    
	    action inc_nested_level { nested_level++; }
	 	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	  	action is_nested { nested_level > 0 }

	    # For handeling nested nature block comments {- -}
	    d_block_comment_begin = '/+' @comment @inc_nested_level;
	    d_block_comment_end = '+/' @comment when is_nested @dec_nested_level;
	    d_block_comment := |*
	   		newline;
	   		spaces;
	   		(any - newline) => comment;
	    	d_block_comment_begin;
	    	d_block_comment_end;
	    *|;
	    
	    d_bt_string = '`' @code string_char* '`' @code;
	    d_string_literal = string_literal | d_bt_string;
	    
	  	d_line := |*
	  		c_block_comment_begin => { fcall c_block_comment; };
	  		d_block_comment_begin => { fcall d_block_comment; };
	  		c_line_comment;
	  		d_string_literal;
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
