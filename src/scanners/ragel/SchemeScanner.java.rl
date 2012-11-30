package net.ohloh.ohcount4j.scan;

public class SchemeScanner extends BaseScanner{

  %%{
    machine scheme;
    include common "common.rl";
    
    scheme_line_comment = ';' @comment nonnewline* @comment;

	scheme_block_comment_begin = '#|' @comment;
	scheme_block_comment_end = '|#' @comment;
	
	scheme_block_comment := |*
		scheme_block_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;
    
  	scheme_line := |*
		scheme_block_comment_begin => { fcall scheme_block_comment; };
    	scheme_line_comment;
    	spaces;
    	newline;
    	string_literal;
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
