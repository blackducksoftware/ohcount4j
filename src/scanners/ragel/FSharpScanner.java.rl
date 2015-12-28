package com.blackducksoftware.ohcount4j.scan;

public class FSharpScanner extends BaseScanner{

  %%{
    machine fs;
    include common "common.rl";
    include c "c.rl";
    
    fs_block_comment_begin = '(*' @comment;
    fs_block_comment_end = '*)' @comment;
    fs_block_comment := |*
    	fs_block_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
    *|;
    
  	fs_line := |*
		fs_block_comment_begin => { fcall fs_block_comment; };
    	c_line_comment;
    	spaces;
    	string_literal;
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
