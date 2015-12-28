package com.blackducksoftware.ohcount4j.scan;

public class SqlScanner extends BaseScanner{

  %%{
    machine sql;
    include common "common.rl";
    include c "c.rl";
    
    sql_line_comment = "--" @comment nonnewline* @comment | "#" @comment nonnewline* @comment;
    
    sql_block_comment_begin = '{' @comment;
	sql_block_comment_end = '}' @comment;

	sql_block_comment := |*
		sql_block_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;
    
  	sql_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		sql_block_comment_begin => { fcall sql_block_comment; };
    	sql_line_comment;
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
