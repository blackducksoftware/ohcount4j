package com.blackducksoftware.ohcount4j.scan;

public class PascalScanner extends BaseScanner{

  %%{
    machine pascal;
    include common "common.rl";
    include c "c.rl";
 
 	pascal_bracket_comment_begin = '{' @comment;
 	pascal_bracket_comment_end = '}' @comment;
 	
 	pascal_bracket_comment := |*
		pascal_bracket_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;
 	
 	pascal_starparen_comment_begin = '(*' @comment;
 	pascal_starparen_comment_end = '*)' @comment;

	pascal_starparen_comment := |*
		pascal_starparen_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;

  	pascal_line := |*
		pascal_bracket_comment_begin => { fcall pascal_bracket_comment; };
		pascal_starparen_comment_begin => { fcall pascal_starparen_comment; };
		c_line_comment;
	   	spaces;
		string_literal_sq; # no dq strings in standard pascal
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
