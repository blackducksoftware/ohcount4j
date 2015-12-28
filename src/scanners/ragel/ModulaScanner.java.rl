package com.blackducksoftware.ohcount4j.scan;

public class ModulaScanner extends BaseScanner{

	protected int nested_level = 0;

  %%{
    machine modula;
    include common "common.rl";
    
    action inc_nested_level { nested_level++; }
	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	action is_nested { nested_level > 0 }
    

    # For handeling nested nature of comments (* *)
	modula_comment_begin = '(*' @comment @inc_nested_level;
	modula_comment_end = '*)' @comment when is_nested @dec_nested_level;
	modula_comment := |*
		newline;
		spaces;
		(any - newline) => comment;
	 	modula_comment_begin;
	  	modula_comment_end;
	*|;
    
  	modula_line := |*
		modula_comment_begin => { fcall modula_comment; };
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
