package com.blackducksoftware.ohcount4j.scan;

public class ColdFusionScanner extends BaseScanner{

  protected int nested_level = 0;

  %%{
    machine coldfusion;
    include common "common.rl";

    action inc_nested_level { nested_level++; }
	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	action is_nested { nested_level > 0 }
    
    coldfusion_comment_begin = '<!---' @comment @inc_nested_level;
    coldfusion_comment_end = '--->' @comment when is_nested @dec_nested_level;
    coldfusion_comment := |*
    	spaces;
		newline;
		(any - newline) => comment;
		coldfusion_comment_begin;
		coldfusion_comment_end;
    *|;
    
  	coldfusion_line := |*
		coldfusion_comment_begin => { fhold; fcall coldfusion_comment; };
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
