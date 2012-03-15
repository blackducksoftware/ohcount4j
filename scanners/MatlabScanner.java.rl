package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class MatlabScanner extends BaseScanner{

  %%{
    machine matlab;
    include common "common.rl";
    
    matlab_continue_comment = '...' @comment nonnewline* @comment;
    
    matlab_line_comment = '%' @comment ( nonnewline? @comment 
    		| ( (nonnewline - [{]) nonnewline* @comment) );
    
    matlab_block_comment_begin = '%{' @comment;
    matlab_block_comment_end = '%}' @comment;
    
    matlab_block_comment := |*
    	matlab_block_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
    *|;
    
  	matlab_line := |*
		matlab_block_comment_begin => { fcall matlab_block_comment; };
		matlab_line_comment;
		matlab_continue_comment;
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
  
  @Override
  public Language getLanguage(){
  	return Language.MATLAB;
  }
  
  
  
}
