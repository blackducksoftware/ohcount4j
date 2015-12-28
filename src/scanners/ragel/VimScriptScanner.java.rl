package com.blackducksoftware.ohcount4j.scan;

public class VimScriptScanner extends BaseScanner{

  %%{
    machine vim;
    include common "common.rl";
    
    action has_code {
    	codeSeen.size() > 0
    }
    
    vim_comment = '"' when !has_code @comment nonnewline* @comment;
    
  	vim_line := |*
    	vim_comment;
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
