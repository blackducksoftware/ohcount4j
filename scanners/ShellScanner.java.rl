package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class ShellScanner extends BaseScanner {

  %%{
    machine shell;
    include common "common.rl";

    shell_line_comment = '#' @comment nonnewline* @comment;
    
  	shell_line := |*
    	shell_line_comment;
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
  	return Language.SHELL;
  }
  
}
