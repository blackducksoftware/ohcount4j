package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class AssemblyScanner extends BaseScanner{

  %%{
    machine asm;
    include common "common.rl";
    include c "c.rl";
    
    asm_line_comment_starts = '//' | '#' | '*' | ';' | '!';
    asm_line_comment = asm_line_comment_starts @comment nonnewline* @comment;
    
  	asm_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
    	asm_line_comment;
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
  	return Language.LANG_ASM;
  }
  
}
