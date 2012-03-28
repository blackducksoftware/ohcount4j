package net.ohloh.ohcount4j.scan;

public class MathematicaScanner extends BaseScanner{

	protected int nested_level = 0;

  %%{
    machine mathematica;
    include common "common.rl";
    
    action inc_nested_level { nested_level++; }
	action dec_nested_level { nested_level--; if(nested_level == 0) fret; }
	action is_nested { nested_level > 0 }
    
    mathematica_block_comment_begin = '(*' @comment @inc_nested_level;
    mathematica_block_comment_end = '*)' @comment when is_nested @dec_nested_level;
    
    mathematica_block_comment := |*
		spaces;
		newline;
		(any - newline) => comment;
		mathematica_block_comment_begin;
    	mathematica_block_comment_end;
    *|;
    
  	mathematica_line := |*
		mathematica_block_comment_begin => { fcall mathematica_block_comment; };
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
