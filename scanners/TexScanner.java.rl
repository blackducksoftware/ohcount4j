package net.ohloh.ohcount4j.scan;

public class TexScanner extends BaseScanner{

  %%{
    machine tex;
    include common "common.rl";
    
    tex_comment = '%' @comment nonnewline* @comment;
    
    tex_string_begin = '``' @code;
    tex_string_end = '\'\'' @code;
    tex_string := |*
    	tex_string_end => { fret; };
		spaces;
		newline;
		(any - newline) => code;
    *|;
    
  	tex_line := |*
    	tex_comment;
    	tex_string_begin => { fcall tex_string; };
    	spaces;
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
