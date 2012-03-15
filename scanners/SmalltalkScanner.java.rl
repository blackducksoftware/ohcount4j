package net.ohloh.ohcount4j.scan;

public class SmalltalkScanner extends BaseScanner{

  %%{
    machine st;
    include common "common.rl";
    
    st_comment_begin = '"' @comment;
    st_comment_end = '"' @comment;
    st_comment := |*
    	st_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
    *|;
    
  	st_line := |*
		st_comment_begin => { fcall st_comment; };
    	spaces;
    	string_literal_sq;
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
