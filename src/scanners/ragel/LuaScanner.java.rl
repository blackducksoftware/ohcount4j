package com.blackducksoftware.ohcount4j.scan;

public class LuaScanner extends BaseScanner{

  %%{
    machine lua;
    include common "common.rl";
    	
    lua_line_comment = '--' @comment (nonnewline? | ([^\[] | [\[] [^\[]) nonnewline*) @comment;
    
    lua_block_comment_begin = '--[[' @comment;
    lua_block_comment_end = ']]' @comment;
    
    lua_block_comment := |*
	    lua_block_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
    *|;
 
    
  	lua_line := |*
		lua_block_comment_begin => { fcall lua_block_comment; };
    	lua_line_comment;
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
