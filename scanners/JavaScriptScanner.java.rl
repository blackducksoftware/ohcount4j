package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class JavaScriptScanner extends BaseScanner{

  %%{
    machine js;
    include common "common.rl";
		include c "c.rl";
    
  	js_line := |*
			c_block_comment_begin => { fcall c_block_comment; };
    	c_line_comment;
    	spaces;
    	newline;
    	string_literal;
    	(any - newline) => code;
  	*|; 
  }%%

	%% write data;

	@Override
	public void doScan(){
		%% write init;
		init();
		%% write exec;
	}

	@Override
	public Language getLanguage(){
		return Language.LANG_JAVASCRIPT;
	}
}
