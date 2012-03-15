package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class CSSScanner extends BaseScanner{

	%%{

	machine css;

	include common "common.rl";
	include c "c.rl";

	css_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
   	c_line_comment;
		spaces;
		newline;
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
		return Language.CSS;
	}
}
