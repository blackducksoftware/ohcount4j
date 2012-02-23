package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class CSSScanner extends BaseScanner{

	%%{

	machine css;

	include common "common.rl";
	include css "css.rl";

	css_line := |*
		spaces;
		css_comment;
		css_string;
		css_newline;
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
		return Language.LANG_CSS;
	}
}