package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class JavaScriptScanner extends BaseScanner{

	%%{

	machine js;

	include common "common.rl";
	include js "js.rl";

	js_line := |*
		spaces;
		js_comment;
		js_string;
		js_newline;
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