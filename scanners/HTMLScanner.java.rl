package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class HTMLScanner extends BaseScanner{

	%%{

	machine html;
	include common "common.rl";
	include c "c.rl";

	html_css_begin = '<style' [^>]* '>' @code;
	html_css_end = '</style' [^>]* '>';
	html_css_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		html_css_end => { setLanguage(Language.LANG_HTML); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|; 

	html_js_begin = '<script' [^>]* '>' @code;
	html_js_end = '</script' [^>]* '>';
	html_js_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
		c_line_comment;
		html_js_end => { setLanguage(Language.LANG_HTML); p = ts; fret; };
		spaces;
		string_literal;
		newline;
		(any - newline) => code;
	*|; 

	html_comment_begin = '<!--' @comment;
	html_comment_end = '-->' @comment;
	html_comment := |*
		html_comment_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
	*|;

	html_line := |*
		html_css_begin => { setLanguage(Language.LANG_CSS); fcall html_css_line; };
		html_js_begin => { setLanguage(Language.LANG_JAVASCRIPT); fcall html_js_line; };
		html_comment_begin => { fcall html_comment; };
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

	@Override
	public Language getLanguage() {
		return Language.LANG_HTML;
	}
}
