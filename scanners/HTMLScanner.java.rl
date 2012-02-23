package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class HTMLScanner extends BaseScanner{

	%%{

	machine html;
	include common "common.rl";
	include css "css.rl";

	html_comment =  ('<!--' @comment ( text_with_newlines @comment )* :>> '-->') @comment;
	html_sq_str = ('\'' @code ( text_with_newlines @code )* :>> '\'') @code;
	html_dq_str = ('"' @code ( text_with_newlines @code )* :>> '"') @code;
	html_string = html_sq_str | html_dq_str @code;
	html_newline = newline %nl;

	html_css_begin = '<style' [^>]* '>' @code;
	html_css_end = '</style' [^>]* '>';
	html_css_line := |*
		html_css_end => { setLanguage(Language.LANG_HTML); p = ts; fret; };
		spaces;
		css_comment;
		css_string;
		css_newline;
		(any - newline) => code;
	*|; 

	html_line := |*
		html_css_begin => { setLanguage(Language.LANG_CSS); fcall html_css_line; };
		spaces;
		html_comment;
		html_string;
		html_newline;
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