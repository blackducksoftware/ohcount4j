package com.blackducksoftware.ohcount4j.scan;

public class PhpScanner extends BaseScanner{

	%%{
	machine php;
	include common "common.rl";
	include c "c.rl";

	php_line_comment = c_line_comment | ('#' @comment nonnewline* @comment);

	php_heredoc_begin = '<<<' nonnewline+ >begin_define_match %end_define_match @code;
	php_heredoc_end = spaces* (nonnewline+ >begin_try_match) :>> ';' @{ fhold; } @code;
	php_heredoc := |*
		#php_heredoc_end => { p -=1; boolean match = match(); p+=1; if(match) fret; }; 
		php_heredoc_end => { if(match()) fret; }; 
		spaces;
		newline;
		(any - newline) => code;
	*|;

	php := |*
		c_block_comment_begin => { fcall c_block_comment; };
		php_heredoc_begin => { fcall php_heredoc; };
		php_line_comment;
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
